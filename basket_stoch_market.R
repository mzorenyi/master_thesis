library(readxl)
library(openxlsx)
library(quantmod)
library(keras)
library(reticulate)
library(tensorflow)

### Change version
version <- 5

use_virtualenv("~/tf_petya_venv/", required = TRUE)

main_path <- "/Users/martonzorenyi/Downloads/MTS_final_models/"
bloomberg_folder <- paste0(main_path, "bloombergdata/")
source(paste0(main_path, "scripts/iv_functions.R"))
source(paste0(main_path, "scripts/functions_stoch.R"))

## For Saving Results
type <- "basket"
training_string <- "stoch"
test_string <- "market"
output_folder <- paste0(main_path, "outputs/", type, "_option/", training_string, "_", test_string, "/")
results_file <- paste0(output_folder, type, "_", training_string, "_", test_string, "_results.csv")
print(results_file)
strike <- 150

S1_name <- "JNJ"
S2_name <- "PG"


duration <- "1m"
print(paste("Applying Model for Stocks", S1_name, "and", S2_name, "for timeframe", duration))
aod <- "2025-06-16"
end_date <- "7/18/25" #m/dd/yy format in Bloomberg data
end_date_qmod <- "2025-07-18"

# For plots and saving Results:
plot_string <- paste0(type ,"_", training_string,"_", test_string,"_", S1_name, "_", S2_name, "_", duration, "_v_", version, ".pdf")
target_dir <- "/Users/martonzorenyi/Downloads/Real_Market/"
# Read in T-Bill Data

get_risk_free_rate <- function(duration, aod){ # duration 1m or 3m; aod "2025-06-17"
  aod <- as.Date(aod)
  duration <- toupper(duration)
  quantmod_symbol <- paste0("DGS", duration, "O")
  ts <- getSymbols(quantmod_symbol, src = "FRED", auto.assign = F, from = aod, to = aod)
  exponent <- as.numeric(substr(duration, start = 1, stop = 1))
  (as.numeric(ts) / 100 + 1) ^ (exponent/12) - 1
}

read_in_function <- function(security, duration){
  security <- tolower(security)
  path <- paste0(bloomberg_folder, security, "_", duration, ".xlsx")
  
  ## Read in Option Data
  option_data <- read.xlsx(path, startRow = 3)
  
  # Function to modify excel sheet
  modify_df <- function(df){
    colnames(df) <- c("Strikes","Ticker1","Geld","Brief","Schl","IVM","Vol.","Strikes","Ticker","Geld","Brief","Schl","IVM","Vol.")
    df <- df[grepl(end_date, df$Ticker1),]
    df_calls <- df[,1:7]
    df_puts <- df[,8:ncol(df)]
    list(calls = df_calls, puts = df_puts)
  }
  output <- modify_df(option_data)
  # Get Stock Data from quantmod
  quantmod_symbol <- toupper(security)
  quantmod_data <- getSymbols(quantmod_symbol, auto.assign = F, from = as.Date("2020-06-17"), to = as.Date(end_date_qmod) + 1)
  colnames(quantmod_data) <- c("Open", "High", "Low", "Close", "Vol.", "Adj.")
  quantmod_data <- quantmod_data$Close
  output <- append(output, list(stockdata = quantmod_data))
  output
}

# duration as number:
dur <- as.numeric(substr(duration, start = 1, stop = 1))
period_string <- paste0(aod, "/", as.Date(end_date_qmod))

## Get data
# Risk-free Rate
risk_free <- get_risk_free_rate(duration = duration, aod = aod)
asset1 <- read_in_function(S1_name, duration = duration)
asset2 <- read_in_function(S2_name, duration = duration)

## Get correlations
joint <- merge(asset1$stockdata, asset2$stockdata, join = "left")
joint <- na.omit(joint)
whole_history <- joint
joint <- joint[paste0("/",aod)]
corr <- as.numeric(cor(joint$Close, joint$Close.1))

## For now we work with implied volatility calculated from Bloomberg with 3 stocks closest to ATM
S1_0 <- as.numeric(asset1$stockdata[aod])
S2_0 <- as.numeric(asset2$stockdata[aod])

## Approx. volatility with IV

vola1 <- iv_asset(asset1)
vola2 <- iv_asset(asset2)


# joint_xts is your 2-column xts object (Close and Close.1)
# Example: rolling 20-day correlation
# --- 1. Compute 30-day rolling correlation from joined xts ---
rolling_corr <- rollapply(
  joint,
  width = 60,
  FUN = function(z) cor(z[, 1], z[, 2]),
  by.column = FALSE,
  align = "right",
  fill = NA
)

# Remove NA padding
hallo <- na.omit(rolling_corr)

# Find differences
#plot(hallo, ylim = c(-1,1))

a <- density(hallo, from = -0.99, to = 0.99)
#plot(a)

# Your empirical data
x_data <- a$x
y_data <- a$y

# Your model function (replace with your actual one)
model <- function(x, par) {
  # Example: par[1] = A, par[2] = B
  KAPPA <- par[1]
  THETA <- par[2]
  ((1-x)/(1+x))^(-THETA * KAPPA) * (1+x^2)^(KAPPA - 1)  # <-- replace with your actual function
}

# Loss function: sum of squared differences
loss_fn <- function(par) {
  pred <- model(x_data, par)
  sum((pred - y_data)^2)
}

# Optimization (use reasonable starting values)
opt <- optim(par = c(10, 0.4), fn = loss_fn, method = "L-BFGS-B", lower = c(0.1,-0.99), upper = c(10, 0.99))

# Best-fit parameters
best_par <- opt$par
cat("Best parameters:", best_par, "\n")



KAPPA <- best_par[1]
THETA <- best_par[2]


#model_plot <- function(x) {
#  # Example: par[1] = A, par[2] = B
#  ((1-x)/(1+x))^(-THETA * KAPPA) * (1+x^2)^(KAPPA - 1)  # <-- replace with your actual function
#}
# For static model just take 5-year long run correlation
########### To be added - Take Theta instead  ########### 
rho <- as.numeric(rolling_corr[aod])





use_virtualenv("~/tf_petya_venv/", required = TRUE)


## Parameters
N <- length(asset1$stockdata[paste0(aod, "/")]) - 1 # time discretization
T <- 1 # maturity
R <- 3 # number of shown trajectories --- not used for training/testing
m <- 2 # number of assets

## Parameters for asset dynamics
S_1_0 <- S1_0
S_2_0 <- S2_0
mu1 <- risk_free
mu2 <- risk_free
sigma1 <- vola1
sigma2 <- vola2
#rho <- corr
## Parameters for stoch. correlation
#gamma <- 0.4
#GAMMA <- 0.5
#alpha <- 0.1
#rho_0 <- c()
## Train/Test setup
Ktrain <- 40000 # Size of training data
Ktest <- 1 # Size of test data
epochs <- 100
batch_size <- 256
activator <- "relu" # Activation function to use in the networks # maybe 'relu' can be tried
#print(Ktrain)
## Parameters for plotting
pdf_width <- 10
pdf_height <- 7

gamma <- KAPPA
GAMMA<- THETA

find_alpha <- function(gam, GAM){
  denom <- min(1+GAM, 1-GAM)
  sqrt(gam * denom) - 0.01
}

alpha <- find_alpha(gamma, GAMMA)
large_rhos <- c()
rho_0 <- THETA
reps <- 0
sanity_check <- function(gam, alp){
  a <- alp^2/(1 + c(-1,1) * gam)
  max(a)
}
#goal_reps <- length(gammas) * length(GAMMAS) * length(alphas)

if(sanity_check(GAMMA, alpha) > gamma){
  stop("Condition for gamma not met, passing on to next simulation.")
  reps <- reps + 1
  print(paste0("Simulation", reps, "out of", goal_reps, "done."))
  next
}
set.seed(seed = 261000)
print(paste0("Rho_0 =", rho))
starting_time <- Sys.time()

## Network structure
learnV0 <- TRUE # Learn setup wealth. If set to FALSE, then it uses the MC estimate as initial wealth
d <- 3 # number of hidden layers in strategy
n <- 200 # nodes for nodes in hidden layers

## Digits shown in some print commands:
prec <- 2

# Time discretization
TimePoints <- seq(0, T, length.out = N + 1)

# Time Conversion function
TimeConv <- function(t) {
  # return(sqrt(T - t))   ## Works better as input variable in case of diffusion models!
  return(T - t)  ## The NN expects a time to maturity as information
  # return(t)  ## The NN expects actual time as information. The results are the same as 'time to maturity'
  ## but the information will be stored differently in the NN.
}

###### Functions ######
# Function to generate Asset paths:

## Function for payoff of exchange option
f <- function(trainpathes) {
  pmax((trainpathes[1,N+1,] + trainpathes[2,N+1,])/2 - strike , 0) # max{S1-S2,0}
}

## Function to check hedging positions

get_nn_hedge_ratios <- function(tt, test_paths, model){
  index_S <- tail(which(TimePoints <= tt), 1)
  ttm <- rep(TimeConv(tt), times = Ktest)
  # Get S_t
  S <- apply(test_paths, MARGIN = 2, function(x) x)
  S1 <- S[1,]
  S2 <- S[2,]
  S1 <- S1[index_S] # selects S_t^1
  S2 <- S2[index_S] # selects S_t^2
  test_input <- cbind(ttm, S1, S2)
  hedge_ratios <- predict(hedge, test_input, verbose = 0)
  H1 <- hedge_ratios[,1]
  H2 <- hedge_ratios[,2]
  list(H1 = H1, H2 = H2)
}

## Function to compare NN hedging ratios to theoretical hedging ratios
compare_ratios <- function(asset, t, rho = rho){
  ttm <- T - t
  index <- tail(which(TimePoints <= t), 1)
  testus <- test_gens[,index,1:Ktest]
  testus <- rbind(testus, rho, ttm)
  nn_hedge <- get_nn_hedge_ratios(tt = t, test_paths = test_gens, model = hedge)
  if(asset == 1){
    nn_hedge <- nn_hedge$H1
    #theor <- pnorm(mapply(d1, testus[1,], testus[2,], testus[3,], testus[4,]))
  } else if(asset == 2){
    nn_hedge <- nn_hedge$H2
    #theor <- (-1) * pnorm(mapply(d2, testus[1,], testus[2,], testus[3,], testus[4,]))
  } else{
    stop("Asset not/wrongly specified")
  }
  list(NN = nn_hedge)
}

## Function to show evolution of hedging portfolio's value over time
hedging_pf_val_exchange <- function(path_num, showplot = T, rho = rho){
  ## Neural Network
  hp_nn_val <- rep(0, times = (N+1))
  hp_nn_val[1] <- V_0
  #hp_theor_val <- rep(0, times = (N+1))
  #hp_theor_val[1] <- V_0
  for (i in 2:(N+1)) {
    ## NN Hedging Portfolio
    gains_from_asset1_nn <- (prices1[[i]][path_num] - prices1[[i-1]][path_num]) * ratios_asset1[[i-1]]$NN[path_num]
    gains_from_asset2_nn <- (prices2[[i]][path_num] - prices2[[i-1]][path_num]) * ratios_asset2[[i-1]]$NN[path_num]
    hp_nn_val[i] <- hp_nn_val[i-1] + gains_from_asset1_nn + gains_from_asset2_nn
    ## Theor. Hedging Portfolio
    #gains_from_asset1_theor <- (prices1[[i]][path_num] - prices1[[i-1]][path_num]) * ratios_asset1[[i-1]]$THEORY[path_num]
    #gains_from_asset2_theor <- (prices2[[i]][path_num] - prices2[[i-1]][path_num]) * ratios_asset2[[i-1]]$THEORY[path_num]
    #hp_theor_val[i] <- hp_theor_val[i-1] + gains_from_asset1_theor + gains_from_asset2_theor
  }
  option_payoff <- max(prices1[[(N+1)]][path_num] - prices2[[(N+1)]][path_num], 0)
  if(showplot == T){
    par(mfrow = c(1,1))
    title <- paste("Evolution of Hedging Portfolio vs. Option Payoff, Sim Nr.", path_num)
    plot(TimePoints, hp_nn_val, type = "line", main = title, ylim = c(0, 50), ylab = "Value", xlab = "t", col = "blue")
    abline(option_payoff, 0, col = "purple")
    #par(new = T)
    #plot(TimePoints, hp_theor_val, type = "line", ylim = c(0,10), ylab = "", xlab = "", col = "black")
    legend("topleft", c("NN Hedging Portfolio", "Option Payoff"), col = c("blue", "purple"), lty = 1, cex = 0.5)
  }
  list(NN = hp_nn_val)
}


## Function to get hedging ratios over time; also plots evolution if showplot set to TRUE
hedge_over_time <- function(path_num, showplot = T, rho = rho){
  hedging_times <- TimePoints[-length(TimePoints)] # only times considered where we actually rebalance
  hedges_asset1 <- c()
  for (i in 1:length(hedging_times)) {
    print(hedging_times[i])
    nn_ratio <- compare_ratios(asset = 1, t = hedging_times[i], rho = rho)$NN[path_num]
    #theor_ratio <- compare_ratios(asset = 1, t = hedging_times[i], rho = rho)$THEORY[path_num]
    hedges_asset1 <- rbind(hedges_asset1, nn_ratio)
  }
  
  hedges_asset2 <- c()
  for (i in 1:length(hedging_times)) {
    print(hedging_times[i])
    nn_ratio <- compare_ratios(asset = 2, t = hedging_times[i], rho = rho)$NN[path_num]
    #theor_ratio <- compare_ratios(asset = 2, t = hedging_times[i], rho = rho)$THEORY[path_num]
    hedges_asset2 <- rbind(hedges_asset2, nn_ratio)
  }
  
  if(showplot == T){
    tit1 <- paste("Asset Paths for simulation Nr.", path_num, "with corr. = ", rho)
    tit2 <- "Hedging Ratios over Time"
    print("Plotting...")
    ## Plot Asset over time:
    par(mfrow = c(1,2))
    plot(test_gens[,,path_num][1,], type = "line", col = "blue", ylim = c(100, 200), ylab = "", main = tit1)
    par(new = T)
    plot(test_gens[,,path_num][2,], type = "line", col = "black", ylim = c(100, 200), ylab = "")
    legend("topright", c("Asset 1", "Asset 2"), col = c("blue", "black"), lty = 1)
    
    ## Plot hedging ratios:
    # Asset 1:
    
    plot(hedges_asset1[,1], type = "line",col = "blue", ylim = c(-1,1), ylab = "", main = tit2)
    par(new = T)
    plot(hedges_asset1[,2], type = "line", lty = 2, col = "blue", ylim = c(-1,1), ylab = "")
    # Asset 2:
    par(new = T)
    plot(hedges_asset2[,1], type = "line",col = "black", ylim = c(-1,1), ylab = "")
    par(new = T)
    plot(hedges_asset2[,2], type = "line", lty = 2, col = "black", ylim = c(-1,1), ylab = "")
    legend("topleft", c("NN Hedge Asset 1", "Theor. Hedge Asset 1", "NN Hedge Asset 2", "Theor. Hedge Asset 2"),
           col = c("blue", "blue", "black", "black"), lty = c(1,2,1,2), cex = 0.5)
  }
  list(Asset1 = hedges_asset1, Asset2 = hedges_asset2)
}
print(paste("Chunk started at", Sys.time()))

###### Neural Networks Architecture ######
### -------------------- 1. Neural Network for Initial Wealth --------------------
d_V0 <- 0  # number of hidden layers for initial wealth network
price0 <- layer_input(shape = c(m))

V0 <- price0
if (d_V0 > 0) {
  for (i in 1:d_V0) {
    V0 <- V0 %>% layer_dense(units = 32, activation = activator)
  }
}
V0 <- V0 %>% layer_dense(units = 1, activation = 'linear', trainable = learnV0)

pi <- keras_model(inputs = price0, outputs = V0)

if (learnV0) {
  cat("\n[Network for Initial Wealth]:\n")
  summary(pi)
}

### -------------------- 2. Hedge Neural Network --------------------
timeprice <- layer_input(shape = c(1 + m))  # [TTM, S1, S2]

output <- timeprice
for (i in 1:d) {
  output <- output %>% layer_dense(units = n, activation = activator)
}
output <- output %>% layer_dense(units = m, activation = 'linear')

hedge <- keras_model(inputs = timeprice, outputs = output)
cat("\n[Network for Hedging Positions]:\n")
summary(hedge)

### -------------------- 3. Terminal Wealth Model --------------------
Obs <- layer_input(shape = c(N, 1 + m))     # input: [TTM, S1, S2] for all timesteps
Incr <- layer_input(shape = c(N, m))        # input: price increments

# Compute V0 from initial prices (S1_0, S2_0)
V0_input <- layer_lambda(f = function(x) x[,1,2:3])(Obs)# shape: (batch_size, 2)
V0 <- pi(V0_input)

# TimeDistributed application of hedge NN
H <- Obs %>% time_distributed(hedge)  # shape: (batch_size, N, m)

# Flatten for dot product with increments
H_flat <- H %>% layer_flatten()
Incr_flat <- Incr %>% layer_flatten()

Gain <- layer_dot(axes = 1)(list(H_flat, Incr_flat))  # scalar gain
wealth <- layer_add()(list(V0, Gain))  # terminal wealth

model_wealth <- keras_model(inputs = list(Obs, Incr), outputs = wealth)
model_wealth %>% compile(optimizer = 'adam', loss = 'mean_squared_error')

cat("\n[Network for Terminal Wealth]:\n")
summary(model_wealth)



###### Model Training ######
training_paths <- path_gen_stoch(Ktrain) # generates training paths
xtrain <- shape_inputs_stoch(arr = training_paths) # reshapes TP's
ytrain <- f(training_paths)

# Actual training

#print(paste0("Training for rho = ", rho))

model_wealth %>% fit(
  x = xtrain,
  y = ytrain,
  epochs = epochs,
  batch_size = batch_size,
  verbose = 1
)

#af         
## Test Period:
test_asset1 <- asset1$stockdata[period_string]
test_asset2 <- asset2$stockdata[period_string]
test_paths <- rbind(as.numeric(test_asset1), as.numeric(test_asset2))
#test_paths <- cbind(test_paths, c(173, 161))

## Get NN Hedging Ratios
ratios1 <- rep(0, times = N)
ratios2 <- rep(0, times = N)
for (i in 1:N) {
  ratios <- get_nn_hedge_ratios(TimePoints[i], test_paths = test_paths)
  ratios1[i] <- ratios$H1
  ratios2[i] <- ratios$H2
}

## Gains from Trade and V0
V0 <- as.numeric(predict(pi, cbind(S1_0, S2_0)))
gains_from_trade <- rep(0, times = N)
for (i in 1:N) {
  gains_from_trade[i] <- (test_paths[1, i+1] - test_paths[1, i]) * ratios1[i] + (test_paths[2, i+1] - test_paths[2, i]) * ratios2[i] 
}
hp_end <- sum(gains_from_trade) + V0
final_payoff <- max((test_paths[1, N+1] + test_paths[2, N+1])/2 - strike, 0)

## Save Result
if(file.exists(results_file)){
  res <- read.csv(results_file)
  res <- res[,-1]
  res_new <- data.frame(Asset_1 = S1_name, Asset_2 = S2_name, Hedging_Error = final_payoff - hp_end, Ktrain = Ktrain, cor = rho, gamma = gamma, GAMMA = GAMMA, alpha = alpha)
  res <- rbind(res, res_new)
} else{
  res <- data.frame(Asset_1 = S1_name, Asset_2 = S2_name, Hedging_Error = final_payoff - hp_end, Ktrain = Ktrain, cor = rho, gamma = gamma, GAMMA = GAMMA, alpha = alpha)
}
write.csv(res, file = results_file)
## Some Plots:

# Calc correlation over test timeframe
test_corr <- rollapply(
  whole_history,
  width = 30,
  FUN = function(z) cor(z[, 1], z[, 2]),
  by.column = FALSE,
  align = "right",
  fill = NA
)
test_corr <- tail(test_corr, N)

## Plots
plot_path <- paste0(output_folder, plot_string)
pdf(plot_path)
## Stocks, Hedging Ratio, Correlation, Hedging Pf over Time
par(mfrow = c(2,2))
plot(TimePoints, test_paths[1,], type = "l", col = "black", main = "Evolution of Stock Prices", xlab = "t", ylab = "Value", ylim = c(100,200))
par(new = T)
plot(TimePoints, test_paths[2,], type = "l", col = "blue", main = "Evolution of Stock Prices", xlab = "t", ylab = "Value", ylim = c(100,200))
legend("topleft", c(S1_name, S2_name), col = c("black", "blue"), cex = 0.5, lty = 1)

## Hedging Ratios
plot(head(TimePoints, N), ratios1, type = "l", col = "black", main = "Evolution of Hedging Ratios", xlab = "t", ylab = "Hedging Ratio", ylim = c(-1,1))
par(new = T)
plot(head(TimePoints, N), ratios2, type = "l", col = "blue", main = "Evolution of Hedging Ratios", xlab = "t", ylab = "", ylim = c(-1,1))
legend("topleft", c(S1_name, S2_name), col = c("black", "blue"), cex = 0.5, lty = 1)

## Correlation

plot(head(TimePoints, N), test_corr[,1], ylim = c(-1,1), main = "30d Correlation over test period", type = "l", ylab = "Correlation", xlab = "t")

## Evolution of Hedging Portfolio Value
hedging_portfolio_value <- c(V0, gains_from_trade)
hedging_portfolio_value_ts <- cumsum(hedging_portfolio_value)
plot(TimePoints, hedging_portfolio_value_ts, col = "black", type = "l", main = "Evolution of Hedging Portfolio", xlab = "t", ylab = "Value", ylim= c(min(final_payoff, min(hedging_portfolio_value_ts)) - 5, max(final_payoff, max(hedging_portfolio_value_ts)) + 5))
abline(final_payoff, 0, col = "purple")
legend("topleft", c("Hedging PF value", "Final Payoff"), col = c("black", "purple"), cex = 0.5, lty = 1)

dev.off()

