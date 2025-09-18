#### basket Options Code clean ####
rm(list = ls())
library(keras)
library(reticulate)
library(tensorflow)

### Change version
version <- 10

use_virtualenv("~/tf_petya_venv/", required = TRUE)

main_path <- "/Users/martonzorenyi/Downloads/MTS_final_models/"
source(paste0(main_path, "scripts/functions_stoch.R"))

## For Saving Results
type <- "basket"
training_string <- "stoch"
test_string <- "stoch"
output_folder <- paste0(main_path, "outputs/", type, "_option/", training_string, "_", test_string, "/")
results_file <- paste0(output_folder, type, "_", training_string, "_", test_string, "_results_", version, ".csv")


## Parameters
N <- 20 # time discretization
T <- 2 # maturity
R <- 3 # number of shown trajectories --- not used for training/testing
m <- 2 # number of assets

## Parameters for asset dynamics
S_1_0 <- 150
S_2_0 <- 150
mu1 <- 0
mu2 <- 0
sigma1 <- 0.1
sigma2 <- 0.1
#rho <- 0.35
strike <- 150
## Parameters for stoch. correlation
gammas <- c(2.01,5)
GAMMAS <- c(-0.5, 0,0.25,0.5)
alphas <- c(1)
rho_0 <- c()

reps <- 0
goal_reps <- length(gammas) * length(GAMMAS) * length(alphas)

for(gamma in gammas){
  for(GAMMA in GAMMAS){
    for(alpha in alphas){

      if(sanity_check(GAMMA, alpha) > gamma){
        print("Condition for gamma not met, passing on to next simulation.")
        reps <- reps + 1
        print(paste0("Simulation", reps, "out of", goal_reps, "done."))
        next
      }
## Train/Test setup
Ktrain <- 40000 # Size of training data
Ktest <- 10000 # Size of test data
epochs <- 100
batch_size <- 256
activator <- "relu" # Activation function to use in the networks # maybe 'relu' can be tried
rho <- 0.25
n_mc <- 1000
seed <- 789134
## Parameters for plotting
pdf_width <- 10
pdf_height <- 7



  
  set.seed(seed = seed)
  
  {
    ## Network structure
    learnV0 <- TRUE # Learn setup wealth. If set to FALSE, then it uses the MC estimate as initial wealth
    d <- 3 # number of hidden layers in strategy
    n <- 200 # nodes for nodes in hidden layers
    
    ## Digits shown in some print commands:
    prec <- 2
    
    # Time discretization
    TimePoints <- seq(0, T, length.out = N + 1)
    

    
    ### -------------------- 0. Functions
    
    
    
    ## Function to check hedging positions
    
    get_nn_hedge_ratios <- function(tt, test_paths, model){
      index_S <- tail(which(TimePoints <= tt), 1)
      ttm <- rep(TimeConv(tt), times = Ktest)
      # Get S_t
      S <- apply(test_paths, MARGIN = 2, function(x) x)
      S1 <- S[seq(1, nrow(S), by = 3),]
      S2 <- S[seq(2, nrow(S), by = 3),]
      S1 <- S1[,index_S] # selects S_t^1
      S2 <- S2[,index_S] # selects S_t^2
      test_input <- cbind(ttm, S1, S2)
      hedge_ratios <- predict(hedge, test_input, verbose = 0)
      H1 <- hedge_ratios[,1]
      H2 <- hedge_ratios[,2]
      list(H1 = H1, H2 = H2)
    }
    
    ## Function for payoff of basket option
    f <- function(trainpathes) {
      pmax((trainpathes[1,N+1,] + trainpathes[2,N+1,])/2 - strike , 0) # max{mean(S1,S2),0}
    }
    
    ## Function to compare NN hedging ratios to theoretical hedging ratios
    compare_ratios <- function(asset, t, rho){
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
    
    ## Function to get hedging ratios over time; also plots evolution if showplot set to TRUE
    hedge_over_time <- function(path_num, showplot = T, rho){
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
        tit1 <- paste("Asset Paths for simulation Nr.", path_num)
        tit2 <- paste("Evolution of Hedging Ratios for simulation Nr.", path_num, "from test set")
        print("Plotting...")
        ## Plot Asset over time:
        par(mfrow = c(1,2))
        plot(test_gens[,,path_num][1,], type = "line", col = "blue", ylim = c(100, 200), ylab = "", main = tit1)
        par(new = T)
        plot(test_gens[,,path_num][2,], type = "line", col = "green", ylim = c(100, 200), ylab = "", main = tit2)
        legend("topright", c("Asset 1", "Asset 2"), col = c("blue", "green"), lty = 1)
        
        ## Plot hedging ratios:
        # Asset 1:
        
        plot(hedges_asset1[,1], type = "line",col = "blue", ylim = c(-1,1), ylab = "", main = "Evolution of Hedging Ratios")
        #par(new = T)
        #plot(hedges_asset1[,2], type = "line", col = "red", ylim = c(-1,1), ylab = "")
        # Asset 2:
        par(new = T)
        plot(hedges_asset2[,1], type = "line", lty = 2,col = "blue", ylim = c(-1,1), ylab = "")
        #par(new = T)
        #plot(hedges_asset2[,2], type = "line", lty = 2, col = "red", ylim = c(-1,1), ylab = "")
        legend("topleft", c("NN Hedge Asset 1", "NN Hedge Asset 2"),
               col = "blue", lty = c(1,2))
      }
      list(Asset1 = hedges_asset1, Asset2 = hedges_asset2)
    }
    
    compare_ratios_basket <- function(asset, t, rho = rho){
      index <- (t + 0.1)*10 
      if(asset == 1){
        nn <- ratios_asset1[[index]]$NN
        #mc <- ratios_asset1[[index]]$MC
      }
      else if(asset == 2){
        nn <- ratios_asset2[[index]]$NN
        #mc <- ratios_asset2[[index]]$MC
      } else{
        stop("Asset misspecified, select 1 or 2 porfavor!")
      }
      list(NN = nn)
    }
    
    hedge_over_time_basket <- function(path_num, showplot = T, rho = rho){
      nn1 <- rep(0, times = N)
      nn2 <- rep(0, times = N)
      #mc1 <- rep(0, times = N)
      #mc2 <- rep(0, times = N)
      for (i in 1:N) {
        nn1[i] <- compare_ratios_basket(asset = 1, t = TimePoints[i], rho = rho)$NN[path_num]
        nn2[i] <- compare_ratios_basket(asset = 2, t = TimePoints[i], rho = rho)$NN[path_num]
        #mc1[i] <- compare_ratios_basket(asset = 1, t = TimePoints[i], rho = rho)$MC[path_num]
        #mc2[i] <- compare_ratios_basket(asset = 2, t = TimePoints[i], rho = rho)$MC[path_num]
      }
      if(showplot == T){
        tit1 <- paste0("Asset Paths for simulation Nr.", path_num)
        tit2 <- "Hedging Ratios over Time"
        tit3 <- "Correlation over Time"
        print("Plotting...")
        ## Plot Asset over time:
        par(mfrow = c(2,2))
        plot(TimePoints[1:(N+1)], test_gens[,,path_num][1,], type = "line", col = "blue", ylim = c(100, 200), ylab = "", main = tit1, xlab = "t")
        par(new = T)
        plot(TimePoints[1:(N+1)], test_gens[,,path_num][2,], type = "line", col = "black", ylim = c(100, 200), ylab = "", xlab = "")
        legend("topleft", c("Asset 1", "Asset 2"), col = c("blue", "black"), lty = 1, cex = 0.5)
        
        ## Plot correlation over time:
        plot(TimePoints[1:(N+1)], test_gens[,,path_num][3,], type = "line", col = "black", ylim = c(-1,1), ylab = "Correlation", xlab = "t", main = tit3)
        
        ## Plot hedging ratios:
        # Asset 1:
        
        plot(TimePoints[1:N], nn1, type = "line",col = "blue", ylim = c(-1,1), ylab = "", main = tit2, xlab = "t")
        #par(new = T)
        #plot(TimePoints[1:N], mc1, type = "line",lty = 2, col = "blue", ylim = c(0,1), ylab = "", xlab = "")
        # Asset 2:
        par(new = T)
        plot(TimePoints[1:N], nn2, type = "line",col = "black", ylim = c(-1,1), ylab = "", xlab = "")
        #par(new = T)
        #plot(TimePoints[1:N], mc2, type = "line", lty = 2, col = "black", ylim = c(0,1), ylab = "", xlab = "")
        legend("topleft", c("NN Hedge Asset 1", "NN Hedge Asset 2"),
               col = c("blue", "black"), lty = 1, cex = 0.5)
      }
      NN <- list(asset1 = nn1, asset2 = nn2)
      #MC <- list(asset1 = mc1, asset2 = mc2)
      #list(NN = NN, MC = MC)
      list(NN = NN)
    }
    
    ## Function to get Evolution of Hedging Portfolio's value
    hedging_pf_val_basket <- function(path_num, showplot = T, rho = rho){
      ## Neural Network
      hp_nn_val <- rep(0, times = (N+1))
      hp_nn_val[1] <- V_0
      #hp_mc_val <- rep(0, times = (N+1))
      #hp_mc_val[1] <- V_0
      for (i in 2:(N+1)) {
        ## NN Hedging Portfolio
        gains_from_asset1_nn <- (prices1[[i]][path_num] - prices1[[i-1]][path_num]) * ratios_asset1[[i-1]]$NN[path_num]
        gains_from_asset2_nn <- (prices2[[i]][path_num] - prices2[[i-1]][path_num]) * ratios_asset2[[i-1]]$NN[path_num]
        hp_nn_val[i] <- hp_nn_val[i-1] + gains_from_asset1_nn + gains_from_asset2_nn
        ## MC Hedging Portfolio
        #gains_from_asset1_mc <- (prices1[[i]][path_num] - prices1[[i-1]][path_num]) * ratios_asset1[[i-1]]$MC[path_num]
        #gains_from_asset2_mc <- (prices2[[i]][path_num] - prices2[[i-1]][path_num]) * ratios_asset2[[i-1]]$MC[path_num]
        #hp_mc_val[i] <- hp_mc_val[i-1] + gains_from_asset1_mc + gains_from_asset2_mc
      }
      option_payoff <- max((prices1[[(N+1)]][path_num] + prices2[[(N+1)]][path_num])/2 - strike, 0)
      if(showplot == T){
        par(mfrow = c(1,1))
        title <- paste("Evolution of Hedging Portfolio vs. Option Payoff, Sim Nr.", path_num)
        plot(TimePoints, hp_nn_val, type = "line", main = title, ylim = c(0, 50), ylab = "Value", xlab = "t", col = "blue")
        abline(option_payoff, 0, col = "purple")
        #par(new = T)
        #plot(TimePoints, hp_mc_val, type = "line", ylim = c(0,50), ylab = "", xlab = "", col = "black")
        legend("topleft", c("NN Hedging Portfolio", "Option Payoff"), col = c("blue", "purple"), lty = 1, cex = 0.5)
      }
      #list(NN = hp_nn_val, MC = hp_mc_val)
      list(NN = hp_nn_val)
    }
    
    
    ## Function to get MC hedging ratios
    ##### Neural Networks Architecture
  {
      print(paste("Chunk started at", Sys.time()))
      
      ###### Neural Networks Architecture ######
      ### -------------------- 1. Neural Network for Initial Wealth --------------------
      d_V0 <- 2  # number of hidden layers for initial wealth network
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
      print(paste("Generating paths, started at", Sys.time()))
      training_paths <- path_gen_stoch(Ktrain) # generates training paths
      print(paste("Path generation done at", Sys.time()))
      xtrain <- shape_inputs_stoch(arr = training_paths) # reshapes TP's
      ytrain <- f(training_paths)
      
      # Actual training
      print("Training started")
      model_wealth %>% fit(
        x = xtrain,
        y = ytrain,
        epochs = epochs,
        batch_size = batch_size,
        verbose = 1
      )
      
      print("Generating Test Paths")
      # Test the model
      test_gens <- path_gen_stoch(Ktest) # Generates Ktest testpaths for S1 and S2
      xtest <- shape_inputs_stoch(arr = test_gens)
      ytest <- f(test_gens)
      
      #test_res <- compare_ratios(asset = 1, t = 0.2, rho = 0.35)
      print(paste("Chunk ended at", Sys.time()))
    }
    
    {
      ## Get Prices
      # Get ratios and prices for asset 1
      ratios_asset1 <- replicate(N, list())
      prices1 <- replicate(N+1, list())
      for(i in 1:N){
        ratios_asset1[[i]] <- list(NN = compare_ratios(1, i/10-1/10, rho = rho)$NN)
        prices1[[i]] <- test_gens[1,i,1:Ktest]
      }
      prices1[[N+1]] <- test_gens[1, N+1, 1:Ktest]
      
      # Get ratios and prices for asset 2
      prices2 <- replicate(N, list())
      ratios_asset2 <- replicate(N+1, list())
      for(i in 1:N){
        ratios_asset2[[i]] <- list(NN = compare_ratios(2, i/10-1/10, rho = rho)$NN)
        prices2[[i]] <- test_gens[2,i,1:Ktest]
      }
      prices2[[N+1]] <- test_gens[2, N+1, 1:Ktest]
    }
    
    ## Performance of NN model
    
    sum_vec <- rep(0, Ktest)
    for (i in 1:N) {
      sum_vec <- sum_vec + (prices1[[i+1]] - prices1[[i]]) * ratios_asset1[[i]]$NN + (prices2[[i+1]] - prices2[[i]]) * ratios_asset2[[i]]$NN
    }
    final_payoffs <- pmax((prices1[[N+1]] + prices2[[N+1]])/2 - strike, 0)
    V_0 <- predict(pi, cbind(S_1_0, S_2_0))
    final_hedge_pos <- as.numeric(V_0) + sum_vec
    m_nn <- mean(fin_nn <- final_payoffs - final_hedge_pos)
    

    ## Saving plots
    
    example_path_num <- ceiling(runif(1) * Ktest)
    while(final_payoffs[example_path_num] == 0){
      example_path_num <- ceiling(runif(1) * Ktest)
    }
    
    ## Chunk to save histogram output as PDF
    # 1. Define the output path
    plot_path <- paste0(output_folder, "_hist_gamma", gamma, "_GAMMA_", GAMMA, "_alpha_", alpha, "_v", version, ".pdf")
    
    # 2. Open the PDF device
    pdf(plot_path, width = pdf_width, height = pdf_height)
    
    # 3. Re-run your plotting code
    par(mfrow = c(1,1), oma = c(0,0,3,0))
    hist(fin_nn, main = "NN Strategy" ,ylim = c(0,Ktest/4), xlab = "Net Payoff", breaks = 20)
    #hist(fin_theory, main = "MC Strategy", xlim = c(-10, 10),ylim = c(0,2500), xlab = "Net Payoff", breaks = 20)
    mtext(paste0("Payoffs Hedging Portfolio net of final Option Values"), outer = T)
    # 4. Close the device to write the file
    dev.off()
    
    ## Chunk to save hedge_over_time output as PDF
    
    # 1. Define the output path
    plot_path <- paste0(output_folder, "_hedge_over_time_gamma", gamma, "_GAMMA_", GAMMA, "_alpha_", alpha,"_v",version, ".pdf")
    
    # 2. Open the PDF device
    pdf(plot_path, width = pdf_width, height = pdf_height)
    
    
    # 3. Re-run your plotting code
    hedge_over_time_basket(path_num = example_path_num, showplot = T, rho = rho)
    
    # 4. Close the device to write the file
    dev.off()
    
    ## Chunk to save evolution output as PDF
    
    # 1. Define the output path
    plot_path <- paste0(output_folder, "_pf_over_time_gamma", gamma, "_GAMMA_", GAMMA, "_alpha_", alpha,"_v",version, ".pdf")
    
    # 2. Open the PDF device
    pdf(plot_path)
    
    # 3. Re-run your plotting code
    hedging_pf_val_basket(path_num = example_path_num, showplot = T, rho = rho)
    
    # 4. Close the device to write the file
    dev.off()
    
    ## Risk measures
    # VaR
    vars_nn <- quantile(fin_nn, probs = c(0.05, 0.95))
    #vars_mc <- quantile(fin_theory, probs = c(0.05, 0.95))
    
    var_nn <- quantile(fin_nn, probs = c(0.05))
    #var_mc <- quantile(fin_theory, probs = c(0.05))
    
    # Expected Shortfall
    es_nn <- mean(fin_nn[which(fin_nn < as.numeric(quantile(fin_nn, 0.05)))])
    #es_mc <- mean(fin_theory[which(fin_theory < as.numeric(quantile(fin_theory, 0.05)))])
    
    # Quadratic Hedging Error
    qhe_nn <- mean((final_payoffs - final_hedge_pos)^2)
    
    res <- data.frame(gamma = gamma,
                      GAMMA = GAMMA,
                      alpha = alpha,
                      m_nn = m_nn,
                      var_nn = as.numeric(var_nn),
                      es_nn = es_nn,
                      qhe_nn = qhe_nn,
                      Ktest = Ktest)
    res
    if(file.exists(results_file)){
      results_so_far <- read.csv(results_file, header = T)
      res <- rbind(results_so_far[,-1], res)
    }
    write.csv(res, file = results_file)
    print(paste("Ended at", Sys.time()))
  }
  reps <- reps+1
  print(paste0("Simulation", reps, "out of", goal_reps, "done."))
    } # alphas
  }#GAMMAS
}#gammas
