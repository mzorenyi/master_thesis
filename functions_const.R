## Functions for constant Correlation ##
path_gen_const <- function(replications){
  one_scen <- function(){
    #vec <- la_lista[[1]]
    vec <- matrix(data = c(log(S_1_0), log(S_2_0)), nrow = 2, ncol = N + 1)
    vec[,2:ncol(vec)] <- 0
    # Increments:
    # Drifts
    drift1 <- mu1 - sigma1^2/2
    drift2 <- mu2 - sigma2^2/2
    # Deltas
    for(i in 1:N){
      dt <- TimePoints[i+1] - TimePoints[i]
      Z <- rnorm(2)
      delta1 <- drift1 * dt + sigma1 * Z[1] * sqrt(dt)
      delta2 <- drift2 * dt + rho*sigma2 * Z[1] * sqrt(dt) + sqrt(1-rho^2) * sigma2 * Z[2] * sqrt(dt)
      vec[1,i+1] <- vec[1,i] + delta1
      vec[2,i+1] <- vec[2,i] + delta2
    }
    exp(vec)
  }
  replicate(replications, one_scen())
}

# Time Conversion function
TimeConv <- function(t) {
  T-t
}

shape_inputs_const <- function(arr) {
  diffs <- apply(arr, c(1,3), function(x) diff(x))
  K <- dim(arr)[3]
  N <- dim(arr)[2] - 1
  d <- dim(arr)[1]
  x <- list(array(0, dim = c(K, N, 1 + d)), array(0, dim = c(K, N, d)))
  for (i in 1:N) {
    x[[1]][, i, 1] <- rep(TimeConv(TimePoints[i]), K)
  }
  
  # Add S1 into array
  for (i in 1:K) {
    x[[1]][i,,2] <- arr[1,1:N,i]
  }
  
  # Add S2 into array
  for (i in 1:K) {
    x[[1]][i,,3] <- arr[2,1:N,i]
  }
  
  # x[[2]]:
  # Delta S1:
  for (i in 1:K) {
    x[[2]][i,,1] <- diffs[,1,i]
  }
  
  # Delta S2:
  for (i in 1:K) {
    x[[2]][i,,2] <- diffs[,2,i]
  }
  return(x)
}

get_nn_hedge_ratios <- function(tt, test_paths, model){
  index_S <- tail(which(TimePoints <= tt), 1)
  ttm <- rep(TimeConv(tt), times = Ktest)
  # Get S_t
  S <- apply(test_paths, MARGIN = 2, function(x) x)
  S1 <- S[seq(1, nrow(S), by = 2),]
  S2 <- S[seq(2, nrow(S), by = 2),]
  S1 <- S1[,index_S] # selects S_t^1
  S2 <- S2[,index_S] # selects S_t^2
  test_input <- cbind(ttm, S1, S2)
  hedge_ratios <- predict(hedge, test_input, verbose = 0)
  H1 <- hedge_ratios[,1]
  H2 <- hedge_ratios[,2]
  list(H1 = H1, H2 = H2)
}

## Functions to compute d1 and d2
## Function to compute d_t^1
d1 <- function(X_t, Y_t, rho, ttm, sigma_1 = sigma1, sigma_2 = sigma2){
  eta1 <- c(sigma_1,0)
  eta2 <- c(sigma_2 * rho, sqrt(1-rho^2) * sigma_2)
  (log(X_t/Y_t) + 1/2*norm(eta1 - eta2, "2")^2 * ttm)/sqrt(norm(eta1 - eta2, "2")^2 * ttm)
}

d2 <- function(X_t, Y_t, rho, ttm, sigma_1 = sigma1, sigma_2 = sigma2){
  d1_t <- d1(X_t, Y_t, rho, ttm, sigma_1, sigma_2)
  eta1 <- c(sigma_1,0)
  eta2 <- c(sigma_2 * rho, sqrt(1 - rho^2) * sigma_2)
  d1_t - sqrt(norm(eta1 - eta2, "2")^2 * ttm)
}

## Function to compare NN hedging ratios to theoretical hedging ratios





## Function to show evolution of hedging portfolio's value over time
hedging_pf_val_exchange <- function(path_num, showplot = T, rho = rho){
  ## Neural Network
  hp_nn_val <- rep(0, times = (N+1))
  hp_nn_val[1] <- V_0
  hp_theor_val <- rep(0, times = (N+1))
  hp_theor_val[1] <- V_0
  for (i in 2:(N+1)) {
    ## NN Hedging Portfolio
    gains_from_asset1_nn <- (prices1[[i]][path_num] - prices1[[i-1]][path_num]) * ratios_asset1[[i-1]]$NN[path_num]
    gains_from_asset2_nn <- (prices2[[i]][path_num] - prices2[[i-1]][path_num]) * ratios_asset2[[i-1]]$NN[path_num]
    hp_nn_val[i] <- hp_nn_val[i-1] + gains_from_asset1_nn + gains_from_asset2_nn
    ## Theor. Hedging Portfolio
    gains_from_asset1_theor <- (prices1[[i]][path_num] - prices1[[i-1]][path_num]) * ratios_asset1[[i-1]]$THEORY[path_num]
    gains_from_asset2_theor <- (prices2[[i]][path_num] - prices2[[i-1]][path_num]) * ratios_asset2[[i-1]]$THEORY[path_num]
    hp_theor_val[i] <- hp_theor_val[i-1] + gains_from_asset1_theor + gains_from_asset2_theor
  }
  option_payoff <- max(prices1[[(N+1)]][path_num] - prices2[[(N+1)]][path_num], 0)
  upper_limit <- option_payoff + 10
  if(showplot == T){
    par(mfrow = c(1,1))
    title <- paste("Evolution of Hedging Portfolios vs. Option Payoff, Sim Nr.", path_num)
    plot(TimePoints, hp_nn_val, type = "line", main = title, ylim = c(0, upper_limit), ylab = "Value", xlab = "t", col = "blue")
    abline(option_payoff, 0, col = "purple")
    par(new = T)
    plot(TimePoints, hp_theor_val, type = "line", ylim = c(0,upper_limit), ylab = "", xlab = "", col = "black")
    legend("topleft", c("NN Hedging Portfolio", "Theor. Hedging Portfolio", "Option Payoff"), col = c("blue", "black", "purple"), lty = 1, cex = 0.5)
  }
  list(NN = hp_nn_val, THEORY = hp_theor_val)
}
