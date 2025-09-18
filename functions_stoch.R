## Function for stochastic correlation dynamics
# Time Conversion function

TimeConv <- function(t) {
  # return(sqrt(T - t))   ## Works better as input variable in case of diffusion models!
  #return(T - t)  ## The NN expects a time to maturity as information
  return(t)  ## The NN expects actual time as information. The results are the same as 'time to maturity'
  ## but the information will be stored differently in the NN.
}

# Function to generate Asset paths:
path_gen_stoch <- function(replications){
  one_scen <- function(){
    #vec <- la_lista[[1]]
    vec <- matrix(data = c(log(S_1_0), log(S_2_0)), nrow = 2, ncol = N + 1)
    vec[,2:ncol(vec)] <- 0
    vec <- rbind(vec, 0) # to incorporate stochastic correlation
    # Increments:
    # Drifts
    drift1 <- mu1 - sigma1^2/2
    drift2 <- mu2 - sigma2^2/2
    # rho_0
    Z_0 <- rnorm(1)
    rho <- 2*(pnorm(Z_0) - 1/2)
    #rho_0 <<- rbind(rho_0, rho) # saves initial rho values, maybe some hidden patterns there
    vec[3,1] <- rho # NEW 10/07
    # Deltas
    for(i in 1:N){
      dt <- TimePoints[i+1] - TimePoints[i]
      Z <- rnorm(3, mean = c(0,0,0), sd = c(1,1,0.1))
      delta1 <- drift1 * dt + sigma1 * Z[1] * sqrt(dt)
      delta2 <- drift2 * dt + rho*sigma2 * Z[1] * sqrt(dt) + sqrt(1-rho^2) * sigma2 * Z[2] * sqrt(dt)
      vec[1,i+1] <- vec[1,i] + delta1
      vec[2,i+1] <- vec[2,i] + delta2
      rho <- gamma*(GAMMA - rho)*dt + alpha * sqrt(1 - rho^2)*Z[3]
      if(abs(rho) > 1){
        print("Problem")
        rho <- sign(rho) * 1
      }
      vec[3, i + 1] <- rho # NEW 10/07
    }
    vec <- exp(vec)
    vec[3,] <- log(vec[3,]) # necessary because otherwise we get exp(rho) in vec
    vec
  }
  replicate(replications, one_scen())
}


## Shaping function --> hedge NN expects inputs TTM, S1_t and S2_t
## wealth NN needs deltas of S1 and S2 to compute gains from trade.
shape_inputs_stoch <- function(arr) {
  arr <- arr[-3,,]
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

sanity_check <- function(gam, alp){
  a <- alpha^2/(1 + c(-1,1) * gam)
  max(a)
}