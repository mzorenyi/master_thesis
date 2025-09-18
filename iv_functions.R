varvector <- function(x){   ## creates vector containing exogenous variables
  K <- data[x,2] ## strike price
  P <- data[x,3] ## option price
  t <- data[x,4] ## time to maturity
  S <- data[x,5] ## underlying asset price
  c(K,P,t,S)
} 

generalcall <- function(x, sigma, r=0.03){   ##risk free rate default value 0.03
  K <- varvector(x)[1] ## strike price recalled from var vector
  t <- varvector(x)[3] ## time to maturity from recalled from var vector
  S <- varvector(x)[4] ## underlying price recalled from var vector
  d1 <- (log(S/K) + (r+0.5*sigma^2)*t)/(sigma*sqrt(t)) ## d1 black scholes
  d2 <- d1 - sigma*sqrt(t) ## d2 black scholes
  
  N <- function(y){
    pnorm(y)       ## CDF of Normal Distribution
  }
  
  S*N(d1) - K*exp(-r*t)*N(d2) ## call option black scholes formula
}


implied_volatility <- function(x){
  
  generalcall <- function(x, sigma, r=risk_free){   ##risk free rate default value 0.03
    K <- x$strike ## strike price recalled from var vector
    t <- x$ttm ## time to maturity from recalled from var vector
    S <- x$S0 ## underlying price recalled from var vector
    d1 <- (log(S/K) + (r+0.5*sigma^2)*t)/(sigma*sqrt(t)) ## d1 black scholes
    d2 <- d1 - sigma*sqrt(t) ## d2 black scholes
    
    N <- function(y){
      pnorm(y)       ## CDF of Normal Distribution
    }
    
    S*N(d1) - K*exp(-r*t)*N(d2) ## call option black scholes formula
  }
  
  
  i <- 0 ## starting value for i at volatility of 0%
  C <- x$price ## price of option, loop will stop if it exceeds this value
  P <- generalcall(x,i) ## starting value for P, (generalcall(x,0))
  
  while (P <= C) {
    #print(i)                    ## loop that tries different values for sigma till
    i <- i+0.0001                ## it reaches the listed for on the market
    P <- generalcall(x,i)
  }
  i
}

iv_asset <- function(asset){
  S_0 <- as.numeric(asset$stockdata[aod])
  indices <- head(sort(abs(as.numeric(asset$calls$Strikes) - S1_0), index = T)$ix, 3)
  get_varvector <- function(index){
    strike <- asset$calls$Strikes[index]
    price <- asset$calls$Schl[index]
    data.frame(strike = as.numeric(strike), price = as.numeric(price))
  }
  outp <- get_varvector(indices)
  input_vec <- cbind(outp, ttm = 1, S0 = S_0)
  res <- rep(0, times = 3)
  for (i in 1:3) {
    res[i] <- implied_volatility(input_vec[i,])
  }
  return(mean(res))
}
