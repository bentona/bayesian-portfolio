
#########################################################################################################################

#############         THIS FILE READS IN DATA AND MODELS STOCK RETURNS USINGS A RANDOM WALK       #############


#########################################################################################################################
#########################################################################################################################


# SET WORKING DIRECTORY

setwd("~/Documents/bayesian-portfolio/")


#########################################################################################################################


# SOURCE DATA

source('./data.R')


# Multivariate Random walk for model for fitting daily stock returns.
S <- 10000
N <- nrow(daily_return)
C <- ncol(daily_return)

stocks_rw <- list()


# Data 
X <- daily_return[1:(N-1),]
Y <- daily_return[2:N,]
n <- nrow(Y)

# Initial values
mu <- colMeans(Y-X)
covmat <- cov(Y-X)

# Priors
mu0 <- rep(0,C)
tau <- 10
V <- diag(C)/tau

P_Y <- matrix(NA,n,C)

avg_errors <- matrix(S,n,C)


for(s in 1:S){
  P <- (1/n)*covmat + V
  
  for (t in 1:n){
    P_Y[t,] <- rmvnorm(1,X[t,],P)
  }
  
  epsilon <- Y - P_Y
  avg_errors <- colMeans(epsilon)
  colnames(epsilon) <- colnames(X)
  covmat2 <- cov(epsilon)
  
  covmat <- rinvwishart(n-C, covmat2)
  
  stocks_rw[[s]] <- list(P_Y = P_Y, covmat = covmat)
}










