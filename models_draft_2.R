
#########################################################################################################################

#############         THIS FILE READS IN DATA AND MODELS LOGGED STOCK PRICES USINGS A RANDOM WALK       #############


#########################################################################################################################
#########################################################################################################################


# SET WORKING DIRECTORY

setwd("~/Documents/bayesian-portfolio/")


#########################################################################################################################


# SOURCE DATA

#source('./data_1.R')


# Multivariate Random walk for model for fitting daily stock returns.
S <- 10000
N <- nrow(daily_close)
C <- ncol(daily_close)

stocks_rw <- matrix(NA,S,c+c*c)


# Data 
X <- log(daily_close[1:(N-1),2:C])
Y <- log(daily_close[2:N,2:C])

stock_names <- colnames(X)

# Let Y take the value of the first difference
Y <- Y-X
n <- nrow(Y)
c <- ncol(Y)

# Initial values
covmat <- cov(Y)
mu1 <- colMeans(Y)
df <- n-c


# Priors
A <- t(Y)%*%Y 
mu0 <- rep(0,c)
V <- diag(c)

P_Y <- matrix(NA,n,c)

avg_errors <- matrix(S,n,c)


for(s in 1:S){
  P <- covmat + V
  M <- mu1 + mu0
  alpha <- rmvnorm(1,mu,P)
  
  covmat <- rinvwishart(df+c, A+V)
  cov_out <- matrix(covmat, 1, c*c)
  
  stocks_rw[s,] <- c(alpha, cov_out)
}

cov_names <- NULL
for (i in 1:c){
  for (j in 1:c){
    cov_names[j+c*(i-1)] <- paste0(stock_names[i],"-",stock_names[j])
  }
}
colnames(stocks_rw) <- c(stock_names, cov_names)


#save.image("./Random_walk_data.RData")






