
#########################################################################################################################

#############         THIS FILE READS IN DATA AND MODELS LOGGED STOCK PRICES USINGS A RANDOM WALK       #############


#########################################################################################################################
#########################################################################################################################


# SET WORKING DIRECTORY

#setwd("~/Documents/bayesian-portfolio/")

#by_symbol <- group_by_symbol(sample)

#daily_close <- daily_returns(by_symbol)

#########################################################################################################################


# SOURCE DATA

#source('./data_1.R')


# Multivariate Random walk for model for fitting daily stock returns.
S <- 100000
N <- nrow(daily_close)
C <- ncol(daily_close)

for_pred <- 20        # Number of values to keep for prediction



# Data 
X <- log(daily_close[1:(N-1),2:C])
Y <- log(daily_close[2:N,2:C])

stock_names <- colnames(X)
n <- nrow(Y)
c <- ncol(Y)

X <- X[1:(n-for_pred),]
Y <- Y[1:(n-for_pred),]

# Let Y take the value of the first difference
Y <- Y-X


# Space for samples
stocks_rw <- matrix(NA,S,c+c*c)

# Initial values
covmat <- cov(Y)
mu1 <- colMeans(Y)
df <- n-c


# Priors
A <- t(Y)%*%Y 
mu0 <- rep(0,c)
V <- diag(c)


for(s in 1:S){
  P <- covmat 
  if(s==1){P <- covmat+V}
  M <- mu1 + mu0
  alpha <- rmvnorm(1,M,P)
  
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






