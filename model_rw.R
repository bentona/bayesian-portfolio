
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

source('./def_functions.R')

# Define filename for selected sample
#file = "sample_14.csv"

# Get log closing price
X <- log_close(daily_close_func(group_by_symbol(load_data(file))))


# Multivariate Random walk for model for fitting daily stock returns.
#S <- 100000


#T <- 45        # Number of values to keep for prediction


# Let Y take the value of the first difference
Y <- first_dif_func(X)
Y <- Y[1:(nrow(Y)-T),2:ncol(Y)]

stock_names <- colnames(Y)
n <- nrow(Y)
c <- ncol(Y)

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






