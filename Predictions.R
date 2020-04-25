######### Predict one period into future using random walk model

S <- nrow(stocks_rw)
burn <- 5000  # number of samples to drop for burn in
N <- nrow(daily_close)
C <- ncol(daily_close)


X <- log(daily_close[1:(N-1),2:C])
Y <- log(daily_close[2:N,2:C])

n <- nrow(X)
c <- ncol(X)

stock_names <- colnames(X)

X <- X[(n-20):n,]
Y <- Y[(n-20):n,]

# Drop first 5000 for burnin
samples <- stocks_rw[burn:S,]

# Samples to keep
E_value <- colMeans(samples)

# Expected daily return (100* = percentage)
E_return <- E_value[1:c]

# Covariance of returns
covar <- matrix(E_value[(c+1):length(E_value)],c,c, dimnames = list(stock_names,stock_names))


# Create a sample space
S <- 1000000 # number of samples to take

pred_Y <- matrix(NA,S,c)

pred_Y <- X[1,] + rmvnorm(S,E_return, covar)

pred_sum <- summary(pred_Y)
pred_range <- quantile(pred_Y[,1], c(0.025,0.5,0.975))

pred_error <- Y[1,] - colMeans(pred_Y)

  
  
  
  
  
  
  
  
  
  
  


