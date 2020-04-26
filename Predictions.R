######### Predict multiple periods into future using random walk model

S <- nrow(stocks_rw)
burn <- 15000  # number of samples to drop for burn in
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
S <- nrow(samples)
k <- ncol(samples)

# Samples to keep
E_value <- colMeans(samples)

# Expected daily return (100* = percentage)
E_return <- E_value[1:c]

# Covariance of returns
covar <- matrix(E_value[(c+1):length(E_value)],c,c, dimnames = list(stock_names,stock_names))


# Number of time periods to predict
T <- 45   

# Prediction list
pred_Y <- list()

# Generate predictions
x <- X[1,]
E_ln_price <- matrix(NA,S,c)
colnames(E_ln_price) <- stock_names
for (t in 1:T){
  for(s in 1:S){
    e_return <- samples[s,1:c]
    covar2 <- matrix(samples[s,(c+1):k],c,c)
    E_ln_price[s,] <- x + rmvnorm(1,e_return,covar2)
  }
  pred_Y[[t]] <- list(log=E_ln_price, last_price = x)
  x <- colMeans(E_ln_price)
  
}  




# Create forecast for plotting
forecast <- matrix(NA, T, c*3)

for (t in 1:T){
  forecast[t,1:c] <- colMeans(pred_Y[[t]]$log)
  for (j in 1:c){
   forecast[t,j+c] <- quantile(pred_Y[[t]]$log[,j],c(0.025))
   forecast[t,j+2*c] <- quantile(pred_Y[[t]]$log[,j],c(0.975))
  }
}
  
forecast <- rbind(pred_Y[[1]]$last_price, forecast)  # This inserts actual last price as initial forcast point for plotting


# Name forecast columns
names2 <- NULL

for (i in 1:c){
  names2[i] <- stock_names[i]
  names2[c+i] <- paste0(stock_names[i], "_025")
  names2[2*c+i] <- paste0(stock_names[i], "_975")
}

# Add colnames
colnames(forecast) <- c(names2)


# Create log and non-log forecast
log_forecast <- forecast

forecast <- exp(forecast)

# Create a time period tracker
for_period <- 0:T+1

log_forecast <- cbind(for_period, log_forecast)
forecast <- cbind(for_period, forecast)


#View(forecast)  
#View(log_forecast)  
