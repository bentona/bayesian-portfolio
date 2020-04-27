
optimize <- function(model_mu, model_cov, guess_w){
  sharpe <- function(port){
    E <- model_mu %*% port
    port_var <- t(port) %*% model_cov %*% port
    E/sqrt(port_var)
  } 
  
  optim(
    guess_w, # initial guess
    sharpe, # fn to optimize
    method = c("L-BFGS-B"), # only l-bfgs-b supports bounding for multi-dimensional optim
    control=list(
      maxit=10000, # default iters isn't enough to converge
      fnscale=-1 # We want to maximize, not minimize
    ),
    lower=0 # lower bound for each var
  )
}


test_forward <- function(portfolio, forward_sample){
  test_by_symbol <- group_by_symbol(forward_sample)
  test_daily_return <- daily_returns(test_by_symbol)
  
  stock_values <- matrix(ncol=length(portfolio), nrow=nrow(test_daily_return))
  stock_values[1,] <- portfolio
  for (i in 2:nrow(stock_values)){
    stock_values[i,] <- (test_daily_return[i,]/100 + 1) * stock_values[i-1,]
  }
  
  stock_values
}