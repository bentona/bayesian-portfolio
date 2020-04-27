source('./data.R')
source('./optimize_test.R')

#setwd("/Users/benton/Personal/bayesian-portfolio")

model_returns <- function(daily_return, Nstocks){
  S <- 15000

  T <- nrow(daily_return)
  N <- nrow(daily_return)
  
  # Initial Values
  mu1 <- colMeans(daily_return)
  cov1 <- cov(daily_return)
  
  # Portfolio returns
  
  model_string <- textConnection("model{
  # Priors
  
  for (i in 1:Nstocks){
    mu[i] ~ dnorm(0,0.1)
  }
  
  Omega[1:Nstocks,1:Nstocks] ~ dwish(scale[,],df)
  
  # Likelihood
  
  for (i in 2:T){
    stock_returns[i,1:Nstocks] ~ dmnorm(mu, Omega[,])
  }
}")
  scale <- diag(Nstocks)
  
  data <- list(stock_returns=daily_return, scale=scale, df=N-1, T=T, Nstocks=Nstocks)
  init <- list(mu=mu1) # something like? Omega=solve(cov1)) https://sourceforge.net/p/mcmc-jags/discussion/610037/thread/eab372de/
  params <- c("Omega","mu")
  model <- jags.model(model_string, data=data, inits=init, n.chains = 2, quiet=TRUE)
  
  update(model, S)
  samples <- coda.samples(model, variable.names = params, n.iter=S)
  samples
}


portfolio_for_model <- function(samples, Nstocks){
  samples <- cbind(samples[[1]])
  Omega_samples <- samples[,1:Nstocks^2]
  mu_start <- Nstocks^2 + 1
  mu_end <- mu_start + Nstocks - 1
  mu_samples <- samples[,mu_start:mu_end]
  
  covmat <- matrix(colMeans(Omega_samples), nrow = Nstocks, ncol = Nstocks)
  mu <- colMeans(mu_samples)
  
  optimal <- optimize(mu, covmat, seq(1, Nstocks)/Nstocks)
  portfolio_to_test <- optimal$par
  
  portfolio_to_test/sum(portfolio_to_test)
}



process <- function(training, n, test){
  result <- c()
  by_symbol <- group_by_symbol(training)
  daily_return <- daily_returns(by_symbol)
  
  result$start <-  Sys.time()
  model_samples <- model_returns(daily_return, Nstocks=n)
  result$end <-  Sys.time()
  
  result$portfolio <- portfolio_for_model(model_samples, Nstocks=n)
  result$stock_values <- test_forward(result$portfolio, test)
  result$port_return <- sum(result$stock_values[nrow(result$stock_values),])
  result
}


sample_25 <- c(
  training=load_data('./sample_25.csv'), 
  n=25, 
  test=load_data('./test_25.csv')
)


sample_6 <- c()

sample_6$training <- load_data('./sample_6.csv')
sample_6$n <- 6
sample_6$test <- load_data('./test_6.csv')


sample_11 <- c()

sample_11$training <- load_data('./sample_11.csv')
sample_11$n <- 11
sample_11$test <- load_data('./test_11.csv')


sample_14 <- c()

sample_14$training <- load_data('./sample_14.csv')
sample_14$n <- 14
sample_14$test <- load_data('./test_14.csv')


sample_25 <- c()

sample_25$training <- load_data('./sample_25.csv')
sample_25$n <- 25
sample_25$test <- load_data('./test_25.csv')

sample_27 <- c()

sample_27$training <- load_data('./sample_27.csv')
sample_27$n <- 27
sample_27$test <- load_data('./test_27.csv')





result_6 <- process(sample_6$training, sample_6$n, sample_6$test)

result_11 <- process(sample_11$training, sample_11$n, sample_11$test)

result_14 <- process(sample_14$training, sample_14$n, sample_14$test)

result_25 <- process(sample_25$training, sample_25$n, sample_25$test)

result_27 <- process(sample_27$training, sample_27$n, sample_27$test)




market_only_port <- matrix(c(1.0), nrow=1, ncol=1)
market_forward <- load_data(filename='./test_market.csv')
market_values <- test_forward(market_only_port, market_forward)



  returns_6 <- rowSums(result_6$stock_values) #t(t(stock_values)/stock_values[1,])
  returns_11 <- rowSums(result_11$stock_values)
  returns_14 <- rowSums(result_14$stock_values)
  returns_25 <- rowSums(result_25$stock_values)
  returns_27 <- rowSums(result_27$stock_values)
  market_returns <- market_values
  
  plot(NA, xlim = c(0, length(market_returns)), ylim = c(.5,2),
       main ="Stock Prices", xlab="Trading days in 2020", ylab = "Portfolio Value", type = "n")
  lines(seq(1, length(market_returns)), returns_6, col=3)
  lines(seq(1, length(market_returns)), returns_11, col=4)
  lines(seq(1, length(market_returns)), returns_14, col=5)
  lines(seq(1, length(market_returns)), returns_25, col=6)
  lines(seq(1, length(market_returns)), returns_27, col=7)
  lines(seq(1, length(market_returns)), market_returns, col=8)
  
  legend("topright", c(
    '6 stock port',
    '11 stock port',
    '14 stock port',
    '25 stock port',
    '27 stock port',
    'market'
    ), col = 3:8, lty = 1, cex = 1.5)


ex_time <- c(
  as.difftime(c(  result_6$end, result_6$start), units = "mins")

  result_11$end, result_11$start,
  result_14$end, result_14$start,
  result_25$end, result_25$start,
  result_27$end, result_27$start
)
  
#plot_perf(stock_values)

#pie(normalized_portfolio)


