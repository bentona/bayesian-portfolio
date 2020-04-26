source('./data.R')
source('./optimize_test.R')

sample <- load_data('./sample_11.csv')
by_symbol <- group_by_symbol(sample)
daily_return <- daily_returns(by_symbol)

#setwd("/Users/benton/Personal/bayesian-portfolio")
set.seed(183)

S <- 10000
port_samples <- matrix(NA,S,3)

T <- nrow(daily_return)
N <- nrow(by_symbol[[1]])

# Initial Values
mu1 <- colMeans(daily_return)
cov1 <- cov(daily_return)

# Portfolio returns

model_1_string <- textConnection("model{
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

Nstocks <- ncol(daily_return)
scale <- diag(Nstocks)

data <- list(stock_returns=daily_return, scale=scale, df=N-1, T=T, Nstocks=Nstocks)
init <- list(mu=mu1) # something like? Omega=solve(cov1)) https://sourceforge.net/p/mcmc-jags/discussion/610037/thread/eab372de/
params <- c("Omega","mu")
model_1 <- jags.model(model_1_string, data=data, inits=init, n.chains = 2, quiet=TRUE)

update(model_1, 15000)
samp_1 <- coda.samples(model_1, variable.names = params, n.iter=10000)


samples <- cbind(samp_1[[1]])
Omega_samples <- samples[,1:Nstocks^2]
mu_start <- Nstocks^2 + 1
mu_end <- mu_start + Nstocks - 1
mu_samples <- samples[,mu_start:mu_end]

covmat <- matrix(colMeans(Omega_samples), nrow = Nstocks, ncol = Nstocks)
mu <- colMeans(mu_samples)

random_weights <- runif(Nstocks)
random_weights <- random_weights/sum(random_weights)


optimal <- optimize(mu, covmat, random_weights)
portfolio_to_test <- optimal$par

normalized_portfolio <- portfolio_to_test/sum(portfolio_to_test)



test_sample <- load_data(filename='./test_6.csv')

stock_values <- test_forward(normalized_portfolio, test_sample)

port_return <- sum(stock_values[nrow(stock_values),])



market_only_port <- matrix(c(1.0), nrow=1, ncol=1)
market_forward <- load_data(filename='./test_market.csv')
market_values <- test_forward(market_only_port, market_forward)


plot_perf <- function(stock_values){
  returns <- rowSums(stock_values) #t(t(stock_values)/stock_values[1,])
  market_returns <- market_values
  
  plot(NA, xlim = c(0, length(returns)), ylim = c(.5,1.5),
       main ="Stock Prices", xlab="Date", ylab = "Price", type = "n")
  lines(seq(1, length(returns)), returns, col=1)
  lines(seq(1, length(returns)), market_returns, col=2)
  
 # legend("topright", line_labels[2:k], col = 2:k, lty = 1, bty="n", cex = 0.6)
  
}

plot_perf




