source('./data.R')

sample <- load_data()
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

sample_index <- sample(1:nrow(Omega_samples), 1)
covmat <- matrix(Omega_samples[sample_index,], nrow=Nstocks, ncol=Nstocks)

mu <- mu_samples[sample_index]

draw <- rmvnorm(n=1, mean=mu1, sigma=covmat)

w <- runif(Nstocks)
w <- w/sum(w)


sharpe <- function(w){
  E <- draw %*% w
  port_var <- t(w) %*% covmat %*% w
  E/sqrt(port_var)
} 

optimization <- optim(
  w, # initial guess
  sharpe, # fn to optimize
  method = c("L-BFGS-B"), # only l-bfgs-b supports bounding for multi-dimensional optim
  control=list(
    maxit=10000, # default iters isn't enough to converge
    fnscale=-1 # We want to maximize, not minimize
  ),
  lower=0 # lower bound for each var
)

optimal <- optimization$par

normalized_optimal <- optimal/sum(optimal)

normalized_optimal


test_sample <- load_data(filename='./test.csv')
test_by_symbol <- group_by_symbol(test_sample)
test_daily_return <- daily_returns(test_by_symbol)


stock_values <- matrix(ncol=Nstocks, nrow=nrow(test_daily_return))
stock_values[1,] <- normalized_optimal
for (i in 2:nrow(stock_values)){
  stock_values[i,] <- (test_daily_return[i,]/100 + 1) * stock_values[i-1,]
}

sum(stock_values[nrow(stock_values),])

