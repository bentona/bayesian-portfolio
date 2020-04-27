########## Dynamic modeling

source("./def_functions.R")

# Define filename for selected sample
file = "sample_6.csv"

# Get log closeing price
X <- log_close(daily_close_func(group_by_symbol(load_data(file))))


# Check for autocorrelations print=TRUE by default
# Random walk
rw <- first_dif_func(X)
rw_autocorrs <- autocorr_func(rw,60, print=FALSE)

# linear model of random walk Y_t = alpha + beta[Y_t-1] + epsilon_t
# If TRUE, then function returns residuals, if FALSE, function returns value FALSE
lm_rw <- apply(X[,2:ncol(X)],2,lm_rw_func)

# Check for autocorrelation in errors from linear model
lm_rw_autocorrs <- autocorr_func(lm_rw, 60, print=FALSE)

# Check for autocorrelation of errors
autocorr_check <- lapply(c(rw_autocorrs, lm_rw_autocorrs), if_length_func)

#### If all are TRUE, then no autocorrelations in the errors. Errors are stationary. 
# Move on to GARCH model
print(autocorr_check)


