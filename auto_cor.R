# This provides general test for serially correlation (autocorrelation)

source("./data.R")

# Define filename for selected sample
file = "sample_6.csv"

# Get log closeing price
X <- log_close(daily_close_func(group_by_symbol(load_data(file))))


# Check for autocorrelations print=TRUE by default
autocorrs_rw <- autocorr_func(X,60, print=TRUE)


