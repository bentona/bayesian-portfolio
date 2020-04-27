

######################################## DEFINED FUNCTIONS ##############################################

######################################## <FUNC_NAME> ##############################################

######################################## LOAD_DATA ##############################################

# Read in and transform the data
load_data <- function(filename="sample_6.csv"){
  sample <- read.csv(filename, stringsAsFactors = FALSE)
  sample$date <- as.Date(sample$datetime)
  sample$symbol <- as.factor(sample$symbol)
  sample
}



######################################## GROUP_IDENT ##############################################

group_ident <- function(x,y){ x } # can use add_return_pct instead if we need this in the future



######################################## GROUP_BY_SYMBOL ##############################################

group_by_symbol <- function(sample) {
  sample %>% group_by(symbol) %>% group_map(group_ident, keep = TRUE)
}


######################################## DAILY_CLOSE_FUNC ##############################################


# Daily close

daily_close_func <- function(by_symbol){
  N <- nrow(by_symbol[[1]])
  C <- length(by_symbol)
  
  levs <- levels(map(by_symbol, function(c){c$symbol})[[1]])
  
  daily_close <- matrix(NA, N,C+1)
  colnames(daily_close) <- c("date",levs)
  daily_close[,"date"] <- by_symbol[[1]]$date
  for (i in 1:C){
    daily_close[,(i+1)] <- by_symbol[[i]]$close
    
    
  }
  
  #daily_close[2:N,2:(C+1),drop=FALSE] # Preserve matrix format even if C=1
  return(daily_close)
}


######################################## LOG_CLOSE ##############################################

# Let X be the logged values of daily_close
log_close <- function(daily_close){
  # dimensions of daily close
  N <- nrow(daily_close)
  C <- ncol(daily_close)
  X <- matrix(NA, N,C)
  X[,1] <- daily_close[,"date"]
  X[,2:C] <- log(daily_close[,2:C])
  colnames(X) <- colnames(daily_close)
  return(X)
}


######################################## DAILY_RETURNS_FUNC ##############################################

# Daily returns

daily_returns_func <- function(by_symbol){
  N <- nrow(by_symbol[[1]])
  C <- length(by_symbol)
  
  levs <- levels(map(by_symbol, function(c){c$symbol})[[1]])
  
  daily_close <- matrix(NA, N,C+1)
  colnames(daily_close) <- c("date",levs)
  daily_close[,"date"] <- by_symbol[[1]]$date
  for (i in 1:C){
    daily_close[,(i+1)] <- by_symbol[[i]]$close
  }
  
  daily_return <- matrix(NA, N, C+1)
  colnames(daily_return) <- colnames(daily_close)
  daily_return[,1] <- daily_close[,1]
  
  for (j in 2:(C+1)){
    for (i in 2:N){
      daily_return[i,j] <- 100*(daily_close[i,j] / daily_close[i-1,j]-1)
    }
  }
  
  daily_return[2:N,2:(C+1),drop=FALSE] # Preserve matrix format even if C=1
}



######################################## FIRST_DIF_FUNC ##############################################

first_dif_func <- function(X){
  names <- colnames(X)
  # dim X
  n <- nrow(X)
  c <- ncol(X)
  date_col <- which(colnames(X)=="date")
  date <- X[,date_col]
  data <- X[,drop(-date_col)]
  X <- matrix(NA,n-1,c)
  X[,2:c] <- data[2:n,] - data[1:n-1,]
  X[,1] <- date[2:n]
  colnames(X) <- names
  return(X)
}


######################################## AUTOCORR_FUNC ##############################################

# Returns significant lags for each series, This function is on ERRORS ONLY

autocorr_func <- function(X, lags=90,print=TRUE){
  # Strip date column
  date_col <- which(colnames(X)=="date")
  ifelse(length(date_col)>0,
         data <- X[,drop(-date_col)],
         data <- X)
  
  t <- nrow(data)
  c <- ncol(data)
  out <-apply(data,2,acf,min(c(lags,t/4)))
  signif_lags <- lapply(out, sig_lag, print=print)
}




######################################## SIG_LAG ##############################################

# Check for significant lags
sig_lag <- function(x,name, print){
  test <- ifelse(out[[]]$acf[2:length(out[[]]$acf)]>0.6,TRUE,FALSE)
  corre <- which(test==TRUE)
  if(print==TRUE){
    ifelse(length(corre)==0,
           print(paste("No autocorrelation for lags 1 to ",length(test))),
           print(paste("Significant autocorrelation exist for", out[[]], "at lags:",paste(corre, sep=','))))
  }
  return(corre)
}


######################################## LM_RW_FUNC ##############################################

lm_rw_func <- function(x){
  n <- length(x)
  model <- lm(x[2:n] ~ x[1:n-1])
  fit <- summary(model)
  test <- lm_rw_t.test(fit)
  ifelse(test == TRUE,
         return(fit$residuals),
         test)
}

######################################## LM_RW_T.TEST ##############################################
lm_rw_t.test <- function(fit){
  t_value <- (1-fit$coefficients[2,1])/fit$coefficients[2,2]
  test <- if(dt(t_value, df=fit$df[2])>=0.055){TRUE}else{test<-FALSE}
  return(test)
}

######################################## IF_LENGTH_FUNC ##############################################
if_length_func <- function(x, test_length=0){
  ifelse(length(x)>test_length,FALSE,TRUE)
}









######################################## FUNCTION_NAME ##############################################


















