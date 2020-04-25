# Import the data

#############################   NOTES   #######################################

# This file could be cleaned to make it more efficient? Let's review at a later date.



#load libraries

library(rjags)
library(data.table)
library(tidyverse)
library(LaplacesDemon)
library(mvtnorm)

# Clear environment

#clear.data <- readline(prompt = "Would you like to REMOVE ALL ITEMS FROM GLOBAL ENVIRONMENT (Y/N)? :")

#if(clear.data=="Y"){
 # rm(list = ls())
#}


#############################################################################################################


# Read in and transform the data
<<<<<<< HEAD
sample <- read.csv("./sample_6.csv", stringsAsFactors = FALSE)
sample$date <- as.Date(sample$datetime)
sample$symbol <- as.factor(sample$symbol)

justone <- sample[sample$symbol == 'PRTA',]
justone <- justone[1:200,]
=======
load_data <- function(filename="sample_6.csv"){
  sample <- read.csv(filename, stringsAsFactors = FALSE)
  sample$date <- as.Date(sample$datetime)
  sample$symbol <- as.factor(sample$symbol)
  sample
}
>>>>>>> 327f226f167e315e5d81089b33514f1630856155


add_return_pct <- function(x, y){
  df <- x
  df$close_30_ago = NA
  df$date_30_ago = NA
  
  date_offset <- 22
  for (i in (date_offset+1):nrow(df)){
    # TODO - do fancy date parsing for more exact offsets
    df$date_30_ago[i] <- as.character(df$date[i - date_offset])
    df$close_30_ago[i] <- df$close[i - date_offset]
  }
  
  df$last_30_return <- df$close / df$close_30_ago - 1
  df$annualized_last_30_return = (df$last_30_return + 1) ^ (365/30) - 1
  df
}

group_ident <- function(x,y){ x } # can use add_return_pct instead if we need this in the future

group_by_symbol <- function(sample) {
  sample %>% group_by(symbol) %>% group_map(group_ident, keep = TRUE)
}


#############################################################################################################

# NOT IN USE


# Create data table with 30 day stock returns by ticker name (includes date)

# varname <- as.character(by_symbol[[1]]$symbol[1])
# stock_returns <- data.table(date=by_symbol[[1]]$date, return=by_symbol[[1]]$last_30_return)
# setnames(stock_returns, "return",paste(varname))
# 
# for (i in 2:length(by_symbol)){
#   varname <- as.character(by_symbol[[i]]$symbol[1])
#   dt <- data.table(date=by_symbol[[i]]$date, return= by_symbol[[i]]$last_30_return)
#   setnames(dt, "return",paste(varname))
#   
#   stock_returns <- full_join(stock_returns,dt, by="date")
# }
# 
# stock_returns <- na.omit(stock_returns)



#############################################################################################################


# Daily returns

daily_returns <- function(by_symbol){
  N <- nrow(by_symbol[[1]])
  C <- length(by_symbol)
  
  daily_close <- matrix(NA, N,C+1)
  colnames(daily_close) <- c("date",levels(as.factor(sample$symbol)))
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
  
  daily_return[2:N,2:(C+1)]
  
  #return(daily_return) # Changed this to give prices
  return(daily_close)
}


#############################################################################################################
