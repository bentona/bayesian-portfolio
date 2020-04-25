# Plot the prices

#plot(daily_close[,1],daily_close[,2:ncol(daily_close)], type = 'l')

n <- nrow(daily_close)
k <- ncol(daily_close)

line_labels <- colnames(daily_close)[2:k]

plot(NA, xlim = range(daily_close[,1]), ylim = range(daily_close[,2:k]),
     main ="Stock Prices", xlab="Date", ylab = "Price", type = "n")
for (i in 2:k){
  lines(daily_close[,1], daily_close[,i], col= i)
  x <- round((max(daily_close[,1])-rnorm(1,80,5))- runif(1,0, 45),0)
  x2 <- ifelse(length(which(daily_close[,1]==x))==0,which(daily_close[,1]==(x+3)),which(daily_close[,1]==x))
  y <- ifelse(is.null(daily_close[x2,i]+10),daily_close[x2+3,i],daily_close[x2,i])
  text(x, y, labels = line_labels[i], cex=.6, col = 1)
}

#i=7
#plot(daily_close[,1], daily_close[,i], type = "l")
