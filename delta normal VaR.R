# Finance 567 spring 2022
# Homework 2, Questions 2-3

# Read the .csv file containing the returns and put them in a dataframe
returns <- read.csv("returns4symbols.2004-2018.csv")

# Let's do some preliminary stuff that is need for both Questions 2 and 3
# Create a vector v with the position values
v <- c(5000000,3000000,1000000,1000000)
# Find the indexes of 1/2/2008 and 12/31/2009
begin = which(returns$date == 20080102)
end = which(returns$date == 20091231)

# Question 2
# compute delta-Normal VaR using an equally-weighted covariance matrix 
# for each day fron 1/2/2008 to 12/31/2009

# Use a loop to compute the VaR
# we will use r to hold the most recent 250 returns
r = matrix(0, nrow = 250, ncol = 4)
DNVaR = numeric(end-begin+1) #create a vector to hold the VaRs
#for each date, put the past 250 returns into a matrix r
for(t in 1:(end-begin+1)){
  r[1:250,1]=returns[(begin + t - 250):(begin + t - 1), 2]
  r[1:250,2]=returns[(begin + t - 250):(begin + t - 1), 3]
  r[1:250,3]=returns[(begin + t - 250):(begin + t - 1), 4]
  r[1:250,4]=returns[(begin + t - 250):(begin + t - 1), 5]
  equalweightedcov = (t(r) %*% r)/nrow(r)  #estimate of cov matrix of returns on date begin + t - 1
  DNVaR[t] = 2.326 * sqrt(v %*% equalweightedcov %*% v)
}


# An alternative calculation using the function cov.wt
# This calculation should agree with the previous calculation
DNVaR2 = numeric(end-begin-1)
weight = rep(1/nrow(r), nrow(r))  #vector of (equal) weights to be passed to cov.wt
for(t in 1:(end-begin+1)){
  r[1:250,1]=returns[(begin + t - 250):(begin + t - 1), 2]
  r[1:250,2]=returns[(begin + t - 250):(begin + t - 1), 3]
  r[1:250,3]=returns[(begin + t - 250):(begin + t - 1), 4]
  r[1:250,4]=returns[(begin + t - 250):(begin + t - 1), 5]
  weightedcovoutput = cov.wt(r, weight, cor = FALSE, center = FALSE, method = "ML")
  equalweightedcov2 = weightedcovoutput$cov
  DNVaR2[t] = 2.326 * sqrt(v %*% equalweightedcov2 %*% v)
}

# Question 3
# Compute exponentially-weighted covariance matrix and delta-Normal VaR for each day fron 1/2/2008 to 12/31/2009
# Similar to calculation above, but with different weight vector
# This calcuation uses 250 past returns but fewer could be used, as weights are almost zero more than 200 days in the past
exponentialDNVaR = numeric(end-begin-1)
# Compute the vector of exponentially declining weights to be passed to cov.wt
lambda = 0.94
weight = numeric(250) # create a vector to hold the weights
weight[250] = 1 - lambda  #this is the weight for the most recent return
for(i in 249:1){
  weight[i] = lambda*weight[i+1] #weights decline exponentially
}
#for each date, put the past 250 returns into a matrix r
for(t in 1:(end-begin+1)){
  r[1:250,1]=returns[(begin + t - 250):(begin + t - 1), 2]
  r[1:250,2]=returns[(begin + t - 250):(begin + t - 1), 3]
  r[1:250,3]=returns[(begin + t - 250):(begin + t - 1), 4]
  r[1:250,4]=returns[(begin + t - 250):(begin + t - 1), 5]
  weightedcovoutput = cov.wt(r, weight, cor = FALSE, center = FALSE, method = "ML")
  exponentialcov = weightedcovoutput$cov
  exponentialDNVaR[t] = 2.326 * sqrt(v %*% exponentialcov %*% v)
}


# Now let's plot the delta-Normal VaRs 
# Before plotting the two series find the max and min values so we can set the range for the y axis
upperylim = max(max(DNVaR),max(exponentialDNVaR))
lowerylim = min(min(DNVaR),min(exponentialDNVaR))

# Create the dates vector for the plot
dates_plot = returns$date[which(returns$date == 20080102):which(returns$date == 20091231)]
dates_for_plot = as.data.frame(dates_plot)
dates_for_plot = transform(dates_for_plot, 
                           dates_plot = as.Date(as.character(dates_plot), "%Y%m%d"))

# Now plot the two series
plot(x = dates_for_plot$dates_plot, y = DNVaR, type = 'l', xaxt = "n",
     ylim = c(lowerylim, upperylim), col = "firebrick", 
     xlab = "time", ylab = "VaR")
axis(1, dates_for_plot$dates_plot, format(dates_for_plot$dates_plot, "%b%y"), cex.axis = .7)
lines(x = dates_for_plot$dates_plot, y = exponentialDNVaR, col = "forestgreen")

legend("topleft",
       legend  = c("delta-normal VaR", "exponential D-N VaR"),
       col = c("firebrick", "forestgreen"), lty = 1, cex=0.55)


