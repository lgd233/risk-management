# Example of calculation of HS VaR for a simple equity portfolio
# Finance 567 spring 2021
#

# Read the .csv file containing the returns and put them in a dataframe
returns <- read.csv("returns4symbols.2004-2018.csv")

# The first task is:
#   1. Assume that the current date is Jan. 2, 2008 and that you hold the following portfolio:
#      a. $5,000,000 invested in SPY
#      b. $3,000,000 invested in XLF
#      c. $1,000,000 invested in MSFT
#      d. $1,000,000 invested in AAPL
#   2. Use data from the most recent 1,000 days to compute the historical simulation VaR

# Let's create a vector v with the position values
v <- c(5000000,3000000,1000000,1000000)

# Select the most recent 1,000 returns
index <- which(returns$date == 20080102) #find the index of Jan. 2, 2008
sample <- returns[(index-999):index,1:5] #extract the most recent 1,000 returns

# Use the most recent 1,000 returns to compute 1,000 simulated or hypothetical P/Ls
portfolioPL1 = data.frame(date = sample$date,PL = v[1]*(1+sample$SPY) + v[2]*(1+sample$XLF) 
                          + v[3]*(1+sample$MSFT) + v[4]*(1+sample$AAPL)-sum(v))
# Or carry out an equivalent and somewhat simpler computation of the hypothetical P/Ls
portfolioPL2 = data.frame(date = sample$date,PL = v[1]*sample$SPY + v[2]*sample$XLF 
                          + v[3]*sample$MSFT + v[4]*sample$AAPL)

# Compute the HS VaR as the negative of the 1% quantile
HSVaR1 = -quantile(portfolioPL1[, 2], probs = 0.01)
HSVaR2 = -quantile(portfolioPL2[, 2], probs = 0.01) #should match HSVaR1


# The second task is to compute the HS VaR for every day from 1/2/2008 through 12/31/2009
# 1. Assume that on each date you hold the following portfolio:
#      a. $5,000,000 invested in SPY
#      b. $3,000,000 invested in XLF
#      c. $1,000,000 invested in MSFT
#      d. $1,000,000 invested in AAPL
# 2. Then compute the HS VaRs:
#      a. Use data from the most recent 1,000 days to compute the HS VaR on 1/2/2008
#      b. Use data from the most recent 1,000 days to compute the HS VaR on 1/3/2008
#      c. Continue until you have computed the HS VaR for every day from 1/2/2008 and 12/31/2009


# Find the indexes of 1/2/2008 and 12/31/2009
begin = which(returns$date == 20080102)
end = which(returns$date == 20091231)

# Use a loop to compute the VaRs
HSVaR3 = numeric(end-begin+1) # create a vector to hold the VaRs
# Now use a loop to compute HS VaRs:
for(j in 1:(end-begin+1)){
  sample <- returns[(begin+j-1000):(begin+j-1),1:5] #extract the most recent 1,000 returns
  # use the most recent 1,000 returns to compute 1,000 simulated or hypothetical P/Ls
  portfolioPL3 = data.frame(date = sample$date,PL = v[1]*sample$SPY + v[2]*sample$XLF 
                           + v[3]*sample$MSFT + v[4]*sample$AAPL)
  HSVaR3[j] = -quantile(portfolioPL3[, 2], probs = 0.01) #compute the HS VaR
}

# There is a slightly more efficient way to compute the VaRs
# First compute portfolio P/L using the vector V and returns from every date
portfolioPL4 = data.frame(date = returns$date, 
                         PL = v[1]*returns$SPY+v[2]*returns$XLF
                         +v[3]*returns$MSFT+v[4]*returns$AAPL)

# Once we have the portfolio P/L using the returns from every date
# we can compute HS VaR using the most recent 1,000 P/Ls
# Use a loop to do this for each date from Jan. 2, 2008 through Dec. 31, 2009:
HSVaR4 = numeric(end-begin+1)
for(j in 1:(end-begin+1)){
  HSVaR4[j] = -quantile(portfolioPL4[(begin + j - 1000):(begin + j - 1), 2], probs = 0.01)
}

# Just for fun, let's also compute the standard deviation of portfolio P/L using the data from previous 21 days
# we will use r to hold the most recent 21 returns
r = matrix(0, nrow = 21, ncol = 4)
portfolioSD= numeric(end-begin+1) #create a vector to hold the standard deviations of portfolio P/L
#for each date, put the past 21 returns into the matrix r
for(j in 1:(end-begin+1)){
  r[1:21,1]=returns[(begin + j - 21):(begin + j - 1), 2]
  r[1:21,2]=returns[(begin + j - 21):(begin + j - 1), 3]
  r[1:21,3]=returns[(begin + j - 21):(begin + j - 1), 4]
  r[1:21,4]=returns[(begin + j - 21):(begin + j - 1), 5]
  equalweightedcov = (t(r) %*% r)/21  #estimate of cov matrix of returns on date begin + j - 1
  portfolioSD[j] = sqrt(v %*% equalweightedcov %*% v)
  normalQuantile = 2.326*portfolioSD
}


# Now let's plot the HS VaRs and compare them to the standard deviation of portfolio P/L

#before plotting the two series find the max and min values so we can set the range for the y axis
upperylim = max(max(HSVaR4),max(portfolioSD))
lowerylim = min(min(HSVaR4),min(portfolioSD))

# Create the dates vector for the plot
dates_plot = portfolioPL4$date[which(portfolioPL4$date == 20080102):which(portfolioPL4$date == 20091231)]
dates_for_plot = as.data.frame(dates_plot)
dates_for_plot = transform(dates_for_plot, 
                           dates_plot = as.Date(as.character(dates_plot), "%Y%m%d"))

#now plot the two series
plot(x = dates_for_plot$dates_plot, y = HSVaR4, type = 'l', xaxt = "n",
     ylim = c(lowerylim, upperylim), col = "firebrick", 
     xlab = "time", ylab = "VaR")
axis(1, dates_for_plot$dates_plot, format(dates_for_plot$dates_plot, "%b%y"), cex.axis = .7)
lines(x = dates_for_plot$dates_plot, y = portfolioSD, col = "forestgreen")

legend("topleft",
       legend  = c("HS VaR", "SD of P/L"),
       col = c("firebrick", "forestgreen"), lty = 1, cex=0.55)


# Finally let's plot the HS VaRs and compare them to the negative of the 1% normal quantile

#before plotting the two series find the max and min values so we can set the range for the y axis
upperylim = max(max(HSVaR4),max(normalQuantile))
lowerylim = min(min(HSVaR4),min(normalQuantile))

# Create the dates vector for the plot
dates_plot = portfolioPL4$date[which(portfolioPL4$date == 20080102):which(portfolioPL4$date == 20091231)]
dates_for_plot = as.data.frame(dates_plot)
dates_for_plot = transform(dates_for_plot, 
                           dates_plot = as.Date(as.character(dates_plot), "%Y%m%d"))

#now plot the two series
plot(x = dates_for_plot$dates_plot, y = HSVaR4, type = 'l', xaxt = "n",
     ylim = c(lowerylim, upperylim), col = "firebrick", 
     xlab = "time", ylab = "VaR")
axis(1, dates_for_plot$dates_plot, format(dates_for_plot$dates_plot, "%b%y"), cex.axis = .7)
lines(x = dates_for_plot$dates_plot, y = normalQuantile, col = "deepskyblue")

legend("topleft",
       legend  = c("HS VaR", "normal Quantile"),
       col = c("firebrick", "deepskyblue"), lty = 1, cex=0.55)