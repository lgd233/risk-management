# Example of using maximum likelihood to estimate GPD parameters

# read the csv file containing the index data and compute returns and loss = -return
SP500 <- read.csv("4_MLE_data.csv") 
SP500 <- data.frame(date = SP500[2:nrow(SP500),1],
                     return = log(SP500[2:nrow(SP500),5]/SP500[1:(nrow(SP500)-1),5]),
                     loss = -log(SP500[2:nrow(SP500),5]/SP500[1:(nrow(SP500)-1),5]))


#compute and graph the mean excess losses
u <- seq(0.01, 0.05, by=0.002)
meanexcess <- numeric(length(u))
for(i in 1:length(u)){
  meanexcess[i] <-  mean(SP500$loss[SP500$loss >= u[i]]-u[i])
}

#plot the mean excess losses
upperylim = max(meanexcess)
lowerylim = min(min(meanexcess),0)
plot(x = u, y = meanexcess, type = 'l', ylim = c(lowerylim,upperylim), col = "deepskyblue", 
     xlab = "Threshold u", ylab = "Mean excess")
legend("topleft",legend  = c("Mean excess"), col = c("deepskyblue"), lty = 1, cex=0.7)

#Losses greater than the threshold
largeloss = SP500$loss[SP500$loss >= 0.022]
Q2losses = length(largeloss)


#MLE of the GPD parameters xi and beta

excessloss = largeloss - 0.022 #we work with the excess losses beyond the threshold u = 0.022

#We need a function that returns the negative of the log likelihood 
fr1 <- function(x){  
  xi = x[1]
  beta= x[2]

  ##log Likelihood 
  if (x[1] < 0 || x[2] < 0){
    neglogLH = 9999
  } else {
    logdensity = log(1/beta) - (1 + 1/xi)*log(1+(xi/beta)*excessloss)
    neglogLH = -sum(logdensity)
  }
  return(neglogLH)
}

#initial guess for the parameter vector
guess <- c(0.50, 0.10)
#carry out the optimation
Q3 = optim(guess,fr1, hessian = TRUE)
xi = Q3$par[1]
beta = Q3$par[2]
Hessian = Q3$hessian
covariance = solve(Hessian)  #inverse of the Hessian is the covariance matrix of the estimates
stderr_xi = sqrt(covariance[1,1]) #std. error is square root of element on main diagonal
stderr_beta = sqrt(covariance[2,2]) #std. error is square root of element on main diagonal


#Graph the GPD conditional density function using the estimated parameters
x <- seq(0.022, 0.10, by=0.001) #vector of values for which to graph density
excess = x - 0.022
GPDdensity <- (1/beta)*(1+(xi/beta)*excess)^(-(1 + 1/xi))  

#plot the GPD density
upperylim = max(GPDdensity)
lowerylim = min(min(GPDdensity),0)
plot(x = x, y = GPDdensity, type = 'l', ylim = c(lowerylim,upperylim), col = "deepskyblue", 
     xlab = "Loss", ylab = "density")
legend("topleft",legend  = c("GPD density"),
       col = c("deepskyblue"), lty = 1, cex=0.7)



#calculate and plot various probabilities
x <- seq(0.022, 0.10, by=0.001)  #vector of values for which to graph probabilities
excess = x - 0.022
conditionalprob = (1+(xi/beta)*excess)^(-1/xi) 
prob022 = length(largeloss)/length(SP500$loss)
GPDprob = prob022*conditionalprob

#plot the GPD tail probabilities
upperylim = max(GPDprob)
lowerylim = min(min(GPDprob,0))
plot(x = x, y = GPDprob, type = 'l', ylim = c(lowerylim,upperylim), col = "deepskyblue", 
     xlab = "Loss", ylab = "Probability")
legend("topleft",legend  = c("GPD probability"),
       col = c("deepskyblue"), lty = 1, cex=0.7)

#The probabilities requested by Question 5 are:
Q5prob022 = GPDprob[1]
Q5prob05 = GPDprob[x == 0.05]
Q5prob10 = GPDprob[x == 0.10]