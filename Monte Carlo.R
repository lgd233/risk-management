#
# Solution to Fin 567 Homework 3 spring 2021
#

library(fOptions) #needed to compute option values
library(MASS)     #needed to simulate multivariate normal rvs
library(mvtnorm) #needed to simulate multivariate t rvs

# Question 1 Monte Carlo VaR using the Normal distribution
# (a) Use the binomial model to compute the value of the portfolio
nsteps = 21 #number of steps in binomial tree
ABCcall0 <- CRRBinomialTreeOption(TypeFlag = "ca", S=97.37, X=100, Time = 21/252,
            r=0.01, b=0.01, sigma=0.45, n=nsteps, title = NULL, description = NULL)@price
ABCput0  <- CRRBinomialTreeOption(TypeFlag = "pa", S=97.37, X=100, Time = 21/252,
            r=0.01, b=0.01, sigma=0.45, n=nsteps, title = NULL, description = NULL)@price

DEFcall0 <- CRRBinomialTreeOption(TypeFlag = "ca", S=101.27, X=100, Time = 21/252,
            r=0.01, b=0.01, sigma=0.37, n=nsteps, title = NULL, description = NULL)@price
DEFput0  <- CRRBinomialTreeOption(TypeFlag = "pa", S=101.27, X=100, Time = 21/252,
            r=0.01, b=0.01, sigma=0.37, n=nsteps, title = NULL, description = NULL)@price

GHIcall0 <- CRRBinomialTreeOption(TypeFlag = "ca", S=121.34, X=120, Time = 21/252,
                                  r=0.01, b=0.01, sigma=0.40, n=nsteps, title = NULL, description = NULL)@price
GHIput0  <- CRRBinomialTreeOption(TypeFlag = "pa", S=121.34, X=120, Time = 21/252,
                                  r=0.01, b=0.01, sigma=0.40, n=nsteps, title = NULL, description = NULL)@price

V0 <- -50*100*ABCcall0-50*100*ABCput0 - 50*100*DEFcall0 - 50*100*DEFput0 - 40*100*GHIcall0 - 40*100*GHIput0


# (b) Compute the 1% MC VaR
set.seed(137)
mu = c(0.0005, 0.0004, 0.0004) 
cov <- matrix(c(0.028^2,         0.028*0.023*0.4, 0.028*0.025*0.4, 
                0.023*0.028*0.4, 0.023^2,         0.023*0.025*0.4, 
                0.025*0.028*0.4, 0.025*0.023*0.4, 0.025^2), nrow=3, ncol=3)
n = 10000
returns <- mvrnorm(n, mu = mu, Sigma = cov)
S <- matrix(rep(0,3*n), nrow= n, ncol = 3)
S[1:n,1] = 97.37*exp(returns[1:n,1])
S[1:n,2] = 101.27*exp(returns[1:n,2])
S[1:n,3] = 121.34*exp(returns[1:n,3])

ABCcall <- rep(0,n)
ABCput <- rep(0,n)
DEFcall <- rep(0,n)
DEFput <- rep(0,n)
GHIcall <- rep(0,n)
GHIput <- rep(0,n)
V <- rep(0,n)

# Use a loop to compute the simulated options prices
# Note that the remaining time to expiration is 20 days
nsteps = 20
for(i in 1:n){
  ABCcall[i] <- CRRBinomialTreeOption(TypeFlag = "ca", S=S[i,1], X=100, Time = 20/252,
                r=0.01, b=0.01, sigma=0.45, n=nsteps, title = NULL, description = NULL)@price
  ABCput[i]  <- CRRBinomialTreeOption(TypeFlag = "pa", S=S[i,1], X=100, Time = 20/252,
                r=0.01, b=0.01, sigma=0.45, n=nsteps, title = NULL, description = NULL)@price
  
  DEFcall[i] <- CRRBinomialTreeOption(TypeFlag = "ca", S=S[i,2], X=100, Time = 20/252,
                r=0.01, b=0.01, sigma=0.37, n=nsteps, title = NULL, description = NULL)@price
  DEFput[i]  <- CRRBinomialTreeOption(TypeFlag = "pa", S=S[i,2], X=100, Time = 20/252,
                r=0.01, b=0.01, sigma=0.37, n=nsteps, title = NULL, description = NULL)@price
  
  GHIcall[i] <- CRRBinomialTreeOption(TypeFlag = "ca", S=S[i,3], X=120, Time = 20/252,
                                      r=0.01, b=0.01, sigma=0.40, n=nsteps, title = NULL, description = NULL)@price
  GHIput[i]  <- CRRBinomialTreeOption(TypeFlag = "pa", S=S[i,3], X=120, Time = 20/252,
                                      r=0.01, b=0.01, sigma=0.40, n=nsteps, title = NULL, description = NULL)@price
}
V <- -50*100*ABCcall-50*100*ABCput - 50*100*DEFcall - 50*100*DEFput - 40*100*GHIcall - 40*100*GHIput
PLQ1 = V - V0
MCVaRQ1 <- - quantile(PLQ1, 0.01)

# (c) Plot the distribution of profits and losses on the portfolio
hist(PLQ1, main = "Q1(c): P/L Distribution", xlab = "P/L", xlim = c(-40000, 10000), las = 1, breaks = 20)

# (d) Compute the expected shortfall
ESQ1 = -mean(PLQ1[PLQ1<=(-MCVaRQ1)])


#Question 2
#Monte Carlo VaR using a horizon of 21 days
mumonth = 21*mu     #mean return increases with time
covmonth = 21*cov   #variance increases with time
n = 10000
returns = mvrnorm(n, mu = mumonth, Sigma = covmonth)
S <- matrix(rep(0,3*n), nrow= n, ncol = 3)
S <- matrix(rep(0,3*n), nrow= n, ncol = 3)
S[1:n,1] = 97.37*exp(returns[1:n,1])
S[1:n,2] = 101.27*exp(returns[1:n,2])
S[1:n,3] = 121.34*exp(returns[1:n,3])
ABCcall=rep(0,n)
ABCput=rep(0,n)
DEFcall=rep(0,n)
DEFput=rep(0,n)
GHIcall=rep(0,n)
GHIput=rep(0,n)
V <- rep(0,n)

# Here the horizon of the VaR estimate = remaining life of the options
# Thus the option value after 21 days is the option payoff
for(i in 1:n){
  ABCcall[i] <- max(S[i,1]-100,0)
  ABCput[i]  <- max(100-S[i,1],0)
  DEFcall[i] <- max(S[i,2]-100,0)
  DEFput[i]  <- max(100-S[i,2],0)
  GHIcall[i] <- max(S[i,3]-100,0)
  GHIput[i]  <- max(100-S[i,3],0)
}
V <- -50*100*ABCcall-50*100*ABCput - 40*100*DEFcall - 40*100*DEFput- 40*100*GHIcall - 40*100*GHIput
PLQ2 = V - V0
MCVaRQ2 <- - quantile(PLQ2, 0.01)

# Question 3
# Monte Carlo VaR using bivariate t distribution
# (a) Compute the 1% MC VaR
mu = c(0.0005, 0.0004, 0.0004)
cov <- matrix(c(0.028^2,         0.028*0.023*0.4, 0.028*0.025*0.4, 
                0.023*0.028*0.4, 0.023^2,         0.023*0.025*0.4, 
                0.025*0.028*0.4, 0.025*0.023*0.4, 0.025^2), nrow=3, ncol=3)
nu = 4
scale = ((nu-2)/nu)*cov  #scale matrix input to mvrt() is smaller than cov matrix
set.seed(137)
n = 10000
returns = rmvt(n, sigma = scale, df = nu, delta = mu) # parameter sigma is the scale matrix
S <- matrix(rep(0,3*n), nrow= n, ncol = 3)
S[1:n,1]= 101.17*exp(returns[1:n,1])
S <- matrix(rep(0,3*n), nrow= n, ncol = 3)
S[1:n,1] = 97.37*exp(returns[1:n,1])
S[1:n,2] = 101.27*exp(returns[1:n,2])
S[1:n,3] = 121.34*exp(returns[1:n,3])
ABCcall=rep(0,n)
ABCput=rep(0,n)
DEFcall=rep(0,n)
DEFput=rep(0,n)
GHIcall=rep(0,n)
GHIput=rep(0,n)
V <- rep(0,n)

for(i in 1:n){
  ABCcall[i] <- CRRBinomialTreeOption(TypeFlag = "ca", S=S[i,1], X=100, Time = 20/252,
                                      r=0.01, b=0.01, sigma=0.45, n=20, title = NULL, description = NULL)@price
  ABCput[i]  <- CRRBinomialTreeOption(TypeFlag = "pa", S=S[i,1], X=100, Time = 20/252,
                                      r=0.01, b=0.01, sigma=0.45, n=20, title = NULL, description = NULL)@price
  
  DEFcall[i] <- CRRBinomialTreeOption(TypeFlag = "ca", S=S[i,2], X=100, Time = 20/252,
                                      r=0.01, b=0.01, sigma=0.37, n=20, title = NULL, description = NULL)@price
  DEFput[i]  <- CRRBinomialTreeOption(TypeFlag = "pa", S=S[i,2], X=100, Time = 20/252,
                                      r=0.01, b=0.01, sigma=0.37, n=20, title = NULL, description = NULL)@price
  
  GHIcall[i] <- CRRBinomialTreeOption(TypeFlag = "ca", S=S[i,3], X=120, Time = 20/252,
                                      r=0.01, b=0.01, sigma=0.40, n=20, title = NULL, description = NULL)@price
  GHIput[i]  <- CRRBinomialTreeOption(TypeFlag = "pa", S=S[i,3], X=120, Time = 20/252,
                                      r=0.01, b=0.01, sigma=0.40, n=20, title = NULL, description = NULL)@price
}
V <- -50*100*ABCcall-50*100*ABCput - 50*100*DEFcall - 50*100*DEFput - 40*100*GHIcall - 40*100*GHIput
PLQ3 = V - V0
MCVaRQ3 <- - quantile(PLQ3, 0.01)

# (b) Plot the distribution of profits and losses on the portfolio
# hist(PLQ3)
hist(PLQ3, main = "Q3(b): P/L Distribution", xlab = "P/L", xlim = c(-40000, 10000), las = 1, breaks = 100)

# (b) Compute the expected shortfall
ESQ3 = -mean(PLQ3[PLQ3<=(-MCVaRQ3)])

