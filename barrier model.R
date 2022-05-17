

###Homework 7 Question 2

u = 1
a = 0
m = 2.1700908523383
hmn = pnorm((m+a*u)/sqrt(u))-exp(-2*a*m)*pnorm((-m+a*u)/sqrt(u))
defprob = 1-hmn

a1 = 0.05
m1 = 2.1272584054488
hmn1 = pnorm((m1+a1*u)/sqrt(u))-exp(-2*a1*m1)*pnorm((-m1+a1*u)/sqrt(u))
defprob1= 1-hmn1

rho = 0.3
dt = 0.0025

time = seq(0,1,1/400)
n = length(time)

driftmb = 0.05

defc = 0
defd = 0
minma = 0
minmbc = 0
minmbd = 0
ratec = 0
rated = 0
set.seed(2)
for(k in 1:10) {for(i in 1:32000){
  rv = rnorm(n-1)
  rvcorr = rho*rv+sqrt(1-rho^2)*rnorm(n-1)
  ma = m
  mbc = m1
  mbd = m
  for(j in 2:n){
    ma[j] = ma[j-1] + sqrt(dt)*rv[j-1]
    mbc[j] = mbc[j-1]+driftmb*dt+sqrt(dt)*rvcorr[j-1]
    mbd[j] = mbd[j-1] + sqrt(dt)*rvcorr[j-1]
    
  }
  minma[i] = min(ma[-1])
  minmbc[i] = min(mbc[-1])
  minmbd[i] = min(mbd[-1])
  defc[i] = ifelse(((minma[i]<=0)&(minmbc[i]<=0)),1,0)
  defd[i] = ifelse(((minma[i]<=0)&(minmbd[i]<=0)),1,0)
}
ratec[k] = sum(defc)/32000
rated[k] = sum(defd)/32000
}
avgc = mean(ratec)
avgd = mean(rated)
stc = sd(ratec)/sqrt(10)
std = sd(rated)/sqrt(10)
diff = avgd-avgc


diff*320000

