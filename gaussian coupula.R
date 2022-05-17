#Finance 567 spring 2019
#solution of Homework 8

pa= c(rep(20000000,20),rep(10000000,100))  #principal amounts of the loans
pd = 0.012                                 #probability of default
defrate = -log(1-pd)
copulafn = function(rho = 0, rlgd = FALSE, clgd = 1){
alpha = 2
beta = 2
totloss = 0
totdefault = 0
tot_hedgedloss = 0
firstloss = 0
totpairs = 120*(120-1)/2
defaultpairs = 0
ratiopairs = 0
numberdefault = 0
for(i in 1:10000){
  m = rnorm(1)
  ifactor = rnorm(120)
  zi = (rho^(0.5)*m)+((1-rho)^(0.5)*ifactor)
  deft = -log(1-pnorm(zi))/defrate
  default = rep(0,120)
  default[which(deft<=1)] = 1
  betarec = rbeta(120,alpha,beta, ncp = 0)
  if(rlgd == TRUE){lgd = betarec}
  else{lgd = rep(clgd,120)}
  loss = lgd*pa
  loss[which(deft>1)] = 0
  losshedge = loss*0.5
  unhedgeloss = loss-losshedge
  totloss[i] = sum(loss)
  firstloss[i] = min(sum(losshedge[1:20]), 0.2*20*10000000)
  tot_hedgedloss[i] = totloss[i] - firstloss[i]
  # totdefault[i] = totloss[i] - firstloss[i]
  numberdefault[i] = sum(default)
  #defaultpairs[i] = totdefault[i]*(totdefault[i]-1)/2
  defaultpairs[i]=numberdefault[i]*(numberdefault[i]-1)/2
  ratiopairs[i] = defaultpairs[i]/totpairs
}
perport = totloss/sum(pa)
return(data.frame(portfolio_loss = perport, firstloss = firstloss/sum(pa), 
                  hedged_loss = tot_hedgedloss/sum(pa), ratiopairs = ratiopairs))

}

#a version with 2,000 obligors
copulafn2 = function(rho = 0, rlgd = FALSE, clgd = 1){
  alpha = 2
  beta = 2
  totloss = 0
  totdefault = 0
  tot_hedgedloss = 0
  firstloss = 0
  totpairs = 2000*(2000-1)/2
  defaultpairs = 0
  ratiopairs = 0
  numberdefault = 0
  for(i in 1:10000){
    m = rnorm(1)
    ifactor = rnorm(2000)
    zi = (rho^(0.5)*m)+((1-rho)^(0.5)*ifactor)
    deft = -log(1-pnorm(zi))/defrate
    default = rep(0,2000)
    default[which(deft<=1)] = 1
    betarec = rbeta(2000,alpha,beta, ncp = 0)
    if(rlgd == TRUE){lgd = betarec}
    else{lgd = rep(clgd,2000)}
    loss = lgd*pa
    loss[which(deft>1)] = 0
    losshedge = loss*0.5
    unhedgeloss = loss-losshedge
    totloss[i] = sum(loss)
    firstloss[i] = min(sum(losshedge[1:20]), 0.2*20*10000000)
    tot_hedgedloss[i] = totloss[i] - firstloss[i]
    # totdefault[i] = totloss[i] - firstloss[i]
    numberdefault[i] = sum(default)
    #defaultpairs[i] = totdefault[i]*(totdefault[i]-1)/2
    defaultpairs[i]=numberdefault[i]*(numberdefault[i]-1)/2
    ratiopairs[i] = defaultpairs[i]/totpairs
  }
  perport = totloss/sum(pa)
  return(data.frame(portfolio_loss = perport, firstloss = firstloss/sum(pa), 
                    hedged_loss = tot_hedgedloss/sum(pa), ratiopairs = ratiopairs))
  
}

#Question 1a:
res1 = copulafn(rho =0,rlgd = FALSE, clgd = 0.5)
hist(res1$portfolio_loss, breaks = seq(0,0.05,0.0025), 
     main = "histogram of portfolio credit loss rho = 0, lgd = 0.5") #1.a

#Question 1b:
res2 = copulafn(rho =0.3,rlgd = FALSE, clgd = 0.5)
hist(res2$portfolio_loss, breaks = seq(0,0.30,0.0025),
     main =  "histogram of portfolio credit loss rho = 0.3, lgd = 0.5") #1.b
#hist(res2$portfolio_loss[which(res2$portfolio_loss<0.1)], breaks = seq(0,0.1,0.005))

#Question 1c:
pa= rep(100,2000)  #principal amounts of the loans
pd = 0.012         #probability of default
defrate = -log(1-pd)
res3 = copulafn2(rho =0.3,rlgd = FALSE, clgd = 1)
pab1c = mean(res3$ratiopairs)
defcorr1c =(pab1c-pd2*pd2)/(sqrt(pd2*(1-pd2))*sqrt(pd2)*(1-pd2)) #1.c

#Question 1d:
res4 = copulafn2(rho =0.3,rlgd = FALSE, clgd = 1)
std_defrate1d = sd(res4$portfolio_loss)

#Question 1e:
res5 = copulafn2(rho=0.24, rlgd = FALSE, clgd = 1)
std_defrate1e = sd(res5$portfolio_loss)


#Question 2:
EC2a= quantile(res1$portfolio_loss, 0.999)*sum(pa) - pd*sum(pa) #2.a
EC2b= quantile(res2$portfolio_loss, 0.999)*sum(pa) - pd*sum(pa) #2.b

#Question 3:
hist(res1$hedged_loss,breaks = seq(0,0.10,0.005),
     main = "histogram of credit loss on hedged port., rho = 0, lgd = 0.5") #3.a

meanhedgedloss3b = mean(res1$hedged_loss)*sum(pa) #3.b
EC3b= quantile(res1$hedged_loss, 0.999)*sum(pa) - meanhedgedloss3b #3.b

hist(res2$hedged_loss, breaks = seq(0,0.30,0.0025),
     main = "histogram of credit loss on hedged port., rho = 0.3, lgd not random") #3.c 
meanhedgedloss3c = mean(res2$hedged_loss)*sum(pa) #3.c
EC3c= quantile(res2$hedged_loss, 0.999)*sum(pa) - meanhedgedloss3c #3.b

#Question 4:
pa= c(rep(20000000,20),rep(10000000,100))
res4 = copulafn(rho =0,rlgd = TRUE, clgd = 1)
hist(res4$portfolio_loss, breaks = seq(0,0.08,.005),
     main =  "histogram of credit loss on portfolio, rho = 0, lgd random") #4.a
meanportloss4a = mean(res4$portfolio_loss)*sum(pa) #4.a

res5 = copulafn(rho =0.3,rlgd = TRUE, clgd = 1)
hist(res5$hedged_loss , breaks = seq(0,0.30,.0025),
     main =  "histogram of credit loss on hedged port., rho = 0.3, lgd random")#4.b
meanportloss4b = mean(res5$portfolio_loss)*sum(pa) #4.b

