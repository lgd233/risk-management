###Homework 7 Question 1

d2i = qnorm(0.01)
corr = matrix(0.3,100,100)
diag(corr) = 1
library(MASS)
?mvrnorm
def = 0
loss = 0
for(i in 1:10000){
zi = mvrnorm(1,mu = rep(0,100),corr)
reco = matrix(0,100,1)
reco[which(zi<d2i)] = 10000000*rbeta(1,2,2)
loss[i] = sum(reco)
def[i] = sum(zi<d2i)
}
hist(loss,breaks = seq(0,400000000,10000000),
     main = "histogram of credit loss, rho = 0.3, random lgd")

cor1 = matrix(0.4,100,100)
diag(cor1) = 1
def1 = 0
loss1 = 0
for(i in 1:10000){
  zi = mvrnorm(1,mu = rep(0,100),cor1)
  reco = matrix(0,100,1)
  reco[which(zi<d2i)] = 10000000*rbeta(1,2,2)
  loss1[i] = sum(reco)
  def1[i] = sum(zi<d2i)
}
hist(loss1,breaks = seq(0,400000000,10000000),
     main = "histogram of credit loss, rho = 0.4, random lgd")

cor2 = matrix(0.1,100,100)
diag(cor2) = 1
def2 = 0
loss2 = 0
for(i in 1:10000){
  zi = mvrnorm(1,mu = rep(0,100),cor2)
  reco = matrix(0,100,1)
  reco[which(zi<d2i)] = 10000000*rbeta(1,2,2)
  loss2[i] = sum(reco)
  def2[i] = sum(zi<d2i)
}
hist(loss2,breaks = seq(0,400000000,10000000),
     main = "histogram of credit loss, rho = 0.1, random lgd")