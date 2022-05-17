#Finance 567 spring 2022
#solution to Homework 5 (realized variance)
library(readxl)
goog_data = read_excel("F567.s2022.HW5.GOOG data.xlsx", skip = 5)
goog_data = subset(goog_data, select = -c(9,10,11))
ge_data = read_excel("F567.s2022.HW5.GE data.xlsx", skip = 5)

#function for continuously compounded returns
c_ret = function(data, min){
  n = length(data)
  result = log(data[(1+min):n]/data[1:(n-min)])
  return(result)
}

#function for rv calculation
calc_rv = function(data, min){
min_ret = c_ret(data,min) 
offsets = 0
for(i in 2:10){offsets[i] = offsets[i-1]+391}
res = 0
for(i in 1:10){
  idx = 1+offsets[i]
  res[i] = sum(min_ret[idx : (idx + 390-min)]^2)/min
}
return(res)
}


min_vals = c(1,2,5,10,15)
rv_vals = matrix(0,nrow = 10,ncol=5)
avg_rvs = 0

#q1 and  2.a answers below
for(i in 1:5){
  rv_vals[,i] = calc_rv(goog_data$close,min_vals[i]) 
  avg_rvs[i] = mean(rv_vals[,i])
}


###q.b
scale_1 = 1
scale_2 = (1+1*195/194)/2
scale_5 = (1+4*78/77)/5
scale_10 = (1+9*39/38)/10
scale_15 = (1+14*26/25)/15
scales = c(scale_1,scale_2,scale_5,scale_10,scale_15)
new_rv_vals = matrix(0,nrow=10,ncol=5)
new_rv_avgs =0 
for(i in 1:5){
  new_rv_vals[,i] = scales[i]*rv_vals[,i]
  new_rv_avgs[i] = mean(new_rv_vals[,i])
}

##q2.c

avg_var1 = new_rv_avgs[5]/new_rv_avgs[1]
avg_var2 = new_rv_avgs[5]/new_rv_avgs[4]

##3.a
q3_rvs = matrix(0,nrow = 10,ncol=5)
avg_vals_1 = 0
for(i in 1:5){
  q3_rvs[,i] = calc_rv(ge_data$close,min_vals[i]) 
  avg_vals_1[i] = mean(q3_rvs[,i])
}
q3_new_rvs= matrix(0,nrow=10,ncol=5)
avg_vals_2 =0 
for(i in 1:5){
  q3_new_rvs[,i] = scales[i]*q3_rvs[,i]
  avg_vals_2[i] = mean(q3_new_rvs[,i])
}


##q3.b
q3_var1 = avg_vals_2[5]/avg_vals_2[1]
q3_var2 = avg_vals_2[5]/avg_vals_2[4]

##q4 
goog_data$date = as.Date(goog_data$date)
rv_dates = as.Date("2013-02-06"):as.Date("2013-02-21")
open_pr = 0
cl_pr = c(765.74, rep(0,15))
for(i in 1:16){
  open_pr[i] = goog_data[which(goog_data$date == rv_dates[i]), 'open'][[1]][1]
  temp = goog_data[which(goog_data$date == rv_dates[i]), 'close'][[1]]
  if(length(temp)){cl_pr[i+1] = tail(temp, n=1)}
}
cl_pr = cl_pr[cl_pr != 0]
open_pr = open_pr[!is.na(open_pr)]
cl_pr[12] = 795.53
cl_close = log(cl_pr[2:12]/cl_pr[1:11])
cl_open = log(open_pr[1:10]/cl_pr[1:10])

sum_sq_cl = sum(cl_close[1:10]^2)
new_rv_avgs[5]
ratio = sum_sq_cl/new_rv_avgs[5]

rv_rescale = new_rv_vals[,5]*ratio
rv_close = new_rv_vals[,5]+ cl_open^2

