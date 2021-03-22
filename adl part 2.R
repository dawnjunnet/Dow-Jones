rm(list = ls())
setwd('/Users/dawnstaana/Documents/NUS/Year 4/Sem 2/EC4304/Data')
# install.packages('dLagM')
library(dplyr)
library(zoo)
library(Hmisc)
df2 = read.csv('df2.csv',stringsAsFactors = F)
colnames(df2)
df2$X = NULL
str(df2)

df2$Date = as.Date(df2$Date)
lag_func = function(df,num){
  jj = rep(3,num) #3 as the 3rd column is the lag of dji (yvar)
  colnum = c(1:2) #column for date and dji (yvar)
  
  #to extract the other 1st lags (i.e. dji_lag1, ind_lag1, pe_lag1)
  for (i in 0:length(jj)-1){
    colnum = c(colnum,jj[i+1]+i,jj[i+1]+i+seq(30,30*8,30))
  }
  df = df[,colnum]
  return(df)
}

#wind = length of rolling window (365 days) h = steps of prediction
rolladl = function(df,wind,h){
  #how long is the observation not taking into a/c window size
  len = nrow(df) - wind - h + 1 
  test = df[(wind+h):nrow(df),] #train 1:365 and predict 366 onwards
  preds = c() #store predictions
  for (i in 1:len){
    train = df[i:(i+wind-1),]
    lst = data.frame() #store AIC for each lag for each training window
    for(j in 1:30){
      first = lag_func(train,j) #get the column for respective lag
      lmfirst = lm(dji_pctchg~.-Date,data = first) # run adl
      res = data.frame(lag=i,AIC=AIC(lmfirst)) #store lag and AIC
      lst = rbind(lst,res)
      print(paste(i,j))
    }
    opt = lag_func(df[i:(i+wind-1),],which.min(abs(lst$AIC))) #find optimal lag
    lmopt = lm(dji_pctchg~.-Date,data = opt) #run optimal lag model
    result = predict(lmopt,newdata=test[i,]) #predict on test set
    preds = c(preds,result) #append
  }
  return(preds)
}

cc = rolladl(df2,365,1)

#store date, true value and prediction in a df
msecalc = function(wind,h,res){
  
  name = data.frame(Date = df2$Date[(wind+h):nrow(df2)],pred = res,val = df2$dji_pctchg[(wind+h):nrow(df2)])
  
  mse = c()
  for (i in 1:nrow(name)){
    mse = c(mse,(name$val[i]-name$pred[i])**2)
  }
  name = cbind(name,square_error = mse)
  return(name)
}

ar = list() #store ar result
ar[[1]] = msecalc(365,1,cc) #adl 1 step ahead forecast
View(ar[[1]])
plot(ar[[1]][['Date']],ar[[1]][['val']],type="l",col="red",
     xlab = 'Date',ylab = 'dji')
lines(ar[[1]][['Date']],ar[[1]][['pred']],col="green")
# write.csv(ar[[1]],'ar1res.csv')
