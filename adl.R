rm(list = ls())
setwd('/Users/dawnstaana/Documents/NUS/Year 4/Sem 2/EC4304/Data')
# install.packages('dLagM')
library(dplyr)
library(zoo)
library(Hmisc)

df = read.csv('final.csv',stringsAsFactors = F)
df$X = NULL
sum(is.na(df))
# drop these variables as some error on some window
df$inf_pctchg = NULL
df$t5_pctchg = NULL
df$t10_pctchg = NULL
str(df)
# change date column to date and tbill from character to numeric
df$Date = as.Date(df$Date,format='%d/%m/%y')
df$t5_pctchg = as.numeric(df$t5_pctchg)
df$t10_pctchg = as.numeric(df$t10_pctchg)
head(df)
ncol(df)

#create lags 1 to 30 for each predictor (including y-var)
clean = function(df,col){
  for (i in 1:30){
    result = Lag(df[[col]],+i)
    df = cbind(df,result)
    colnames(df)[length(df)] = paste(col,i,sep = '')
  }
  return(df)
}

#duplicate of main df
xx = df
for (j in colnames(df)[-1]){
  xx = clean(xx,j)
}
colnames(xx)
sum(is.na(xx))

#drop non lag variables and the NAs due to extracting lags
df2 = xx[,c(1:2,which(colnames(xx) == "dji_pctchg1"):ncol(xx))]
df2 = df2[complete.cases(df2),]
sum(is.na(df2))

#num = number of lags to be considered
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

# write.csv(df2,'df2.csv')

lst = data.frame()

#wind = length of rolling window (365 days) h = steps of prediction
rolladl = function(df,wind,h){
  #how long is the observation not taking into a/c window size
  len = nrow(df) - wind - h + 1 
  test = df[(wind+h):nrow(df),] #train 1:365 and predict 366 onwards
  preds = c() #store predictions
  for (i in 1:len){
    train = df[i:(i+wind-1),]
    lst = data.frame() #store AIC for each lag
    for(j in 1:30){
      first = lag_func(train,j) #get the column for respective lag
      lmfirst = lm(dji_pctchg~.-Date,data = first) # run adl
      res = data.frame(lag=i,AIC=AIC(lmfirst)) #store lag and AIC
      lst = rbind(lst,res)
      print(paste(i,j)) a
    }
    opt = lag_func(yy[i:(i+wind-1),],which.min(abs(lst$AIC))) #find optimal lag
    lmopt = lm(dji_pctchg~.-Date,data = opt) #run optimal lag model
    result = predict(lmopt,newdata=test[i,]) #predict on test set
    preds = c(preds,result) #append
  }
  return(preds)
}


