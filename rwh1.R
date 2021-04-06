rm(list = ls())
library(aTSA)
require(sqldf)
library(sqldf)
library(glmnet)
library(HDeconometrics)
library(forecast) # rwf function
library(Metrics)  # mse function
install.packages("ggplot2")
library("ggplot2")

##########################################################
#read data
#########################################################
setwd('/C:/Users/YuLiang/Desktop/NUS/Sem 8/EC4304/Group/R2/R2')
df <- read.csv("final.csv")
#drop date col respectively
df <- df[,-1]
#if there are non-finite datapoints, we just set it to NA
#is.na(df) <- sapply(df, is.infinite)
#set all NA datapoints to 0
#df[is.na(df)] <- 0
#write.csv(df,'final.csv')
#df <- read.csv("final.csv")
#check to make sure without_na dataframe should be have the same number of observations as initial df
#without_na = df[complete.cases(df), ]

#new_DF <- df[rowSums(is.na(df)) > 0,]
#a1NotIna2 <- sqldf('SELECT * FROM df EXCEPT SELECT * FROM new_DF')

#Y is a matrix of all X and Y variables
Y = data.matrix(df)
#get dependent variable we want to predict
yy = df$dji_pctchg
#test data
nprev = 2495 #test date starts on 4/1/2010
oosy = tail(yy,nprev)



###############################################################################
## inspect data
###############################################################################
adf.test(yy, output = TRUE)

df$days <- seq.int(nrow(df))
ggplot(df, aes(x=days, y= dji_pctchg)) + geom_line() + ggtitle("DOWJONES")

#replace with your own source path
setwd("/C:/Users/YuLiang/Desktop/NUS/Sem 8/EC4304/Group/R2/R2")
source("func-lasso.R")
source("unconditional-mean.R")
###############################################################################
## Add on the rest of ur methods here!
###############################################################################


# Constructing random walk forecast
# Rolling window
fcast_rolling <- rep(0,nh)
for (i in 0:nh-1) {
  
  # rolling window
  window_rolling<-window(data_ts,start=c(1990,1+i),end=c(1990,ntrain+i))
  
  
  # Forecasts random walk based on above windows
  fcast_rolling[i+1]<-rwf(window_rolling,h=1)$mean
  
}

# Plot of the forecasts based on rolling windows

# plot for rolling window forecasts
plot(data_ts,xlab="Year",ylab="",main="DJIA Price Movements")
lines(ts(fcast_rolling,start = c(1990,ntrain+1),frequency = 365.25),
      lwd=0.1,col="blue")


# Accuracy Tests Mse
(mse_rolling = mse(fcast_rolling,data_test))


# Accuracy Tests Rmse
(Rmse_rolling = rmse(fcast_rolling,data_test)) 


# Accuracy Tests Mae
(Mae_rolling = mae(fcast_rolling,data_test))