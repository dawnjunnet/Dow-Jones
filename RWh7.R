# Load libraries
setwd('/Users/dawnstaana/Documents/NUS/Year 4/Sem 2/EC4304/Data')
library(forecast) # rwf function
library(Metrics)  # mse function
library(tseries)  # adf.test
source('func_pred.R')

rw.rolling.window=function(Y,nprev,h){
  
  save.pred=matrix(NA,nprev,1) #blank for forecasts
  model = list() #create variable to store model
  for(i in nprev:1){#NB: backwards FOR loop: going from 180 down to 1
    Y.window=Y[(1+nprev-i):(nrow(Y)-i-(h-1)),] #define the estimation window (first one: 1 to 491, then 2 to 492 etc.)
    model = rwf(Y.window,h=h,drift = TRUE)$mean[h]
    save.pred[(1+nprev-i),]=model
    cat('Iteration',(1+nprev-i),"\n")
  }
  #Some helpful stuff:
  real=Y #get actual values
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2)) #compute RMSE
  mse=mean((tail(real,nprev)-save.pred)^2) #compute MSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("rmse"=rmse,"mse"=mse, "mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"errors"=errors)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}

# Read data
data <- read.csv("final2.csv",header = T)
head(data)

# convert to time series object
data_ts <- ts(subset(data,select=dji_pctchg),start=c(1990,1),
              frequency=365.25)

# Visualize series dji_pctchg
plot.ts(data_ts,xlab="Time",ylab="",main="DJIA Price Movements")


# Test stationarity
print(adf.test(data_ts))


# Split data into test and training set
ntrain <- 4197
data_train<-window(data_ts,start=c(1990,1),end=c(1990,ntrain))
data_test<-window(data_ts,start=c(1990,ntrain+1))
nh = length(data_test)

# Constructing random walk forecast
# Fixed window forecasts
#fit_rw <- rwf(data_train,nh)

# Plot of the forecasts based on fixed window
#plot(data_ts,xlab="Year",ylab="",main="DJIA price movements")
#lines(fit_rw$mean,col="red",lwd=2)


# mean squared error (Fixed Window)
#mse(fit_rw$mean,data_test)



# *****************************************************************************


# Constructing random walk forecast
######################## Rolling window h=1 ########################

h=1
fcast_rolling <- rep(0,nh)
vv = window(data_ts)
rwf(vv,h=7)$mean[7]
for (i in 1:nh) {
  print(i)
  # rolling window 1
  window_rolling<-window(data_ts,start=c(1990,1+i),end=c(1990,ntrain+i))
  
  
  # Forecasts random walk based on above windows
  fcast_rolling[i+h]<-rwf(window_rolling,h=h,drift = TRUE)$mean[h]
  
}

# Plot of the forecasts based on rolling windows 

# plot for rolling window forecasts
plot(data_ts,xlab="Time",ylab="",main="DJIA Price Movements")
lines(ts(fcast_rolling,start = c(1990,ntrain+1),frequency = 365.25),
      lwd=0.1,col="blue")


# Accuracy Tests Mse
(mse_rolling_h1 = mse(fcast_rolling,data_test))


# Accuracy Tests Rmse
(rmse_rolling_h1 = rmse(fcast_rolling,data_test))


# Accuracy Tests Mae
(mae_rolling_h1 = mae(fcast_rolling,data_test))


# write.csv(fcast_rolling,"fcast_rollingh1.csv")

######################## Rolling window h=1 ########################
data <- read.csv("final2.csv",header = T)
Y = data.matrix(data$dji_pctchg)
colnames(Y) = 'dji_pctchg'
nprev =2495
h=1
fcast_rolling_1 <- rw.rolling.window(Y,nprev,h = 1)
# write.csv(fcast_rolling_1$error,"rw_error.csv")
# write.csv(fcast_rolling_1$pred,"fcast_rollingh1.csv")

######################## Rolling window h=7 ########################
Y = data.matrix(data$dji_pctchg)
colnames(Y) = 'dji_pctchg'
nprev =2495
h=7
fcast_rolling_7 <- rw.rolling.window(Y,nprev,h = 7)
write.csv(fcast_rolling_7$pred,"fcast_rollingh7.csv")
write.csv(fcast_rolling_7$errors,"rw7_error.csv")

######################## Rolling window h=14 ########################
h=14
fcast_rolling_14 <- rw.rolling.window(Y,nprev,14)
# write.csv(fcast_rolling_14$pred,"fcast_rollingh14.csv")
# write.csv(fcast_rolling_14$error,"rw14_error.csv")






