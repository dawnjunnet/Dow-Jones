# Load libraries
library(forecast) # rwf function
library(Metrics)  # mse function
library(tseries)  # adf.test



# Read data
data <- read.csv("data.csv",header = T)
head(data)

# convert to time series object
data_ts <- ts(subset(data,select=dji_pctchg),start=c(1990,1),
                frequency=365.25)

# Visualize series dji_pctchg
plot.ts(data_ts,xlab="Year",ylab="",main="DJIA Price Movements")


# Test stationarity
print(adf.test(data_ts))


# Split data into test and training set
ntrain <- 4198
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

################# Rolling window h=1 #################

fcast_rollingh1 <- rep(0,nh)
for (i in 0:nh-1) {
  
  # rolling window
  window_rolling<-window(data_ts,start=c(1990,1+i),end=c(1990,ntrain+i))
  
  
  # Forecasts random walk based on above windows
  fcast_rollingh1[i+1]<-rwf(window_rolling,h=1)$mean
  
}

# Plot of the forecasts based on rolling windows 

# plot for rolling window forecasts
plot(data_ts,xlab="Year",ylab="",main="DJIA Price Movements")
lines(ts(fcast_rolling,start = c(1990,ntrain+1),frequency = 365.25),
      lwd=0.1,col="blue")


# Accuracy Tests Mse
(mse_rollingh1 = mse(fcast_rollingh1,data_test))


# Accuracy Tests Rmse
(rmse_rollingh1 = rmse(fcast_rollingh1,data_test))


# Accuracy Tests Mae
(mae_rollingh1 = mae(fcast_rollingh1,data_test))


write.csv(fcast_rollingh1,"fcast_rollingh1.csv")


################# Rolling window h=7 #################

fcast_rollingh7 <- rep(0,nh)
for (i in 0:nh-1) {
  
  # rolling window
  window_rolling<-window(data_ts,start=c(1990,1+i),end=c(1990,ntrain+i))
  
  
  # Forecasts random walk based on above windows
  fcast_rollingh7[i+1]<-rwf(window_rolling,h=7)$mean
  
}

# Plot of the forecasts based on rolling windows 

# plot for rolling window forecasts
plot(data_ts,xlab="Year",ylab="",main="DJIA Price Movements")
lines(ts(fcast_rolling,start = c(1990,ntrain+1),frequency = 365.25),
      lwd=0.1,col="blue")


# Accuracy Tests Mse
(mse_rollingh7 = mse(fcast_rollingh7,data_test))


# Accuracy Tests Rmse
(rmse_rollingh7 = rmse(fcast_rollingh7,data_test))


# Accuracy Tests Mae
(mae_rollingh7 = mae(fcast_rollingh7,data_test))
write.csv(fcast_rollingh7,"fcast_rollingh7.csv")


################# Rolling window h=14 #################

fcast_rollingh14 <- rep(0,nh)
for (i in 0:nh-1) {
  
  # rolling window
  window_rolling<-window(data_ts,start=c(1990,1+i),end=c(1990,ntrain+i))
  
  
  # Forecasts random walk based on above windows
  fcast_rollingh14[i+1]<-rwf(window_rolling,h=7)$mean
  
}

# Plot of the forecasts based on rolling windows 

# plot for rolling window forecasts
plot(data_ts,xlab="Year",ylab="",main="DJIA Price Movements")
lines(ts(fcast_rolling,start = c(1990,ntrain+1),frequency = 365.25),
      lwd=0.1,col="blue")


# Accuracy Tests Mse
(mse_rollingh14 = mse(fcast_rollingh14,data_test))


# Accuracy Tests Rmse
(rmse_rollingh14 = rmse(fcast_rollingh14,data_test))


# Accuracy Tests Mae
(mae_rollingh14 = mae(fcast_rollingh14,data_test))
write.csv(fcast_rollingh14,"fcast_rollingh14.csv")