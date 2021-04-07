# Load libraries
library(forecast) # rwf function
library(Metrics)  # mse function
library(tseries)  # adf.test



# Read data
mydata <- read.csv("mydata.csv",header = T)
head(mydata)

# convert to time series object
mydata_ts <- ts(subset(mydata,select=dji_pctchg),start=c(1990,1),
                frequency=365.25)

# Visualize series dji_pctchg
plot.ts(mydata_ts,xlab="Year",ylab="",main="DJIA Price Movements")


# Test stationarity
print(adf.test(mydata_ts))


# Split data into test and training set
ntrain <- 2495
mydata_train<-window(mydata_ts,start=c(1990,1),end=c(1990,ntrain))
mydata_test<-window(mydata_ts,start=c(1990,ntrain+1))
nh = length(mydata_test)

# Constructing random walk forecast
fit_rw <- rwf(mydata_train,nh)



# *****************************************************************************
# Constructing random walk forecast
# Rolling window

fcast_rolling <- rep(0,nh)
fcast_recursive <- rep(0,nh)
for (i in 0:nh-1) {
  
  # rolling window
  window_rolling<-window(mydata_ts,start=c(1990,1+i),end=c(1990,ntrain+i))

  # Forecasts random walk based on above windows
  fcast_rolling[i+1]<-rwf(window_rolling,h=1)$mean

}

# Plot of the forecasts based on rolling windows

# plot for rolling window forecasts
plot(mydata_ts,xlab="Year",ylab="",main="DJIA Price Movements")
lines(ts(fcast_rolling,start = c(1990,ntrain+1),frequency = 365.25),
      lwd=0.1,col="blue")


# Accuracy Tests Mse
(mse_rolling = mse(fcast_rolling,mydata_test))


# Accuracy Tests Rmse
(Rmse_rolling = rmse(fcast_rolling,mydata_test)) 


# Accuracy Tests Mae
(Mae_rolling = mae(fcast_rolling,mydata_test))





