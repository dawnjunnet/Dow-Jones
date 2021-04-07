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
ntrain <- 4197
data_train<-window(data_ts,start=c(1990,1),end=c(1990,ntrain))
data_test<-window(data_ts,start=c(1990,ntrain+1))
nh = length(data_test)


# *****************************************************************************
# Constructing random walk forecast
# Rolling windowfcast_rolling <- rep(0,nh)
for (i in 0:nh-1) {
  
  # rolling window
  window_rolling<-window(data_ts,start=c(1990,1+i),end=c(1990,ntrain+i))
  # rolling window
  window_rolling<-window(data_ts,start=c(1990,1+i),end=c(1990,ntrain+i))
  
  
  # Forecasts random walk based on above windows
  fcast_rolling[i+1]<-rwf(window_rolling,h=7)$mean7
  
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

