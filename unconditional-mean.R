runmean=function(Y,indice, horizon){
  
  aux=embed(Y,horizon) #forecast horizon shift (=lag option)
  y=aux[,indice]
  
  pred=mean(y)
  
  return(list("pred"=pred)) #return the estimated model and h-step forecast
}

mean.rolling.window=function(Y,nprev,indice=1,horizon){
  
  save.pred=matrix(NA,nprev,1) #blank for forecasts
  for(i in nprev:1){ #NB: backwards FOR loop: going from 60 down to 1
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] #define the train window
    mean = runmean(Y.window, indice, horizon)
    
    save.pred[(1+nprev-i),]=mean$pred #save the forecast
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  #Some helpful stuff:
  real=Y[,indice] #get actual values
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2)) #compute RMSE
  mse=mean((tail(real,nprev)-save.pred)^2) #compute MSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("rmse"=rmse,"mse"=mse,"mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"errors"=errors)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}
