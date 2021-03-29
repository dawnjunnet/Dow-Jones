#Here we follow Medeiros et al. (2019) in defining four functions:

#One for forming forecasts using ridge model selected on BIC, which will be called
#on each iteration of the rolling window forecasting exercise.

#The second one for producing the series of h-step ridge forecasts using rolling window.

#The third one will take the resulting coefficients from LASSO, seek out the nonzero ones and rerun OLS with selected predictors to produce post-LASSO forecast.
#The fourth one will call on the third one for producing the series of h-step post-LASSO forecasts using rolling window.

#Inputs for the function:

#1) Data matrix Y: includes all variables

#2) indice - index for dependent variable: 1 for CPI inflation, 2 for PCE inflation

#3) lag - the forecast horizon

#4) alpha - the alpha parameter determining whether we use LASSO (alpha=1) or ElNet (alpha=0.5)

#5) IC - information criterion used to select lambda. Can set to: "bic", "aic" or "aicc"
runridge=function(Y,indice,lag,alpha,IC="bic", family){

  comp=princomp(scale(Y,scale=FALSE))
  Y2=cbind(Y,comp$scores[,1:4]) #augment predictors by the first 4 principal components
  aux=embed(Y2,7+lag) #create 2 lags + forecast horizon shift (=lag option)
  y=aux[,indice] #  Y variable aligned/adjusted for missing data due do lags
  X=aux[,-c(1:(ncol(Y2)*lag))]   # lags of Y (predictors) corresponding to forecast horizon

  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)] #retrieve the last  observations if one-step forecast
  }else{
    X.out=aux[,-c(1:(ncol(Y)*(lag-1)))] #delete first (h-1) columns of aux,
    X.out=tail(X.out,1)[1:ncol(X)] #last observations: y_T,y_t-1...y_t-h
  }

  #Here we use the glmnet wrapper written by the authors that does selection on IC:
  model=ic.glmnet(X,y,crit=IC,alpha = alpha, distType=family) #fit the ridge model selected on IC
  pred=predict(model,X.out) #generate the forecast (note c(X.out,0) gives the last observations on X's and the dummy (the zero))
    
  return(list("model"=model,"pred"=pred)) #return the estimated model and h-step forecast
}


#This function will repeatedly call the previous function in the rolling window h-step forecasting

#Inputs for the function:

#1) Data matrix Y: includes all variables

#2) nprev - number of out-of-sample observations (at the end of the sample)

#3) indice - index for dependent variable: 1 for CPI inflation, 2 for PCE inflation

#4) lag - the forecast horizon


#5) alpha - the alpha parameter determining whether we use LASSO (alpha=1) or ElNet (alpha=0.5)

#6) IC - information criterion used to select lambda. Can set to: "bic", "aic" or "aicc"


ridge.rolling.window=function(Y,nprev,indice,lag,alpha,IC="bic", family){
  # num coef = num lags +intercept+ncol(Y[,-indice])*numlags 
  save.coef=matrix(NA,nprev,21+ncol(Y[,-indice])*4) #blank matrix for coefficients at each iteration
  print(21+ncol(Y[,-indice])*4)
  save.pred=matrix(NA,nprev,1) #blank for forecasts
  model = NULL #create variable to store model
  for(i in nprev:1){ #NB: backwards FOR loop: going from 60 down to 1
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] #define the estimation window (first one: 1 to 420, then 2 to 421 etc.)
    ridge=runridge(Y.window,indice,lag,alpha,IC, family) #call the function to fit the RIDGE selected on IC and generate h-step forecast
    print(length(ridge$model$coefficients))
    save.coef[(1+nprev-i),]=ridge$model$coefficients #save estimated coefficients
    save.pred[(1+nprev-i),]=ridge$pred #save the forecast
    model = ridge$model
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  #Some helpful stuff:
  real=Y[,indice] #get actual values
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual

  mse=mean((tail(real,nprev)-save.pred)^2) #compute RMSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("mse"=mse,"mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors, "model"=model)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}