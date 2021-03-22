###############################################################################
## Data Visualisation
###############################################################################
#correlation plot
#res = cor(Y)
#round(res, 2)
#corrplot(res, order = "hclust", 
#         tl.col = "black", tl.srt = 45)

#Here we follow Medeiros et al. (2019) in defining four functions:

#One for forming forecasts using LASSO or Elastic Net model selected on BIC, which will be called
#on each iteration of the rolling window forecasting exercise.

#The second one for producing the series of h-step LASSO forecasts using rolling window.

#The third one will take the resulting coefficients from LASSO, seek out the nonzero ones and rerun OLS with selected predictors to produce post-LASSO forecast.
#The fourth one will call on the third one for producing the series of h-step post-LASSO forecasts using rolling window.

#Inputs for the function:

#1) Data matrix Y: includes all variables

#2) indice - index for dependent variable: 1 for Equity Premium of S&P500 Index, 2 for Direction of S&P500 Index

#3) lag - the forecast horizon

#4) alpha - the alpha parameter determining whether we use LASSO (alpha=1) or ElNet (alpha=0.5)

#5) IC - information criterion used to select lambda. Can set to: "bic", "aic" or "aicc"

#IMPORTANT: THIS LAG PASSED IN IS ACTUALLY THE FORECAST HORIZON!!
runlasso=function(Y,indice,lag,alpha=1,IC="bic", family){
  
  comp=princomp(scale(Y,scale=FALSE)) # compute principal components to add as predictors
  Y2=cbind(Y,comp$scores[,1:4]) #augment predictors by the first 4 principal components
  aux=embed(Y2,4+lag) #create 4 lags + forecast horizon shift (=lag option)
  y=aux[,indice] #  Y variable aligned/adjusted for missing data due to lags
  X=aux[,-c(1:(ncol(Y2)*lag))]  # lags of Y (predictors) corresponding to forecast horizon   
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)] #retrieve the last  observations if one-step forecast  
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))] #delete first (h-1) columns of aux,
    X.out=tail(X.out,1)[1:ncol(X)] #last observations: y_T,y_t-1...y_t-h
  }
  
  #Here we use the glmnet wrapper written by the authors that does selection on IC:
  model=ic.glmnet(X,y,crit=IC,alpha = alpha, family) #fit the LASSO/ElNet model selected on IC
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

#IMPORTANT: THIS LAG PASSED IN IS ACTUALLY THE FORECAST HORIZON!!
lasso.rolling.window=function(Y,nprev,indice=1,lag,alpha=1,IC="bic", family){
  
  save.coef=matrix(NA,nprev,21 + ncol(Y[,-indice])*4) #blank matrix for coefficients at each iteration
  print(21 + ncol(Y[,-indice])*4)
  save.pred=matrix(NA,nprev,1) #blank for forecasts
  model = NULL #create variable to store model
  for(i in nprev:1){ #NB: backwards FOR loop: going from (total sample size - nprev) down to 1
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] #define the train window
    lasso=runlasso(Y.window,indice,lag,alpha,IC, family) #call the function to fit the LASSO/ElNET selected on IC and generate h-step forecast
    print(length(lasso$model$coef))
    save.coef[(1+nprev-i),]=lasso$model$coef #save estimated coefficients
    save.pred[(1+nprev-i),]=lasso$pred #save the forecast
    model = lasso$model
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
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors, "model"=model)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}

## Function for doing naive post-LASSO - locate nozero coefficients in inputted LASSO results and rerun OLS ##

#Inputs for the function:

#1) Data matrix Y: includes all variables


#2) indice - index for dependent variable: 1 for CPI inflation, 2 for PCE inflation

#3) lag - the forecast horizon

#4) coef - LASSO coefficients from previous results

runpols=function(Y,indice,lag,coef){
  
  comp=princomp(scale(Y,scale=FALSE)) # compute principal components to add as predictors
  Y2=cbind(Y,comp$scores[,1:4]) #augment predictors by the first 4 principal components
  aux=embed(Y2,4+lag) #create 4 lags + forecast horizon shift (=lag option)
  y=aux[,indice] #  Y variable aligned/adjusted for missing data due do lags
  X=aux[,-c(1:(ncol(Y2)*lag))]   # lags of Y (predictors) corresponding to forecast horizon   
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]  #retrieve last observations if one-step forecast
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))] #delete first (h-1) columns of aux,
    X.out=tail(X.out,1)[1:ncol(X)] #last observations: y_T,y_T-1...y_T-h
  }
  
  respo=X[,which(coef[-1]!=0)] #variables selectd by LASSO; find nonzero coefficients in the supplied LASSO coefficient vector and keep corresponding X's
  if(length(respo)==0){ #if no nozero coefficients (full shrinkage)
    model=lm(y ~ 1) #regress on a constant only
  }else{
    model=lm(y ~ respo) #else, run OLS with selected predictors
  }
  
  X.out=c(X.out) #last observations
  coeff=coef(model) #vector of OLS coefficients
  coeff[is.na(coeff)]=0 #if NA set to 0
  pred=c(1,X.out[which(coef[-1]!=0)])%*%coeff #form prediction
  
  return(list("model"=model,"pred"=pred)) #save OLS model estimates and forecast
}


#This function will repeatedly call the post-LASSO function in the rolling window h-step forecasting

#Inputs for the function:

#1) Data matrix Y: includes all variables

#2) nprev - number of out-of-sample observations (at the end of the sample)

#3) indice - index for dependent variable: 1 for CPI inflation, 2 for PCE inflation

#4) lag - the forecast horizon

#5) coef - LASSO coefficients from previous results


pols.rolling.window=function(Y,nprev,indice=1,lag,coef){
  
  save.pred=matrix(NA,nprev,1) #blank for forecasts
  model = NULL
  for(i in nprev:1){#NB: backwards FOR loop: going from 180 down to 1
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] #define the estimation window (first one: 1 to 491, then 2 to 492 etc.)
    m=runpols(Y.window,indice,lag,coef[(1+nprev-i),]) #call the function to fit the post-LASSO model and generate h-step forecast
    save.pred[(1+nprev-i),]=m$pred #save the forecast
    model = m$model
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  #Some helpful stuff:
  real=Y[,indice] #get actual values
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2)) #compute RMSE
  mse=mean((tail(real,nprev)-save.pred)^2) #compute MSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("rmse"=rmse,"mse"=mse, "mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"errors"=errors, "model"=model)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}
