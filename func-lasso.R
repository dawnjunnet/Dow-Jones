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
  aux=embed(Y2,7+lag) #create 30 lags + forecast horizon shift (=lag option)

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

  save.coef=matrix(NA,nprev,141) #blank matrix for coefficients at each iteration
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
  aux=embed(Y2,30+lag) #create 4 lags + forecast horizon shift (=lag option)
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

###################################
# extract the column names        #
# Y = df,                         #
# lag = number of lags considered #
# h = number of steps of forecast #
###################################
extract_name = function(Y,lag,h){
  names_aux = c(colnames(Y))

  for (i in 1:(h+lag-1)){
    for (j in colnames(Y)){
      names_aux = c(names_aux,paste(j,'_lag',i,sep = ''))
    }
  }
  return(names_aux)
}
###################################
# extract the column names        #
# Y = matrix of df                #
# lag = number of lags considered #
###################################
extract_test_name = function(Y,lag){
  name = c()

  for (i in 1:lag) {
    for (j in colnames(Y)) {
      name = c(name,paste(j,'_lag',i,sep = ''))
    }
  }
  return(name)
}
###################################
# running adl function            #
# Y = matrix of df                #
# indice = indice of Y variable   #
# lag = number of lags considered #
# h = number of steps of forecast #
###################################
runadl=function(Y,indice,h,lag){

  aux=embed(Y,h+lag) #create lags + forecast horizon shift (=lag option)
  colnames(aux) = extract_name(Y,lag,h) #get colnames
  y=aux[,indice] #  Y variable aligned/adjusted for missing data due to lags
  X=aux[,-c(1:(ncol(Y)*h))]  # lags of Y (predictors) corresponding to forecast horizon

  train = data.frame(cbind(y,X)) #combine y and X into a dataframe

  if(h==1){
    X.out=tail(aux,1)[1:ncol(X)] #retrieve the last  observations if one-step forecast
    X.out = as.data.frame(matrix(X.out,ncol = length(X.out))) #save test set into a df
  }else{
    X.out=aux[,-c(1:(ncol(Y)*(h-1)))] #delete first (h-1) columns of aux,
    X.out=tail(X.out,1)[1:ncol(X)] #last observations: y_T,y_t-1...y_t-h
    X.out = as.data.frame(matrix(X.out,ncol = length(X.out))) #save test set into a df
  }
  
  colnames(X.out) = extract_test_name(Y,lag) #get colnames of test set
  model = lm(y~.,data = train) #run adl model
  pred=predict(model,X.out) #generate the forecast (note c(X.out,0) gives the last observations on X's and the dummy (the zero))
  return(list("model"=model,"pred"=pred,"AIC"=AIC(model))) #return the estimated model and h-step forecast
}

###########################################
# running adl function (rolling window)   #
# Y = matrix of df                        #
# nprev = no of observation for test set  #
# indice = indice of Y variable           #
# lag = number of lags considered         #
# h = number of steps of forecast         #
###########################################
adl.rolling.window=function(Y,nprev,indice=1,h,lag){

  save.pred=matrix(NA,nprev,1) #blank for forecasts
  model = list() #create variable to store model
  for(i in nprev:1){ #NB: backwards FOR loop: going from 180 down to 1
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] #define the train window
    lst = data.frame() #save result of lags and AIC for each lags
    for(j in 1:7){ #run on 30 lags 
      adl=runadl(Y.window,indice,h,lag=j) #call the function to fit the LASSO/ElNET selected on IC and generate h-step forecast
      res = data.frame(lag=j,AIC=adl$AIC) #df of lag and AIC
      lst = rbind(lst,res) #rbind to main result
      print(paste(i,j))
    }
    opt = which.min(lst$AIC) #choose optimal lag based on lowest overall AIC
    lmopt =runadl(Y.window,indice,h,lag=opt) #call the function to fit the optimal lag for subset of train set
    save.pred[(1+nprev-i),]=lmopt$pred #save the forecast
    model$optlag = c(model$optlag,lst$lag[opt]) #save optimum lag chosen by AIC
    model$optAIC = c(model$optAIC,lst$AIC[opt]) #save optimum AIC
  }
  #Some helpful stuff:
  real=Y[,indice] #get actual values
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual

  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2)) #compute RMSE
  mse=mean((tail(real,nprev)-save.pred)^2) #compute MSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("rmse"=rmse,"mse"=mse,"mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"errors"=errors, "optlag"=model$optlag,
              "optAIC"=model$optAIC)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}

runridge=function(Y,indice,h,lag,alpha=0,IC="bic"){
  
  comp=princomp(scale(Y,scale=FALSE)) # compute principal components to add as predictors
  Y2=cbind(Y,comp$scores[,1:4]) #augment predictors by the first 4 principal components
  aux=embed(Y2,h+lag) #create 4 lags + forecast horizon shift (=lag option)
  colnames(aux) = extract_name(Y2,lag,h)
  y=aux[,indice] #  Y variable aligned/adjusted for missing data due to lags
  X=aux[,-c(1:(ncol(Y2)*h))]  # lags of Y (predictors) corresponding to forecast horizon   
  
  if(h==1){
    X.out=tail(aux,1)[1:ncol(X)] #retrieve the last  observations if one-step forecast  
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(h-1)))] #delete first (h-1) columns of aux,
    X.out=tail(X.out,1)[1:ncol(X)] #last observations: y_T,y_t-1...y_t-h
  }
  X.out = matrix(X.out,ncol = length(X.out))
  #Here we use the glmnet wrapper written by the authors that does selection on IC:
  model = ic.glmnet(X,y,crit = IC,alpha=0)
  pred = predict(model,X.out)
  
  return(list("model"=model,"pred"=pred,'BIC'=as.numeric(model$ic[1]))) #return the estimated model and h-step forecast
}

ridge.rolling.window=function(Y,nprev,indice=1,h){
  
  save.pred=matrix(NA,nprev,1) #blank for forecasts
  model = list() #create variable to store model
  for(i in nprev:1){#NB: backwards FOR loop: going from 180 down to 1
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] #define the estimation window (first one: 1 to 491, then 2 to 492 etc.)
    lst = data.frame() #save result of lags and AIC for each lags
    for(j in 1:7){ #run on 30 lags 
      m=runridge(Y.window,indice = 1,h=h,alpha = 0,IC='bic',lag = j) #call the function to fit the LASSO/ElNET selected on IC and generate h-step forecast
      res = data.frame(lag=j,BIC=as.numeric(m$BIC)) #df of lag and AIC
      lst = rbind(lst,res) #rbind to main result
      print(paste(i,j))
    }
    opt = which.min(lst$BIC) #choose optimal lag based on lowest overall AIC
    lmopt =runadl(Y.window,indice,h,lag=opt) #call the function to fit the optimal lag for subset of train set
    save.pred[(1+nprev-i),]=lmopt$pred #save the forecast
    model$optlag = c(model$optlag,lst$lag[opt]) #save optimum lag chosen by AIC
    model$optBIC = c(model$optBIC,lmopt$BIC)
  }
  #Some helpful stuff:
  real=Y[,indice] #get actual values
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2)) #compute RMSE
  mse=mean((tail(real,nprev)-save.pred)^2) #compute MSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("rmse"=rmse,"mse"=mse, "mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"errors"=errors, "optlag"=model$optlag,
              "optBIC"=model$optAIC)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}
