rm(list = ls())
setwd('/Users/dawnstaana/Documents/NUS/Year 4/Sem 2/EC4304/Data')
library(limSolve)
library(readr)
library(forecast) # rwf function
library(Metrics)  # mse function
library(HDeconometrics)

##########################################################
#standard helper funcs
#########################################################
#Auxiliary function to compute root MSE (same as MSE before, but with square root):
RMSE <- function(pred, truth){ #start and end body of the function by { } - same as a loop
  return(sqrt(mean((truth - pred)^2)))
} #end function with a return(output) statement. Here we can go straight to return because the object of interest is a simple function of inputs

MSE <- function(pred, truth){ #start and end body of the function by { } - same as a loop
  return(mean((truth - pred)^2))
} #end function with a return(output) statement. Here we can go straight to return because the object of interest is a simple function of inputs

MAE = function(pred,truth){
  return(mean(abs(truth-pred)))
}

###############################################################################
## Granger-Ramanathan combinations (Ensemble Learning)
###############################################################################
df = read.csv('final2.csv')
Y = data.matrix(df)
Y2 = data.matrix(df$dji_pctchg)
Y2 = data.matrix(Y2[1:(nrow(Y) - nprev),])
yy = df$dji_pctchg
nprev = 2495
oosy = tail(yy,nprev)

y_test = tail(data.matrix(yy),nprev)
y_validation = tail(data.matrix(yy)[1:(nrow(df)-nprev),],npval)

npval = 1050
train = Y[1:(nrow(Y) - nprev),]
#####################
# 1 step validation #
#####################
# RIDGE #
h1val = ridge.rolling.window(train,npval,indice = 1,h=1)
# write.csv(h1val$pred,'ridge_pred(validation).csv',row.names = F)

# LASSO #
h1val_lasso = lasso.rolling.window(train,npval,1,1,alpha,IC="aic", "gaussian")
# write.csv(h1val_lasso$pred,'lasso_pred(validation).csv',row.names = F)

# ADL #
h1val_adl = adl.rolling.window(train,npval,indice = 1,h = 1)
# write.csv(h1val_adl$pred, 'adl_pred(validation).csv',row.names = F)

# RW use Y2 for this #
h1val_rw = rw.rolling.window(Y2,nprev=npval,h=1)
# write.csv(h1val_rw$pred,'rw_pred(validation).csv',row.names = F)

#####################
# 7 step validation #
#####################
# RIDGE #
h7val = ridge.rolling.window(train,npval,indice = 1,h=7)
# write.csv(h7val$pred,'ridgeh7_pred(validation).csv',row.names = F)

# LASSO #
h7val_lasso = lasso.rolling.window(train,npval,1,7,alpha,IC="aic", "gaussian")
# write.csv(h7val_lasso$pred,'lassoh7_pred(validation).csv',row.names = F)

# ADL #
h7val_adl = adl.rolling.window(train,npval,indice = 1,h = 7)
# write.csv(h7val_adl$pred, 'adlh7_pred(validation).csv',row.names = F)

# RW use Y2 for this #
h7val_rw = rw.rolling.window(Y2,nprev=npval,h=7)
# write.csv(h7val_rw$pred,'rwh7_pred(validation).csv',row.names = F)

fmat7=cbind(ridge7Pred, lasso7Pred, adl7Pred, rw7Pred)
nregressors = ncol(fmat7)
gru7=lsei(fmat7, y_test, f=rep(0,nregressors))
# View(gru7) #Examine weights;

#####################
# 14 step validation #
#####################
# RIDGE #
h14val = ridge.rolling.window(train,npval,indice = 1,h=14)
# write.csv(h14val$pred,'ridgeh14_pred(validation).csv',row.names = F)

# LASSO #
h14val_lasso = lasso.rolling.window(train,npval,1,14,alpha,IC="aic", "gaussian")
# write.csv(h14val_lasso$pred,'lassoh14_pred(validation).csv',row.names = F)

# ADL #
h14val_adl = adl.rolling.window(train,npval,indice = 1,h = 14)
# write.csv(h14val_adl$pred, 'adlh14_pred(validation).csv',row.names = F)

# RW use Y2 for this #
h14val_rw = rw.rolling.window(Y2,nprev=npval,h=14)
# write.csv(h14val_rw$pred,'rwh14_pred(validation).csv',row.names = F)

fmat7=cbind(ridge7Pred, lasso7Pred, adl7Pred, rw7Pred)
nregressors = ncol(fmat7)
gru7=lsei(fmat7, y_test, f=rep(0,nregressors))
# View(gru7) #Examine weights;

#GR weights, no constant, all restrictions in place
##1-step ahead forecast
fmat1=cbind(h1val$pred, h1val_lasso$pred, h1val_adl$pred, h1val_rw$pred)
colnames(fmat1) = c('ridge','lasso','adl','rw')
nregressors = ncol(fmat1)

gru1=lsei(fmat1, y_validation, f=rep(0,nregressors))
# View(gru1) #Examine weights; this is in the order of ridge, lasso, adl, rw

#Combine the forecasts with nonzero weights:
gre.pred1a=gru1$X[1]*h1val$pred+gru1$X[2]*h1val_lasso$pred+gru1$X[3]*h1val_adl$pred+gru1$X[4]*h1val_rw$pred
#MSE 
# write.csv(as.data.frame(gre.pred1a$V1),'grweights1_pred.csv')
GRE.MSE1 = MSE(y_validation,as.numeric(gre.pred1a))
GRE.MSE1 
# write.csv(GRE.MSE1,'mse(gre)1step.csv',row.names = F)
#Combine the forecasts with nonzero weights:
gre.pred7a=gru7$X[1]*ridge7Pred+gru7$X[2]*lasso7Pred+gru7$X[3]*adl7Pred+gru7$X[4]*rw7Pred
# write.csv(as.data.frame(gre.pred7a$V1),'grweights7_pred.csv')
gre7 = as.numeric(gre.pred7a$V1)
GRE.MSE7 = MSE(y_test,gre7)
GRE.MSE7
# write.csv(GRE.MSE7,'mse(gre)7step.csv',row.names = F)

##14-step ahead forecast
ridge14 <- read.csv("h14pred(ridge).csv")
ridge14Pred = ridge14[2]
lasso14 <- read.csv("lasso14step_pred.csv")
lasso14Pred = lasso14[2]
adl14 <- read.csv("h14pred(ADL).csv")
adl14Pred = adl14[2]
rw14 <- read.csv("fcast_rollingh14.csv",stringsAsFactors = F)
rw14 = rw14[2]
RMSE(truth = oosy,rw14$V1)
rw14Pred = rw14

fmat14=cbind(ridge14Pred, lasso14Pred, adl14Pred, rw14Pred)
nregressors = ncol(fmat14)
gru14=lsei(fmat14, y_test, f=rep(0,nregressors))
# View(gru14) #Examine weights; 

#Combine the forecasts with nonzero weights:
gre.pred14a=gru14[1]*ridge14Pred+gru14[2]*lasso14Pred+gru14[3]*adl14Pred+gru14[4]*rw14Pred
# write.csv(as.data.frame(gre.pred14a$V1),'grweights14_pred.csv')
gre14 = as.numeric(gre.pred14a$V1)
GRE.MSE14 = MSE(y_test,gre14)
GRE.MSE14
# write.csv(GRE.MSE14,'mse(gre)14step.csv',row.names = F)

