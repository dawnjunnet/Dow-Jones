rm(list = ls())
library(aTSA)
require(sqldf)
library(sqldf)
library(glmnet)
library(HDeconometrics)

##########################################################
#read data
#########################################################
setwd('/Users/kaijing/Documents/EC4304/Dow-Jones/Data')
df <- read.csv("final.csv")
#drop first two col of s/n and date respectively
#remove the inflation var that was causing issue
df <- df[,-1]
#if there are non-finite datapoints, we just set it to NA
is.na(df) <- sapply(df, is.infinite)
#set all NA datapoints to 0
df[is.na(df)] <- 0
write.csv(df,'final.csv')
df <- read.csv("final.csv")
#check to make sure without_na dataframe should be have the same number of observations as initial df
without_na = df[complete.cases(df), ]

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

#replace with your own source path
setwd("/Users/kaijing/Documents/EC4304/Dow-Jones")
source("func-lasso.R")
###############################################################################
## Add on the rest of ur methods here!
###############################################################################

###############################################################################
## Lasso DO NOT RUN THIS (tentatively - work in progress)
###############################################################################
alpha=1 #set alpha=1 for LASSO

#Run forecasts for LASSO (BIC)
#The SP500R dependent variable is in the 1st position
#forecast horizon 1 day, 1 week, 2 week, 30 days
lasso1a=lasso.rolling.window(Y,nprev,1,1,alpha,IC="bic", "gaussian")
lasso1a$pred
write.csv(lasso1a$pred,'lasso1step_pred.csv')
lasso1a$model
lasso7a=lasso.rolling.window(Y,nprev,1,7,alpha,IC="bic", "gaussian")
lasso7a$pred
lasso7a$model
write.csv(lasso7a$pred,'lasso7step_pred.csv')
lasso14a=lasso.rolling.window(Y,nprev,1,14,alpha,IC="bic", "gaussian")
lasso14a$pred
lasso14a$model
write.csv(lasso14a$pred,'lasso14step_pred.csv')

lasso1a.mse1=lasso1a$errors[2]
write.csv(lasso1a$errors,'lasso1step_errors.csv')
lasso7a.mse7=lasso7a$errors[2]
write.csv(lasso7a$errors,'lasso7step_errors.csv')
lassoa.mse14=lasso14a$errors[2]
write.csv(lasso14a$errors,'lasso14step_errors.csv')

lassoa.mse1
lassoa.mse14

pols.lasso1a=pols.rolling.window(Y,nprev,1,7,lasso1a$coef)
pols.lasso14a=pols.rolling.window(Y,nprev,1,14,lasso3a$coef)
pols.lasso30a=pols.rolling.window(Y,nprev,1,30,lasso6a$coef)
#pols.lasso12a=pols.rolling.window(Y,nprev,1,12,lasso12a$coef)

#Post-LASSO RMSE's:
plasso.mse1=pols.lasso1a$errors[2]
plasso.mse14=pols.lasso14a$errors[2]
plasso.mse30=pols.lasso30a$errors[2]
#plasso.mse12=pols.lasso12a$errors[2]

plasso.mse1
plasso.mse14
plasso.mse30
#plasso.mse12

pred1a.plasso = forecast(pols.lasso1a$model, tail(Y, 2230), 1, 1, 2)
pred1a.plasso
pred14a.plasso = forecast(pols.lasso14a$model, tail(Y, 2230), 1, 14, 2)
pred14a.plasso
pred30a.plasso = forecast(pols.lasso30a$model, tail(Y, 2230), 1, 14, 2)
pred30a.plasso
#pred12a.plasso = forecast(pols.lasso12a$model, tail(Y, 60), 1, 12, 2)
#pred12a.plasso

#ADL 1 step ahead forecast
#1 step ahead forecast
# from my code Y is a df
cc = adl.rolling.window(Y,nprev,indice = 1,h=1,lag)
cc = adl.rolling.window(as.dataframe(Y),nprev,indice = 1,h=1,lag)
cc$pred
# write.csv(cc$pred,'adl1step_pred.csv')
cc$errors
