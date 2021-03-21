rm(list = ls())
library(aTSA)

##########################################################
#read data 
#########################################################
setwd('/Users/kaijing/Documents/EC4304/Dow-Jones/Data')
df <- read.csv("final.csv")
#drop first two col of s/n and date respectively
df <- df[,-2]
#Y is a matrix of all X and Y variables
Y = data.matrix(df)
#get dependent variable we want to predict
yy = df$dji_pctchg
#test data 
nprev = 2230 #approx 1/3 of data for test set size
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
lasso1a=lasso.rolling.window(Y,nprev,1,1,alpha,IC="bic", "gaussian")
lasso3a=lasso.rolling.window(Y,nprev,1,3,alpha,IC="bic", "gaussian")
lasso6a=lasso.rolling.window(Y,nprev,1,6,alpha,IC="bic", "gaussian")
lasso12a=lasso.rolling.window(Y,nprev,1,12,alpha,IC="bic", "gaussian")

lassoa.mse1=lasso1a$errors[2]
lassoa.mse3=lasso3a$errors[2]
lassoa.mse6=lasso6a$errors[2]
lassoa.mse12=lasso12a$errors[2]

lassoa.mse1
lassoa.mse3
lassoa.mse6
lassoa.mse12

pols.lasso1a=pols.rolling.window(Y,nprev,1,1,lasso1a$coef)
pols.lasso3a=pols.rolling.window(Y,nprev,1,3,lasso3a$coef)
pols.lasso6a=pols.rolling.window(Y,nprev,1,6,lasso6a$coef)
pols.lasso12a=pols.rolling.window(Y,nprev,1,12,lasso12a$coef)

#Post-LASSO RMSE's:
plasso.mse1=pols.lasso1a$errors[2]
plasso.mse3=pols.lasso3a$errors[2]
plasso.mse6=pols.lasso6a$errors[2]
plasso.mse12=pols.lasso12a$errors[2]

plasso.mse1
plasso.mse3
plasso.mse6
plasso.mse12

pred1a.plasso = forecast(pols.lasso1a$model, tail(Y, 60), 1, 1, 2)
pred1a.plasso
pred3a.plasso = forecast(pols.lasso3a$model, tail(Y, 60), 1, 3, 2)
pred3a.plasso
pred6a.plasso = forecast(pols.lasso6a$model, tail(Y, 60), 1, 6, 2)
pred6a.plasso
pred12a.plasso = forecast(pols.lasso12a$model, tail(Y, 60), 1, 12, 2)
pred12a.plasso