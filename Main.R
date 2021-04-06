rm(list = ls())
library(aTSA)
require(sqldf)
library(sqldf)
library(glmnet)
library(HDeconometrics)
install.packages("ggplot2")
library("ggplot2")

##########################################################
#read data
#########################################################
setwd('/Users/kaijing/Documents/EC4304/Dow-Jones/Data')
df <- read.csv("final.csv")
#drop date col respectively
df <- df[,-1]
#if there are non-finite datapoints, we just set it to NA
#is.na(df) <- sapply(df, is.infinite)
#set all NA datapoints to 0
#df[is.na(df)] <- 0
#write.csv(df,'final.csv')
#df <- read.csv("final.csv")
#check to make sure without_na dataframe should be have the same number of observations as initial df
#without_na = df[complete.cases(df), ]

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

df$days <- seq.int(nrow(df))
ggplot(df, aes(x=days, y= dji_pctchg)) + geom_line() + ggtitle("DOWJONES")

#replace with your own source path
setwd("/Users/kaijing/Documents/EC4304/Dow-Jones")
source("func-lasso.R")
source("unconditional-mean.R")
source("func-ensemble.R")
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
lasso1a=lasso.rolling.window(Y,nprev,1,1,alpha,IC="aic", "gaussian")
lasso1a$pred
write.csv(lasso1a$pred,'lasso1step_pred.csv')
lasso1a$model
lasso7a=lasso.rolling.window(Y,nprev,1,7,alpha,IC="aic", "gaussian")
lasso7a$pred
lasso7a$model
write.csv(lasso7a$pred,'lasso7step_pred.csv')
lasso14a=lasso.rolling.window(Y,nprev,1,14,alpha,IC="aic", "gaussian")
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

#ADL forecasts
# 1 step ahead forecast (1 day)
h1 = adl.rolling.window(Y,nprev,indice = 1,h=1,lag)
# write.csv(h1$pred,'h1pred.csv')

# 7 step ahead forecasr (1 week)
h7 = adl.rolling.window(Y,nprev,indice = 1,h=7,lag)
# write.csv(h7$pred,'h7pred.csv')

# 14 step ahead forecast (2 weeks)
h14 = adl.rolling.window(Y,nprev,indice = 1,h=14,lag)
h14$errors
h14$optlag
# write.csv(h14$pred,'h14pred.csv')

#Ridge forecast
h1ridge = ridge.rolling.window(Y,nprev,indice = 1,h=1)

###############################################################################
## Rolling Unconditional Mean as the predictor
###############################################################################

mean1=mean.rolling.window(Y,nprev,1,1)
write.csv(mean1$pred,'mean1step_pred.csv')
write.csv(mean1$errors,'mean1step_errors.csv')

mean7=mean.rolling.window(Y,nprev,1,7)
write.csv(mean7$pred,'mean7step_pred.csv')
write.csv(mean7$errors,'mean7step_errors.csv')

mean14=mean.rolling.window(Y,nprev,1,14)
write.csv(mean14$pred,'mean14step_pred.csv')
write.csv(mean14$errors,'mean14step_errors.csv')

mean.mse1=mean1$errors[2]
mean.mse7=mean7$errors[2]
mean.mse14=mean14$errors[2]

mean.mse1
mean.mse7
mean.mse14