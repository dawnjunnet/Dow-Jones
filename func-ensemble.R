library(limSolve)

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


###############################################################################
## Granger-Ramanathan combinations (Ensemble Learning)
###############################################################################

y_test = tail(data.matrix(yy),nprev)
y_validation = tail(data.matrix(yy)[1:(nrow(df)-nprev),],nprev)

setwd('/Users/kaijing/Documents/EC4304/Dow-Jones/predictions')
ridge1 <- read.csv("h1pred(ridge).csv")
ridge1Pred = ridge1[2]
lasso1 <- read.csv("lasso1step_pred.csv")
lasso1Pred = lasso1[2]
adl1 <- read.csv("h1pred(ADL).csv")
adl1Pred = adl1[2]
rw1 <- read.csv("fcast_rollingh1.csv")
rw1Pred = rw1[2]

#GR weights, no constant, all restrictions in place
##1-step ahead forecast
fmat1=cbind(ridge1Pred, lasso1Pred, adl1Pred, rw1Pred)
nregressors = ncol(fmat1)

gru1=lsei(fmat1, y_test, f=rep(0,nregressors))
View(gru1) #Examine weights; this is in the order of ridge, lasso, adl, rw

#Combine the forecasts with nonzero weights:
gre.pred1a=gru1[1]*ridge1Pred+gru1[2]*lasso1Pred+gru1[3]*adl1Pred+gru1[4]*rw1Pred
#MSE 
gre1 = as.numeric(gre.pred1a$V1)
GRE.MSE1 = MSE(y_test,gre1)
GRE.MSE1 

##7-step ahead forecast
ridge7 <- read.csv("h7pred(ridge).csv")
ridge7Pred = ridge7[2]
lasso7 <- read.csv("lasso7step_pred.csv")
lasso7Pred = lasso7[2]
adl7 <- read.csv("h7pred(ADL).csv")
adl7Pred = adl7[2]
rw7 <- read.csv("fcast_rollingh7.csv")
rw7Pred = rw7[2]

fmat7=cbind(ridge7Pred, lasso7Pred, adl7Pred, rw7Pred)
nregressors = ncol(fmat7)
gru7=lsei(fmat7, y_test, f=rep(0,nregressors))
View(gru7) #Examine weights;

#Combine the forecasts with nonzero weights:
gre.pred7a=gru7[1]*ridge7+gru7[2]*lasso7+gru7[3]*adl7Pred+gru7[4]*rw7Pred
gre7 = as.numeric(gre.pred7a$V1)
GRE.MSE7 = MSE(y_test,gre7)

##14-step ahead forecast
ridge14 <- read.csv("h14pred(ridge).csv")
ridge14Pred = ridge14[2]
lasso14 <- read.csv("lasso14step_pred.csv")
lasso14Pred = lasso14[2]
adl14 <- read.csv("h14pred(ADL).csv")
adl14Pred = adl14[2]
rw14 <- read.csv("fcast_rollingh14.csv")
rw14Pred = rw14[2]

fmat14=cbind(ridge14Pred, lasso14Pred, adl14Pred, rw14Pred)
nregressors = ncol(fmat14)
gru14=lsei(fmat14, y_test, f=rep(0,nregressors))
View(gru14) #Examine weights; 

#Combine the forecasts with nonzero weights:
gre.pred14a=gru14[1]*ridge14+gru14[2]*lasso14+gru14[3]*adl14Pred+gru14[4]*rw14Pred
gre14 = as.numeric(gre.pred14a$V1)
GRE.MSE14 = MSE(y_test,gre14)