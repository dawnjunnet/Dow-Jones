rm(list = ls())
setwd('/Users/dawnstaana/Documents/NUS/Year 4/Sem 2/EC4304/Data')
library(lsei)

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

df = read.csv('final2.csv')
yy = df$dji_pctchg
nprev = 2495
npval = 1050 #validation set so as to not make weights biased
oosy = tail(yy,nprev)

y_test = tail(data.matrix(yy),nprev)
y_validation = tail(data.matrix(yy)[1:(nrow(df)-nprev),],npval)

#######################################################
# Obtain all saved 1 step prediction (validation set) #
#######################################################
ridge1 = read.csv('ridge_pred(validation).csv',stringsAsFactors = F)
lasso1 = read.csv('lasso_pred(validation).csv',stringsAsFactors = F)
adl1 = read.csv('adl_pred(validation).csv',stringsAsFactors = F)
rw1 = read.csv('rw_pred(validation).csv',stringsAsFactors = F)

###############################################################################
## Granger-Ramanathan combinations (Ensemble Learning)
###############################################################################
# Create matrix of all prediction
fmat1 = cbind(ridge1,lasso1,adl1,rw1)
colnames(fmat1) = c('ridge','lasso','adl','rw')

# No constant sum to 1 and non negative
gr1 = lsei(fmat1, y_validation, c=rep(1,ncol(fmat1)), d=1, e=diag(ncol(fmat1)), f=rep(0,ncol(fmat1)))
which(gr1>0) #1 and 3 or ridge and adl
##################################
# Load ridge and adl predictions #
##################################
r1_pred = read.csv('h1pred(ridge).csv',stringsAsFactors = F)
adl1_pred = read.csv('h1pred(ADL).csv',stringsAsFactors = F)
combpred1 = gr1[1]*r1_pred[2] + gr1[3]*adl1_pred[2]
comb1_error = data.frame(RMSE = RMSE(as.matrix(combpred1),as.matrix(y_test)),
           MSE = MSE(as.matrix(combpred1),as.matrix(y_test)),
           MAE = MAE(as.matrix(combpred1),as.matrix(y_test)))
# write.csv(combpred1,'forecast_combination_predh1.csv',row.names = F)
# write.csv(comb1_error,'mse(gre)1step.csv',row.names = F)
plot(yy,type="l",main = 'G-R weights 1 step forecast',ylab = 'dji pct change')
lines(c(rep(NA,length(yy)-nprev),data.matrix(combpred1)),col="red")

#######################################################
# Obtain all saved 7 step prediction (validation set) #
#######################################################
ridge7 = read.csv('ridgeh7_pred(validation).csv',stringsAsFactors = F)
lasso7 = read.csv('lassoh7_pred(validation).csv',stringsAsFactors = F)
adl7 = read.csv('adlh7_pred(validation).csv',stringsAsFactors = F)
rw7 = read.csv('rwh7_pred(validation).csv',stringsAsFactors = F)

###############################################################################
## Granger-Ramanathan combinations (Ensemble Learning)
###############################################################################
# Create matrix of all prediction
fmat7 = cbind(ridge7,lasso7,adl7,rw7)
colnames(fmat7) = colnames(fmat1)

# No constant sum to 1 and non negative
gr7 = lsei(fmat7, y_validation, c=rep(1,ncol(fmat7)), d=1, e=diag(ncol(fmat7)), f=rep(0,ncol(fmat7)))
which(gr7>0) #1 2 and 3 or ridge lasso and adl
########################################
# Load ridge lasso and adl predictions #
########################################
r7_pred = read.csv('h7pred(ridge).csv',stringsAsFactors = F)
l7_pred = read.csv('lasso1step_pred.csv',stringsAsFactors = F)
adl7_pred = read.csv('h7pred(ADL).csv',stringsAsFactors = F)
combpred7 = gr7[1]*r7_pred[2] + gr7[2]*l7_pred[2] + gr7[3]*adl7_pred[2]
comb7_error = data.frame(RMSE = RMSE(as.matrix(combpred7),as.matrix(y_test)),
                         MSE = MSE(as.matrix(combpred7),as.matrix(y_test)),
                         MAE = MAE(as.matrix(combpred7),as.matrix(y_test)))
# write.csv(combpred7,'forecast_combination_predh7.csv',row.names = F)
# write.csv(comb7_error,'mse(gre)7step.csv',row.names = F)
plot(yy,type="l",main = 'G-R weights 7 step forecast',ylab = 'dji pct change')
lines(c(rep(NA,length(yy)-nprev),data.matrix(combpred7)),col="red")

#######################################################
# Obtain all saved 14 step prediction (validation set) #
#######################################################
ridge14 = read.csv('ridgeh14_pred(validation).csv',stringsAsFactors = F)
lasso14 = read.csv('lassoh14_pred(validation).csv',stringsAsFactors = F)
adl14 = read.csv('adlh14_pred(validation).csv',stringsAsFactors = F)
rw14 = read.csv('rwh14_pred(validation).csv',stringsAsFactors = F)

###############################################################################
## Granger-Ramanathan combinations (Ensemble Learning)
###############################################################################
# Create matrix of all prediction
fmat14 = cbind(ridge14,lasso14,adl14,rw14)
colnames(fmat14) = colnames(fmat1)

# No constant sum to 1 and non negative
gr14 = lsei(fmat14, y_validation, c=rep(1,ncol(fmat14)), d=1, e=diag(ncol(fmat14)), f=rep(0,ncol(fmat14)))
which(gr14>0) #1 and 3 or ridge and adl
##################################
# Load ridge and adl predictions #
##################################
r14_pred = read.csv('h14pred(ridge).csv',stringsAsFactors = F)
adl14_pred = read.csv('h14pred(ADL).csv',stringsAsFactors = F)
combpred14 = gr14[1]*r14_pred[2] + gr7[3]*adl14_pred[2]
comb14_error = data.frame(RMSE = RMSE(as.matrix(combpred14),as.matrix(y_test)),
                         MSE = MSE(as.matrix(combpred14),as.matrix(y_test)),
                         MAE = MAE(as.matrix(combpred14),as.matrix(y_test)))
# write.csv(combpred14,'forecast_combination_predh14.csv',row.names = F)
# write.csv(comb14_error,'mse(gre)14step.csv',row.names = F)
plot(yy,type="l",main = 'G-R weights 14 step forecast',ylab = 'dji pct change')
lines(c(rep(NA,length(yy)-nprev),data.matrix(combpred14)),col="red")


