install.packages('car')
library(car)

setwd('/Users/Kaijing/Documents/EC4304/Dow-Jones/predictions')

yy = df$dji_pctchg
nprev = 2495 #test date starts on 4/1/2010
oosy = tail(yy,nprev)

#####################
# 1 step window #
#####################

#test validation - not used just to check
npval = 1050
y_validation = tail(data.matrix(yy)[1:(nrow(df)-nprev),],npval)
train = Y[1:(nrow(Y) - nprev),]
h1val_adl = adl.rolling.window(train,npval,indice = 1,h = 1)
write.csv(h1val_adl$pred, 'adl_pred(validation).csv',row.names = F)
adl1_predval = read.csv("adl_pred(validation).csv")
colnames(adl1_predval) = NULL
adl1_predval = unlist(adl1_predval)

adl1_pred = read.csv("h1pred(ADL).csv")[2]
colnames(adl1_pred) = NULL
adl1_pred = unlist(adl1_pred)
adl1.mz = lm(oosy ~ adl1_pred) 
summary(adl1.mz)$coef
linearHypothesis(adl1.mz, c("(Intercept) = 0", "adl1_pred = 1")) 

ridge1_pred = read.csv("h1pred(ridge).csv")[2]
colnames(ridge1_pred) = NULL
ridge1_pred = unlist(ridge1_pred)
ridge1.mz = lm(oosy ~ ridge1_pred) 
summary(ridge1.mz)$coef
linearHypothesis(ridge1.mz, c("(Intercept) = 0", "ridge1_pred = 1")) 

lasso1_pred = read.csv("lasso1step_pred.csv")[2]
colnames(lasso1_pred) = NULL
lasso1_pred = unlist(lasso1_pred)
lasso1.mz = lm(oosy ~ lasso1_pred) 
summary(lasso1.mz)$coef
linearHypothesis(lasso1.mz, c("(Intercept) = 0", "lasso1_pred = 1")) 

combi1_pred = read.csv("forecast_combination_predh1.csv")
colnames(combi1_pred) = NULL
combi1_pred = unlist(combi1_pred)
combi1.mz = lm(oosy ~ combi1_pred) 
summary(combi1.mz)$coef
linearHypothesis(combi1.mz, c("(Intercept) = 0", "combi1_pred = 1")) 

#####################
# 7 step window #
#####################
adl7_pred = read.csv("h7pred(ADL).csv")[2]
colnames(adl7_pred) = NULL
adl7_pred = unlist(adl7_pred)
adl7.mz = lm(oosy ~ adl7_pred) 
summary(adl7.mz)$coef
linearHypothesis(adl7.mz, c("(Intercept) = 0", "adl7_pred = 1")) 

ridge7_pred = read.csv("h7pred(ridge).csv")[2]
colnames(ridge7_pred) = NULL
ridge7_pred = unlist(ridge7_pred)
ridge7.mz = lm(oosy ~ ridge7_pred) 
summary(ridge7.mz)$coef
linearHypothesis(ridge7.mz, c("(Intercept) = 0", "ridge7_pred = 1")) 

lasso7_pred = read.csv("lasso7step_pred.csv")[2]
colnames(lasso7_pred) = NULL
lasso7_pred = unlist(lasso7_pred)
lasso7.mz = lm(oosy ~ lasso7_pred) 
summary(lasso7.mz)$coef
linearHypothesis(lasso7.mz, c("(Intercept) = 0", "lasso7_pred = 1")) 

combi7_pred = read.csv("forecast_combination_predh7.csv")
colnames(combi7_pred) = NULL
combi7_pred = unlist(combi7_pred)
combi7.mz = lm(oosy ~ combi7_pred) 
summary(combi7.mz)$coef
linearHypothesis(combi7.mz, c("(Intercept) = 0", "combi7_pred = 1")) 

#####################
# 14 step window #
#####################
adl14_pred = read.csv("h14pred(ADL).csv")[2]
colnames(adl14_pred) = NULL
adl14_pred = unlist(adl14_pred)
adl14.mz = lm(oosy ~ adl14_pred) 
summary(adl14.mz)$coef
linearHypothesis(adl14.mz, c("(Intercept) = 0", "adl14_pred = 1")) 

ridge14_pred = read.csv("h14pred(ridge).csv")[2]
colnames(ridge14_pred) = NULL
ridge14_pred = unlist(ridge14_pred)
ridge14.mz = lm(oosy ~ ridge14_pred) 
summary(ridge14.mz)$coef
linearHypothesis(ridge14.mz, c("(Intercept) = 0", "ridge14_pred = 1")) 

lasso14_pred = read.csv("lasso14step_pred.csv")[2]
colnames(lasso14_pred) = NULL
lasso14_pred = unlist(lasso14_pred)
lasso14.mz = lm(oosy ~ lasso14_pred) 
summary(lasso14.mz)$coef
linearHypothesis(lasso14.mz, c("(Intercept) = 0", "lasso14_pred = 1")) 

combi14_pred = read.csv("forecast_combination_predh14.csv")
colnames(combi14_pred) = NULL
combi14_pred = unlist(combi14_pred)
combi14.mz = lm(oosy ~ combi14_pred) 
summary(combi14.mz)$coef
linearHypothesis(combi14.mz, c("(Intercept) = 0", "combi14_pred = 1")) 



