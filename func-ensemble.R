###############################################################################
## Granger-Ramanathan combinations (Ensemble Learning)
###############################################################################

y_test = tail(data.matrix(yy),60)
y_validation = tail(data.matrix(yy)[1:(nrow(df)-ntest),],60)
#GR weights, no constant, all restrictions in place
##1-step ahead forecast
fmat1=cbind(ridge1a$pred, lasso1a$pred, elastic1a$pred, ann1a$pred, boosted1a$pred, rf1a$pred)
nregressors = ncol(fmat1)

gru1=lsei(fmat1, y_test, c=rep(1,nregressors), d=1, e=diag(nregressors), f=rep(0,nregressors))
View(gru1) #Examine weights; only ridge has a positive coefficient of 1

#Combine the forecasts with nonzero weights:
gre.pred1a=gru1[1]*ridge1a$pred+gru1[2]*lasso1a$pred+gru1[3]*elastic1a$pred+gru1[6]*rf1a$pred
#MSE is the same as ridge regression
GRE.MSE1 = MSE(y_test,gre.pred1a)

##3-step ahead forecast
fmat3=cbind(ridge3a$pred, lasso3a$pred, elastic3a$pred, ann3a$pred, boosted3a$pred, rf3a$pred)
gru3=lsei(fmat3, y_test, c=rep(1,nregressors), d=1, e=diag(nregressors), f=rep(0,nregressors))
View(gru3) #Examine weights; only ridge has a positive coefficient of 1

#Combine the forecasts with nonzero weights:
gre.pred3a=gru3[1]*ridge3a$pred+gru3[2]*lasso3a$pred+gru3[3]*elastic3a$pred*gru3[4]*ann3a$pred+gru3[6]*rf3a$pred
GRE.MSE3 = MSE(y_test,gre.pred3a)

##6-step ahead forecast
fmat6=cbind(ridge6a$pred, lasso6a$pred, elastic6a$pred, ann6a$pred, boosted6a$pred, rf6a$pred)
nregressors = ncol(fmat6)
gru6=lsei(fmat6, y_test, c=rep(1,nregressors), d=1, e=diag(nregressors), f=rep(0,nregressors))
View(gru6) #Examine weights; only ridge has a positive coefficient of 1

#Combine the forecasts with nonzero weights:
gre.pred6a=gru6[1]*ridge6a$pred+gru6[2]*lasso6a$pred+gru6[3]*elastic6a$pred+gru6[4]*ann6a$pred+gru6[5]*boosted6a$pred
GRE.MSE6 = MSE(y_test,gre.pred6a)

##12-step ahead forecast
fmat12=cbind(ridge12a$pred, lasso12a$pred, elastic12a$pred, ann12a$pred, boosted12a$pred, rf12a$pred)
nregressors = ncol(fmat12)
gru12=lsei(fmat12, y_test, c=rep(1,nregressors), d=1, e=diag(nregressors), f=rep(0,nregressors))
View(gru12) #Examine weights; only ridge has a positive coefficient of 1

#Combine the forecasts with nonzero weights:
gre.pred12a=gru12[1]*ridge12a$pred+gru12[2]*lasso12a$pred+gru12[3]*elastic12a$pred+gru12[4]*ann12a$pred
GRE.MSE12=MSE(y_test,gre.pred12a)

#1-step ahead forecast
rlassocomb.fit = rlasso(y_validation~fmat1,  post=FALSE,intercept=FALSE)
#Form combination:
comblasso1=cbind(ridge1a$pred, lasso1a$pred, elastic1a$pred, ann1a$pred, boosted1a$pred, rf1a$pred)%*%rlassocomb.fit$coefficients
LGRE.MSE1 = MSE(y_test,comblasso1)

#3-step ahead forecast
rlassocomb.fit = rlasso(y_validation~fmat3,  post=FALSE,intercept=FALSE)
#Form combination:
comblasso3=cbind(ridge3a$pred, lasso3a$pred, elastic3a$pred, ann3a$pred, boosted3a$pred, rf3a$pred)%*%rlassocomb.fit$coefficients
LGRE.MSE3 = MSE(y_test,comblasso3)

#6-step ahead forecast
rlassocomb.fit = rlasso(y_validation~fmat6,  post=FALSE,intercept=FALSE)
#Form combination:
comblasso6=cbind(ridge6a$pred, lasso6a$pred, elastic6a$pred, ann6a$pred, boosted6a$pred, rf6a$pred)%*%rlassocomb.fit$coefficients
LGRE.MSE6 = MSE(y_test,comblasso6)

#12-step ahead forecast
rlassocomb.fit = rlasso(y_validation~fmat12,  post=FALSE,intercept=FALSE)
#Form combination:
comblasso12=cbind(ridge12a$pred, lasso12a$pred, elastic12a$pred, ann12a$pred, boosted12a$pred, rf12a$pred)%*%rlassocomb.fit$coefficients
LGRE.MSE12 = MSE(y_test,comblasso12)