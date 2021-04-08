rm(list=ls())
setwd('/Users/dawnstaana/Documents/NUS/Year 4/Sem 2/EC4304/Data')
library(multDM)

df = read.csv('final2.csv')
Y = data.matrix(df)
yy = df$dji_pctchg
nprev = 2495
oosy = tail(yy,nprev)

############################
# 1 step loss differential #
############################
mean1 = read.csv('mean1step_pred.csv',stringsAsFactors = F)
mean1 = mean1[2]

ridge1 = read.csv('h1pred(ridge).csv',stringsAsFactors = F)
ridge1 = ridge1[2]

lasso1 = read.csv('lasso1step_pred.csv',stringsAsFactors = F)
lasso1 = lasso1[2]

adl1 = read.csv('h1pred(ADL).csv',stringsAsFactors = F)
adl1 = adl1[2]

rw1 = read_csv('fcast_rollingh1.csv',col_names = F)
rw1 = as.data.frame(rw1)[2]

DM_same = function(f1,f2,y,num){
  xx = DM.test(f1,f2,y,loss.type = 'SE',h=num)
  if (xx$p.value > 0.05){
    xx = DM.test(f1,f2,y,loss.type = 'SE',h=num,H1='more')
    cat('f1 and f2 are not the same','\n')
    cat('Test whether f1 is better than f2')
  } 
  return(xx)
}

#DM.test
DM_same(mean1$V1,ridge1$V1,oosy,1)
DM_same(mean1$V1,lasso1$V1,oosy,1)
DM_same(mean1$V1,adl1$V1,oosy,1)
DM_same(mean1$V1,rw1$X2,oosy,1)

DM_same(ridge1$V1,lasso1$V1,oosy,1)
DM_same(ridge1$V1,adl1$V1,oosy,1)
DM_same(ridge1$V1,rw1$X2,oosy,1)

DM_same(lasso1$V1,adl1$V1,oosy,1)
DM_same(lasso1$V1,rw1$X2,oosy,1)

DM_same(adl1$V1,rw1$X2,oosy,1)

############################
# 7 step loss differential #
############################
mean7 = read.csv('mean7step_pred.csv',stringsAsFactors = F)
mean7 = mean7[2]

ridge7 = read.csv('h7pred(ridge).csv',stringsAsFactors = F)
ridge7 = ridge7[2]

lasso7 = read.csv('lasso7step_pred.csv',stringsAsFactors = F)
lasso7 = lasso7[2]

adl7 = read.csv('h7pred(ADL).csv',stringsAsFactors = F)
adl7 = adl7[2]

rw7 = read.csv('fcast_rolling_h7.csv',stringsAsFactors = F)
rw7 = rw7[2]

DM_same(mean7$V1,ridge7$V1,oosy,1)
DM_same(mean7$V1,lasso7$V1,oosy,1)
DM_same(mean7$V1,adl7$V1,oosy,1)
DM_same(mean7$V1,rw7$V1,oosy,1)

DM_same(ridge7$V1,lasso7$V1,oosy,1)
DM_same(ridge7$V1,adl7$V1,oosy,1)
DM_same(ridge7$V1,rw7$V1,oosy,1)

DM_same(lasso7$V1,adl7$V1,oosy,1)
DM_same(lasso7$V1,rw7$V1,oosy,1)

DM_same(adl7$V1,rw7$V1,oosy,1)

#############################
# 14 step loss differential #
#############################
mean14 = read.csv('mean14step_pred.csv',stringsAsFactors = F)
mean14 = mean14[2]

ridge14 = read.csv('h14pred(ridge).csv',stringsAsFactors = F)
ridge14 = ridge14[2]

lasso14 = read.csv('lasso14step_pred.csv',stringsAsFactors = F)
lasso14 = lasso14[2]

adl14 = read.csv('h14pred(ADL).csv',stringsAsFactors = F)
adl14 = adl14[2]

rw14 = read.csv('fcast_rolling_h14.csv',stringsAsFactors = F)
rw14 = rw14[2]

DM_same(mean14$V1,ridge14$V1,oosy,1)
DM_same(mean14$V1,lasso14$V1,oosy,1)
DM_same(mean14$V1,adl14$V1,oosy,1)
DM_same(mean14$V1,rw14$V1,oosy,1)

DM_same(ridge14$V1,lasso14$V1,oosy,1)
DM_same(ridge14$V1,adl14$V1,oosy,1)
DM_same(ridge14$V1,rw14$V1,oosy,1)

DM_same(lasso14$V1,adl14$V1,oosy,1)
DM_same(lasso14$V1,rw14$V1,oosy,1)

DM_same(adl14$V1,rw14$V1,oosy,1)



