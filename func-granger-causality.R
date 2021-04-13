install.packages("lmtest")
library(lmtest)

setwd('/Users/kaijing/Documents/EC4304/Dow-Jones/Data')
df <- read.csv("final.csv")

Y = data.matrix(df)
#get dependent variable we want to predict
yy = df$dji_pctchg

#predictors
snp = df$snp_pctchg
vix = df$vix_pctchg
libor = df$libor_pctchg
exrate = df$exrate_pctchg
ffr = df$ffr_pctchg
ind = df$ind_pctchg
pce = df$pce_pctchg
unrate = df$unrate
m1 = df$m1_pctchg
m2 = df$m2_pctchg
t5 = df$t5_pctchg
t10 = df$t10_pctchg
div = df$div_pctchg
pe = df$pe_ratio_pctchg
ptb = df$price_book_pctchg

#test if our predictors cause y
grangertest(snp, yy, order = 7)
grangertest(vix, yy, order = 7)
grangertest(libor, yy, order = 7)
grangertest(exrate, yy, order = 7)
grangertest(ffr, yy, order = 7)
grangertest(ind, yy, order = 7)
grangertest(pce, yy, order = 7)
grangertest(unrate, yy, order = 7)
grangertest(m1, yy, order = 7)
grangertest(m2, yy, order = 7)
grangertest(t5, yy, order = 7)
grangertest(t10, yy, order = 7)
grangertest(div, yy, order = 7)
grangertest(pe, yy, order = 7)
grangertest(ptb, yy, order = 7)

#test if the reverse
grangertest(yy, snp, order = 7)
grangertest(yy, vix, order = 7)
grangertest(yy, libor, order = 7)
grangertest(yy, exrate, order = 7)
grangertest(yy, ffr, order = 7)
grangertest(yy, ind, order = 7)
grangertest(yy, pce, order = 7)
grangertest(yy, unrate, order = 7)
grangertest(yy, m1, order = 7)
grangertest(yy, m2, order = 7)
grangertest(yy, t5, order = 7)
grangertest(yy, t10, order = 7)
grangertest(yy, div, order = 7)
grangertest(yy, pe, order = 7)
grangertest(yy, ptb, order = 7)