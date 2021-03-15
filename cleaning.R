rm(list = ls())
setwd('/Users/kaijing/Documents/EC4304/Dow-Jones/Data')
library(readxl)
library(lubridate)
library(magrittr) 
library(dplyr)
library(tidyr)
library(plyr)
library(DataCombine)
library(zoo)
library(stringi)

dji = read.csv('^DJI.csv')
head(dji)
dji = dji[,c('Date','Close')]
# plot(dji)

pct = function(df,col){
  xx = c()
  for (i in 1:nrow(df)){
    res = (df[[col]][i+1]-df[[col]][i])/df[[col]][i]
    print(res)
    xx = c(xx,res)
  }
  return(xx)
}

dji_pctchg = pct(dji,'Close')
dji$dji_pctchg = dji_pctchg
dji$Close = NULL
final = dji
final$Date = as.Date(final$Date)
head(final)

clean = function(df,col,name){
  df = df[,c('Date',col)]
  res = pct(df,col)
  df[[name]] = res
  df[[col]] = NULL
  return(df)
}

snp = read.csv('^GSPC.csv')
snp = clean(snp,'Close','snp_pctchg')
final = inner_join(final,snp,by='Date')
head(final)

vix = read.csv('^VIX.csv')
vix = clean(vix,'Close','vix_pctchg')
#View(vix)
final = inner_join(final,vix,by='Date')
head(final)

libor = read.csv('3mth LIBOR not seasonally adjusted.csv',stringsAsFactors = FALSE)
colnames(libor)[1] = 'Date'
colnames(libor)[2] = 'Close'
head(libor)
libor$Close = as.numeric(libor$Close)

for (i in which(is.na(libor$Close))){ #random walk to fill NA
  libor$Close[i] = libor$Close[i-1]
}

libor = clean(libor,'Close','libor_pctchg')
#View(libor)
final = inner_join(final,libor,by='Date')
head(final)

dxynb = read.csv('DX-Y.NYB.csv',stringsAsFactors = F)
head(dxynb)
class(dxynb$Close)
dxynb$Close = as.numeric(dxynb$Close)
# plot(dxynb$Close)
sum(is.na(dxynb$Close))

for (i in which(is.na(dxynb$Close))){ #random walk to fill NA
  dxynb$Close[i] = dxynb$Close[i-1]
}
# plot(dxynb$Close)
dxynb = clean(dxynb,'Close','dxynb_pctchg')
head(dxynb)
#View(dxynb)
final = inner_join(final,dxynb,by='Date')
final$Date[nrow(final)]

ffr = read.csv('FFR not seasonally adjusted.csv')
colnames(ffr)[1] = 'Date'
colnames(ffr)[2] = 'Close'
head(ffr)
ffr = clean(ffr,'Close','ffr_pctchg')
#View(ffr)
final = inner_join(final,ffr,by='Date')
# plot(final$ffr_pctchg,type = 'l')

fill = function(df,col){
  for (i in 1:nrow(df)){
    if (is.na(df[[col]][i])){
      df[[col]][i] = df[[col]][i-1]
      print(i)
    } else{
      next
    }
  } 
  return(df)
}
ind_prod = read.csv('Industrial production (monthly) seasonally adjusted.csv')
colnames(ind_prod)[1] = 'Date'
colnames(ind_prod)[2] = 'Close'
ind_prod[nrow(ind_prod),]
head(ind_prod)
# ind_prod = clean(ind_prod,'Close','ind_pctchg')
str(ind_prod)
ind_prod$Date = as.Date(ind_prod$Date)
ind_date = seq(ymd(ind_prod$Date[1]),ymd(ind_prod$Date[nrow(ind_prod)]),by='1 day')
xx = data.frame(Date = ind_date) %>% left_join(ind_prod,by='Date')
#View(xx)
xx = fill(xx,'Close')

# plot(xx$Close)
ind_clean = clean(xx,'Close','ind_pctchg')

head(ind_clean)
plot(ind_clean)
ind_clean$Date %>% class()
final = inner_join(final,ind_clean,by='Date')

pce = read.csv('PCE (monthly) seasonally adjusted.csv',stringsAsFactors = F)
colnames(pce)[1] = 'Date'
colnames(pce)[2] = 'Close'
class(pce$Close)
pce$Date = as.Date(pce$Date)

pce_date = seq(ymd(pce$Date[1]),ymd(pce$Date[nrow(pce)]),by='1 day')
xx = data.frame(Date = pce_date) %>% left_join(pce,by='Date')
xx = fill(xx,'Close')
pce = clean(xx,'Close','pce_pctchg')
final = inner_join(final,pce,by='Date')

unrate = read.csv('UNRATE(monthly) seasonally adjusted.csv')
unrate$DATE = as.Date(unrate$DATE)
colnames(unrate)[1] = 'Date'
colnames(unrate)[2] = 'unrate'
unrate_date = seq(ymd(unrate$Date[1]),ymd(unrate$Date[nrow(unrate)]),by='1 day')
xx = data.frame(Date = unrate_date) %>% left_join(unrate,by='Date')
xx = fill(xx,'unrate')
#View(xx)
final = inner_join(final,xx,by='Date')

### INFLATION ISSUE
inf = read_xlsx('USInfationRateMonthly.xlsx')
#View(inf)
str(inf)
inf$Date = as.Date(inf$Date)
plot(inf$USINF)
View(inf)
unrate_date = seq(ymd(inf$Date[1]),ymd(inf$Date[nrow(inf)]),by='1 day')
xx = data.frame(Date = unrate_date) %>% left_join(inf[,c('Date','USINF')],by='Date')
#View(xx)
xx = fill(xx,'USINF')
#View(xx)
plot(xx$USINF)
inf = clean(xx,'USINF','inf_pctchg')
plot(inf$inf_pctchg)
final = inner_join(final,inf,by='Date')
plot(final$inf_pctchg)

pe = read.csv('PE_ratio.csv',stringsAsFactors = F)
str(pe$Date)
pe$Date = as.Date(pe$Date, format = "%d/%m/%y")
pe$Date[nrow(pe)]
colnames(pe)[1] = 'Date'
#View(pe)

unrate_date = seq(ymd(pe$Date[1]),ymd(pe$Date[nrow(pe)]),by='1 day')
xx = data.frame(Date = unrate_date) %>% left_join(pe,by='Date')
xx = fill(xx,'PE_Ratio')
xx$PE_Ratio = as.numeric(xx$PE_Ratio)
#View(xx)
pe_ratio = clean(xx,'PE_Ratio','peratio_pctchg')
final = inner_join(final,pe_ratio,by='Date')
final[is.na(final)] <- 0
#View(final)

div_yield = read.csv('Dividend_Yield.csv',stringsAsFactors = F)
str(div_yield$Date)
div_yield$Date = as.Date(div_yield$Date, format = "%d/%m/%y")
div_yield$Date[nrow(div_yield)]
colnames(div_yield)[1] = 'Date'
#View(div_yield)

unrate_date = seq(ymd(div_yield$Date[1]),ymd(div_yield$Date[nrow(div_yield)]),by='1 day')
xx = data.frame(Date = unrate_date) %>% left_join(div_yield,by='Date')
xx = fill(xx,'Dividend_Yield')
xx$Dividend_Yield = as.numeric(xx$Dividend_Yield)
#View(xx)
div_yield = clean(xx,'Dividend_Yield','divyield_pctchg')
final = inner_join(final,div_yield,by='Date')
final[is.na(final)] <- 0
#View(final)

priceToBook = read.csv('PriceToBookRatio.csv',stringsAsFactors = F)
str(priceToBook$Date)
priceToBook$Date = as.Date(priceToBook$Date, format = "%d/%m/%y")
priceToBook$Date[nrow(priceToBook)]
colnames(priceToBook)[1] = 'Date'
#View(priceToBook)

unrate_date = seq(ymd(priceToBook$Date[1]),ymd(priceToBook$Date[nrow(priceToBook)]),by='1 day')
xx = data.frame(Date = unrate_date) %>% left_join(priceToBook,by='Date')
xx$Price_To_Book.Ratio = as.numeric(xx$Price_To_Book.Ratio)
xx = fill(xx,'Price_To_Book.Ratio')
#View(xx)
priceToBook = clean(xx,'Price_To_Book.Ratio','priceToBook_pctchg')
final = inner_join(final,priceToBook,by='Date')
final[is.na(final)] <- 0
#View(final)

m1 = read.csv('M1 (weekly) seasonally adjusted.csv',stringsAsFactors = F)
str(m1)
m1$DATE = as.Date(m1$DATE)
m1$DATE[nrow(m1)]
colnames(m1)[1] = 'Date'
#View(m1)
# m1 = clean(m1,'M1','m1_pctchg')
 
unrate_date = seq(ymd(m1$Date[1]),ymd(m1$Date[nrow(m1)]),by='1 day')
xx = data.frame(Date = unrate_date) %>% left_join(m1,by='Date')
xx = fill(xx,'M1')
final = inner_join(final,xx,by='Date')
final = change(final, Var='M1',  type = "percent")
final = subset(final, select = -c(M1) )
final <- rename(final,c('M1_PChangeFrom-1'='m1_pctchg'))
#View(final)

m2 = read.csv('M2 (weekly) seasonally adjusted.csv',stringsAsFactors = F)
str(m2)
m2$DATE = as.Date(m2$DATE)
m2$Date[nrow(m2)]
colnames(m2)[1] = 'Date'

# m2 = clean(m2,'M2','m2_pctchg')
unrate_date = seq(ymd(m2$Date[1]),ymd(m2$Date[nrow(m2)]),by='1 day')
xx = data.frame(Date = unrate_date) %>% left_join(m2,by='Date')
xx = fill(xx,'M2')
final = inner_join(final,xx,by='Date')
final = change(final, Var='M2',  type = "percent")
# drop initial M2 column
final = subset(final, select = -c(M2))
final <- rename(final,c('M2_PChangeFrom-1'='m2_pctchg'))
final[is.na(final)] <- 0
#View(final)
# write.csv(final, 'final.csv')

tspread5 = read.csv('termspread3mth5yr.csv',stringsAsFactors = F)
tspread5$DATE = as.Date(tspread5[['DATE']],format = "%d/%m/%y")
colnames(tspread5)[1] = 'Date'
colnames(tspread5)[2] = 'Close'
plot(tspread5$Close)
tspread5$Close = as.numeric(tspread5$Close)
for (i in which(is.na(tspread5$Close))){ #random walk to fill NA
  tspread5$Close[i] = tspread5$Close[i-1]
}
tspread5 = clean(tspread5,'Close','t5_pctchg')
plot(tspread5$t5_pctchg)
final = inner_join(final,tspread5,by='Date')

tspread10 = read.csv('TermSpread10yr3mth.csv',stringsAsFactors = F)
tspread10$DATE = as.Date(tspread10[['DATE']],format = "%d/%m/%y")
colnames(tspread10)[1] = 'Date'
colnames(tspread10)[2] = 'Close'
str(tspread10)
tspread10$Close = as.numeric(tspread10$Close)
for (i in which(is.na(tspread10$Close))){ #random walk to fill NA
  tspread10$Close[i] = tspread10$Close[i-1]
}
plot(tspread10)
tspread10 = clean(tspread10,'Close','t10_pctchg')
final = inner_join(final,tspread10,by='Date')
View(final)
 write.csv(final,'final.csv')


