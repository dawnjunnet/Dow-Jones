rm(list = ls())
setwd('/Users/dawnstaana/Documents/NUS/Year 4/Sem 2/EC4304/Data')
library(readxl)

dji = read.csv('^DJI.csv')
head(dji)
dji = dji[,c('Date','Close')]
# plot(dji)

pct = function(df,col){
  xx = c()
  for (i in 1:nrow(df)){
    res = (df[[col]][i+1]-df[[col]][i])/df[[col]][i]
    print(i)
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
View(vix)
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
View(libor)
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
View(dxynb)
final = inner_join(final,dxynb,by='Date')
final$Date[nrow(final)]

ffr = read.csv('FFR not seasonally adjusted.csv')
colnames(ffr)[1] = 'Date'
colnames(ffr)[2] = 'Close'
head(ffr)
ffr = clean(ffr,'Close','ffr_pctchg')
View(ffr)
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
View(xx)
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
View(xx)
final = inner_join(final,xx,by='Date')

### INFLATION ISSUE
inf = read_xlsx('USInfationRateMonthly.xlsx')
View(inf)
str(inf)
inf$Date = as.Date(inf$Date)
plot(inf$USINF)
View(inf)
unrate_date = seq(ymd(inf$Date[1]),ymd(inf$Date[nrow(inf)]),by='1 day')
xx = data.frame(Date = unrate_date) %>% left_join(inf[,c('Date','USINF')],by='Date')
View(xx)
xx = fill(xx,'USINF')
View(xx)
plot(xx$USINF)
inf = clean(xx,'USINF','inf_pctchg')
plot(inf$inf_pctchg)
final = inner_join(final,inf,by='Date')
plot(final$inf_pctchg)

m1 = read.csv('M1 (weekly) seasonally adjusted.csv',stringsAsFactors = F)
str(m1)
m1$DATE = as.Date(m1$DATE)
m1$DATE[nrow(m1)]
colnames(m1)[1] = 'Date'
View(m1)
# m1 = clean(m1,'M1','m1_pctchg')
unrate_date = seq(ymd(m1$Date[1]),ymd(m1$Date[nrow(m1)]),by='1 day')
xx = data.frame(Date = unrate_date) %>% left_join(m1,by='Date')
View(xx)
m1 = fill(xx,'M1')
m1 = clean(xx,'M1','m1_pctchg')
plot(xx$m1_pctchg)
final$m1_pctchg = NULL
final = inner_join(final,xx,by='Date')
plot(final$m1_pctchg)

m2 = read.csv('M2 (weekly) seasonally adjusted.csv',stringsAsFactors = F)
str(m2)
m2$DATE = as.Date(m2$DATE)
colnames(m2)[1] = 'Date'
m2$Date[nrow(m2)]
# m2 = clean(m2,'M2','m2_pctchg')
unrate_date = seq(ymd(m2$Date[1]),ymd(m2$Date[nrow(m2)]),by='1 day')
xx = data.frame(Date = unrate_date) %>% left_join(m2,by='Date')
xx = fill(xx,'M2')
m2 = clean(xx,'M2','m2_pctchg')
final$m2_pctchg = NULL
final = inner_join(final,m2,by='Date')
plot(final$m2_pctchg)
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
# write.csv(final,'final.csv')


