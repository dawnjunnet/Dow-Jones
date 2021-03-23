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

snp = read.csv('^GSPC.csv',stringsAsFactors = F)
str(snp)
snp$Date = as.Date(snp$Date)
snp = clean(snp,'Close','snp_pctchg')
head(snp)
final = inner_join(final,snp,by='Date')
head(final)

vix = read.csv('^VIX.csv',stringsAsFactors = F)
str(vix)
vix$Date = as.Date(vix$Date)
head(vix)
vix = clean(vix,'Close','vix_pctchg')
head(vix,15)
final = inner_join(final,vix,by='Date')
head(final)

libor = read.csv('3mth LIBOR not seasonally adjusted.csv',stringsAsFactors = FALSE)
str(libor)
colnames(libor)[1] = 'Date'
colnames(libor)[2] = 'Close'
head(libor)
libor$Date = as.Date(libor$Date)
libor$Close = as.numeric(libor$Close)
sum(is.na(libor))

libor$Close[is.na(libor$Close)] = 0
libor = clean(libor,'Close','libor_pctchg')
head(libor)
final = inner_join(final,libor,by='Date')
head(final,15)

exrate = read.csv('DX-Y.NYB.csv',stringsAsFactors = F)
head(exrate)
str(exrate)
exrate$Date = as.Date(exrate$Date)
exrate$Close = as.numeric(exrate$Close)
# plot(dxynb$Close)
sum(is.na(exrate$Close))

for (i in which(is.na(exrate$Close))) {
  exrate$Close[i] = exrate$Close[i-1]
}

# plot(dxynb$Close)
exrate = clean(exrate,'Close','exrate_pctchg')
plot(exrate$exrate_pctchg)
head(exrate,15)
final = inner_join(final,exrate,by='Date')
final$Date[nrow(final)]
head(final)

ffr = read.csv('FFR not seasonally adjusted.csv',stringsAsFactors = F)
str(ffr)
ffr$DATE = as.Date(ffr$DATE)
colnames(ffr)[1] = 'Date'
colnames(ffr)[2] = 'Close'
head(ffr)
ffr = clean(ffr,'Close','ffr_pctchg')
head(ffr,15)
final = inner_join(final,ffr,by='Date')

fill = function(df,col){
  date = seq(ymd(df$Date[1]),ymd(df$Date[nrow(df)]),by='1 day')
  xx = data.frame(Date = date) %>% left_join(df,by='Date')
  print(xx[[col]])
  for (i in 1:nrow(xx)){
    if (is.na(xx[[col]][i])){
      xx[[col]][i] = xx[[col]][i-1]
      print(i)
    } else{
      next
    }
  }
  return(xx)
}

#monthly data
ind_prod = read.csv('Industrial production (monthly) seasonally adjusted.csv',stringsAsFactors = F)
str(ind_prod)
ind_prod$DATE = as.Date(ind_prod$DATE)
colnames(ind_prod)[1] = 'Date'
colnames(ind_prod)[2] = 'Close'
head(ind_prod)
ind_prod = fill(ind_prod,'Close')
head(ind_prod,15)
ind_prod = clean(ind_prod,'Close','ind_pctchg')
head(ind_prod,15)
# plot(ind_clean)
final = inner_join(final,ind_prod,by='Date')
head(final,18)

#monthly data
pce = read.csv('PCE (monthly) seasonally adjusted.csv',stringsAsFactors = F)
str(pce)
pce$DATE = as.Date(pce$DATE)
colnames(pce)[1] = 'Date'
colnames(pce)[2] = 'Close'

pce$Date = as.Date(pce$Date)
pce = fill(pce,'Close')
pce = clean(pce,'Close','pce_pctchg')
head(pce,15)
final = inner_join(final,pce,by='Date')
head(final,15)

#monthly data
unrate = read.csv('UNRATE(monthly) seasonally adjusted.csv',stringsAsFactors = F)
str(unrate)
unrate$DATE = as.Date(unrate$DATE)
colnames(unrate)[1] = 'Date'
colnames(unrate)[2] = 'unrate'
unrate = fill(unrate,'unrate')
head(unrate,15)
final = inner_join(final,unrate,by='Date')
head(final,15)

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

#weekly data
m1 = read.csv('M1 (weekly) seasonally adjusted.csv',stringsAsFactors = F)
str(m1)
m1$DATE = as.Date(m1$DATE)
m1$DATE[nrow(m1)]
colnames(m1)[1] = 'Date'
m1 = clean(m1,'M1','m1_pctchg')
m1 = fill(m1,'m1_pctchg')
plot(m1$m1_pctchg)
head(m1,15)
final = inner_join(final,m1,by='Date')
head(final,15)

#weekly data
m2 = read.csv('M2 (weekly) seasonally adjusted.csv',stringsAsFactors = F)
str(m2)
m2$DATE = as.Date(m2$DATE)
colnames(m2)[1] = 'Date'
m2 = clean(m2,'M2','m2_pctchg')
m2 = fill(m2,'m2_pctchg')
final = inner_join(final,m2,by='Date')
plot(final$m2_pctchg)
# write.csv(final, 'final.csv')

tspread5 = read.csv('termspread3mth5yr.csv',stringsAsFactors = F)
tspread5$DATE = as.Date(tspread5[['DATE']],format = "%d/%m/%y")
colnames(tspread5)[1] = 'Date'
colnames(tspread5)[2] = 'Close'
plot(tspread5$Close)
tspread5$Close = as.numeric(tspread5$Close)
tspread5$Close[is.na(tspread5$Close)] = 0
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

div_yield = read.csv('Dividend_Yield.csv',stringsAsFactors = F)
str(div_yield)
div_yield$Date = as.Date(div_yield$Date,format = '%d/%m/%y')
colnames(div_yield)[2] = 'div_yield'
div_yield$div_yield = as.numeric(div_yield$div_yield)
sum(is.na(div_yield$div_yield))
plot(div_yield$div_yield)
div_yield$div_yield[is.na(div_yield$div_yield)] = 0
div_yield = clean(div_yield,'div_yield','div_pctchg')
colnames(final)
final = inner_join(final,div_yield,by='Date')

peratio = read.csv('PE_ratio.csv',stringsAsFactors = F)
str(peratio)
peratio$PE_Ratio = as.numeric(peratio$PE_Ratio)
peratio$Date = as.Date(peratio$Date,format = '%d/%m/%y')
sum(is.na(peratio$PE_Ratio))
colnames(peratio)[2] = 'pe_ratio'
peratio$pe_ratio[is.na(peratio$pe_ratio)] = 0
peratio = clean(peratio,'pe_ratio','pe_ratio_pctchg')
final = inner_join(final,peratio,by='Date')
head(final,17)

pricetobook = read.csv('PriceToBookRatio.csv',stringsAsFactors = F)
str(pricetobook)
pricetobook$Date = as.Date(pricetobook$Date,format = '%d/%m/%y')
colnames(pricetobook)[2] = 'price_to_book'
pricetobook$price_to_book = as.numeric(pricetobook$price_to_book)
sum(is.na(pricetobook$price_to_book))
pricetobook$price_to_book[is.na(pricetobook$price_to_book)] = 0
pricetobook = fill(pricetobook,'price_to_book')
pricetobook = clean(pricetobook,'price_to_book','price_book_pctchg')
plot(pricetobook$price_book_pctchg)
final = inner_join(final,pricetobook,by='Date')

# write.csv(final,'df_final2.csv')
