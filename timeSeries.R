#Pick one station and split up the seasons
x <- ADA

DJF <- subset(x, month==1| month==2|month==12)
MAM <- subset(x, month==3|month==4 |month==5)
JJA <- subset(x, month==6|month==7|month==8)
SON <- subset(x, month==9|month==10|month==11)

#transform into timeseries
x.ts <- ts(x$Data, frequency = 365.25, start=c(1983,1,1))
DJF.ts <- ts(DJF$Data,frequency = 90.25, start=1983)
MAM.ts <- ts(MAM$Data,frequency = 91, start=1983)
JJA.ts <- ts(JJA$Data,frequency = 92, start=1983)
SON.ts <- ts(SON$Data,frequency = 91, start=1983)

#decompose and test for stationarity
decompX <- stl(x.ts, s.window = "periodic")
plot(decompX)
adf.test(x.ts)

y <- JJA.ts

decomp <- stl(y, s.window = 91)
plot(decomp)
adf.test(y)

#Mann-Kendall on yearly data
ano = seq(1983,2012)

agg.year <- numeric(30)
for (i in 1:30) {
  k <- subset(DJF, year==ano[i])
  agg.year[i] <- sum(k$Data)
 }

MannKendall(agg.year)

#seasonal MK
agg.mon <- numeric(360)
for (i in 1:30) {
  k <- subset(x, year==ano[i])
  
  k.jan <- subset(k, substr(k$Date, 5, 6)=='01')
  k.feb <- subset(k, substr(k$Date, 5, 6)=='02')
  k.mar <- subset(k, substr(k$Date, 5, 6)=='03')
  k.apr <- subset(k, substr(k$Date, 5, 6)=='04')
  k.may <- subset(k, substr(k$Date, 5, 6)=='05')
  k.jun <- subset(k, substr(k$Date, 5, 6)=='06')
  k.jul <- subset(k, substr(k$Date, 5, 6)=='07')
  k.aug <- subset(k, substr(k$Date, 5, 6)=='08')
  k.sep <- subset(k, substr(k$Date, 5, 6)=='09')
  k.oct <- subset(k, substr(k$Date, 5, 6)=='10')
  k.nov <- subset(k, substr(k$Date, 5, 6)=='11')
  k.dec <- subset(k, substr(k$Date, 5, 6)=='12')
  
  agg.mon[12*(i-1)+1] <- sum(k.jan$Data)
  agg.mon[12*(i-1) +2] <- sum(k.feb$Data)
  agg.mon[12*(i-1)+3] <- sum(k.mar$Data)
  agg.mon[12*(i-1) +4] <- sum(k.apr$Data)
  agg.mon[12*(i-1)+5] <- sum(k.may$Data)
  agg.mon[12*(i-1)+6] <- sum(k.jun$Data)
  agg.mon[12*(i-1)+7] <- sum(k.jul$Data)
  agg.mon[12*(i-1)+8] <- sum(k.aug$Data)
  agg.mon[12*(i-1)+9] <- sum(k.sep$Data)
  agg.mon[12*(i-1)+10]<- sum(k.oct$Data)
  agg.mon[12*(i-1)+11] <- sum(k.nov$Data)
  agg.mon[12*(i-1)+12] <- sum(k.dec$Data)
  
}
agg.mon.ts <- ts(agg.mon)

SeasonalMannKendall(agg.mon.ts)

#Sen's slope
require{trend}
sens.slope(y)

#Autocorrelation among extremes
u <- SON$Data
u.pos <- sort(subset(u, u>=1), decreasing = TRUE)
t <- u.pos[floor(length(u.pos)*0.3)]
u.ext <- subset(u, u >= t)

acf(u.ext)
