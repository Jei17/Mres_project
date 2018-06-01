#Pick one station and split up the seasons
x <- KRA

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
