#Read data
data <- read.table('Ghana_daily.txt', header=TRUE)
dates <- as.Date(as.character(data$Date), format='%Y%m%d')

data.pos <- subset(data, Data>0)
data.norm <- subset(data, Data >= 1)
#exclude dependent and many missing data points
data.norm <- subset(data.norm, Station=='23001AXM' | Station=='21088ODA' | Station=='22050KDA' | Station=='17009KSI' |Station=='07017HO' | Station=='23003TDI' | Station=='01032SUN'| Station=='01018WEN'|Station=='07008KRA' | Station=='07000BOL' | Station=='23022SAL' | Station=='07006TLE' | Station=='04003NAV1' | Station=='23016ACC' | Station=='23002ADA' | Station=='23024TEM')

#Pick out date parts
dfdate <- data.frame(date=dates)
year=as.numeric (format(dates,"%Y"))
month=as.numeric (format(dates,"%m"))
day=as.numeric (format(dates,"%d"))

dataNew <- cbind(dfdate,year,month,day, data)
dataNew.norm <- subset(dataNew, Data >= 1)
#exclude dependent and many missing data points
dataNew.norm <- subset(dataNew.norm, Station=='23001AXM' | Station=='21088ODA' | Station=='22050KDA' | Station=='17009KSI' |Station=='07017HO' | Station=='23003TDI' | Station=='01032SUN'| Station=='01018WEN'|Station=='07008KRA' | Station=='07000BOL' | Station=='23022SAL' | Station=='07006TLE' | Station=='04003NAV1' | Station=='23016ACC' | Station=='23002ADA' | Station=='23024TEM')


#Extract all values 
unique(data$Station)
unique(data$Long)

#Transform dates
d.TLE <- as.Date(as.character(TLE$Date), format='%Y%m%d')

plot(KRA$Data~d.KRA)
hist(data.norm$Data, breaks = 40, freq = FALSE, main = "Hist all pos. data")
boxplot(data.pos$Data, main="Rainy days")

#Subsetting years and months
y2000 <-subset(data, substr(data$Date,1, 4)==2000)
d.y2000 <- as.Date(as.character(y2000$Date), format='%Y%m%d')
Apr <- subset(data, substr(data$Date,5,6)=='04')
d.Apr <- as.Date(as.character(Apr$Date), format='%Y%m%d')

#All stations
#ABE <-subset(dataNew, Station=='14067ABE')
 ACC <-subset(dataNew, Station=='23016ACC')
 ADA <-subset(dataNew, Station=='23002ADA')
#AKA <- subset(dataNew, Station=='15076AKA')
 ODA <- subset(dataNew, Station=='21088ODA')
 AXM <- subset(dataNew, Station=='23001AXM')
 BOL <- subset(dataNew, Station=='07000BOL')
 HO <- subset(dataNew, Station=='07017HO')
 KRA <- subset(dataNew, Station=='07008KRA')
 KDA <- subset(dataNew, Station=='22050KDA')
 KSI <- subset(dataNew, Station=='17009KSI')
 NAV1 <- subset(dataNew, Station=='04003NAV1')
 SAL <- subset(dataNew, Station=='23022SAL')
#BEK <- subset(dataNew, Station=='16015BEK')
 SUN <- subset(dataNew, Station=='01032SUN')
 TDI <- subset(dataNew, Station=='23003TDI')
 TEM <- subset(dataNew, Station=='23024TEM')
 TLE <- subset(dataNew, Station=='07006TLE')
#WA <- subset( dataNew, Station == '01013WA')
 WEN <- subset(dataNew, Station=='01018WEN')
#NAV <- subset(dataNew, Station=='04003NAV')

which(WEN$Data <0)
WEN <- subset(WEN, Data >0)

#Basic plots
hist(KRA.sep$Data)
boxplot(KRA.may$Data, data=KRA.may)
plot(density(KRA$Data))

#Fitting distributions
fit.weibull <- fitdist(KRA1.data, distr ="weibull", method= "mle", lower = c(0,0))

plot(x[order(x)])[1:20]