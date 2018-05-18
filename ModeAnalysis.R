data <- subset(data, Data >0)

#4 stations, 10462 pos obs
Uni <- subset(data, Station=='07008KRA' | Station=='07000BOL' | Station=='07006TLE' | Station=='04003NAV1')
#6 stations, 21306 pos obs
Bi <- subset(data, Station=='21088ODA' | Station=='22050KDA' | Station=='17009KSI' | Station=='07017HO' | Station=='01032SUN'|Station=='01018WEN')
#6 stations, 16280 pos obs
Semi <- subset(data, Station=='23001AXM' | Station=='23003TDI' | Station=='23022SAL' | Station=='23016ACC' | Station=='23002ADA' | Station=='23024TEM')

#4 stations, 10462 pos obs
Uni <- subset(dataNew.pos, Station=='07008KRA' | Station=='07000BOL' | Station=='07006TLE' | Station=='04003NAV1')
#6 stations, 21306 pos obs
Bi <- subset(dataNew.pos, Station=='21088ODA' | Station=='22050KDA' | Station=='17009KSI' | Station=='07017HO' | Station=='01032SUN'|Station=='01018WEN')
#6 stations, 16280 pos obs
Semi <- subset(dataNew.pos, Station=='23001AXM' | Station=='23003TDI' | Station=='23022SAL' | Station=='23016ACC' | Station=='23002ADA' | Station=='23024TEM')

Uni.norm <- subset(Uni, Data < 100 & Data > 1)
Bi.norm <- subset(Bi, Data < 100 & Data > 1)


boxplot(Data ~ month, data = Uni.norm, xlab = 'month', ylab = "rain in mm", main = "Uni modal")
boxplot(Data ~ month, data = Bi.norm, xlab = 'month', ylab = "rain in mm", main = "Bi modal")


Uni.jan <- subset(Uni, substr(Uni$Date, 5, 6)=='01')
Uni.feb <- subset(Uni, substr(Uni$Date, 5, 6)=='02')
Uni.mar <- subset(Uni, substr(Uni$Date, 5, 6)=='03')
Uni.apr <- subset(Uni, substr(Uni$Date, 5, 6)=='04')
Uni.may <- subset(Uni, substr(Uni$Date, 5, 6)=='05')
Uni.jun <- subset(Uni, substr(Uni$Date, 5, 6)=='06')
Uni.jul <- subset(Uni, substr(Uni$Date, 5, 6)=='07')
Uni.aug <- subset(Uni, substr(Uni$Date, 5, 6)=='08')
Uni.sep <- subset(Uni, substr(Uni$Date, 5, 6)=='09')
Uni.oct <- subset(Uni, substr(Uni$Date, 5, 6)=='10')
Uni.nov <- subset(Uni, substr(Uni$Date, 5, 6)=='11')
Uni.dec <- subset(Uni, substr(Uni$Date, 5, 6)=='12')

Unimean.jan <- sum(Uni.jan$Data)/120
Unimean.feb <- sum(Uni.feb$Data)/120
Unimean.mar <- sum(Uni.mar$Data)/120
Unimean.apr <- sum(Uni.apr$Data)/120
Unimean.may <- sum(Uni.may$Data)/120
Unimean.jun <- sum(Uni.jun$Data)/120
Unimean.jul <- sum(Uni.jul$Data)/120
Unimean.aug <- sum(Uni.aug$Data)/120
Unimean.sep <- sum(Uni.sep$Data)/120
Unimean.oct <- sum(Uni.oct$Data)/120
Unimean.nov <- sum(Uni.nov$Data)/119
Unimean.dec <- sum(Uni.dec$Data)/119

Uni.month.rain <- c(Unimean.jan, Unimean.feb, Unimean.mar, Unimean.apr, Unimean.may, Unimean.jun, Unimean.jul, Unimean.aug, Unimean.sep, Unimean.oct, Unimean.nov, Unimean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, Uni.month.rain, main = "monthly average, Uni")


Bi.jan <- subset(Bi, substr(Bi$Date, 5, 6)=='01')
Bi.feb <- subset(Bi, substr(Bi$Date, 5, 6)=='02')
Bi.mar <- subset(Bi, substr(Bi$Date, 5, 6)=='03')
Bi.apr <- subset(Bi, substr(Bi$Date, 5, 6)=='04')
Bi.may <- subset(Bi, substr(Bi$Date, 5, 6)=='05')
Bi.jun <- subset(Bi, substr(Bi$Date, 5, 6)=='06')
Bi.jul <- subset(Bi, substr(Bi$Date, 5, 6)=='07')
Bi.aug <- subset(Bi, substr(Bi$Date, 5, 6)=='08')
Bi.sep <- subset(Bi, substr(Bi$Date, 5, 6)=='09')
Bi.oct <- subset(Bi, substr(Bi$Date, 5, 6)=='10')
Bi.nov <- subset(Bi, substr(Bi$Date, 5, 6)=='11')
Bi.dec <- subset(Bi, substr(Bi$Date, 5, 6)=='12')

Bimean.jan <- sum(Bi.jan$Data)/360
Bimean.feb <- sum(Bi.feb$Data)/360
Bimean.mar <- sum(Bi.mar$Data)/360
Bimean.apr <- sum(Bi.apr$Data)/360
Bimean.may <- sum(Bi.may$Data)/359
Bimean.jun <- sum(Bi.jun$Data)/360
Bimean.jul <- sum(Bi.jul$Data)/360
Bimean.aug <- sum(Bi.aug$Data)/360
Bimean.sep <- sum(Bi.sep$Data)/358
Bimean.oct <- sum(Bi.oct$Data)/360
Bimean.nov <- sum(Bi.nov$Data)/360
Bimean.dec <- sum(Bi.dec$Data)/360

Bi.month.rain <- c(Bimean.jan, Bimean.feb, Bimean.mar, Bimean.apr, Bimean.may, Bimean.jun, Bimean.jul, Bimean.aug, Bimean.sep, Bimean.oct, Bimean.nov, Bimean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, Bi.month.rain, main = "monthly average, Bi")


Semi.jan <- subset(Semi, substr(Semi$Date, 5, 6)=='01')
Semi.feb <- subset(Semi, substr(Semi$Date, 5, 6)=='02')
Semi.mar <- subset(Semi, substr(Semi$Date, 5, 6)=='03')
Semi.apr <- subset(Semi, substr(Semi$Date, 5, 6)=='04')
Semi.may <- subset(Semi, substr(Semi$Date, 5, 6)=='05')
Semi.jun <- subset(Semi, substr(Semi$Date, 5, 6)=='06')
Semi.jul <- subset(Semi, substr(Semi$Date, 5, 6)=='07')
Semi.aug <- subset(Semi, substr(Semi$Date, 5, 6)=='08')
Semi.sep <- subset(Semi, substr(Semi$Date, 5, 6)=='09')
Semi.oct <- subset(Semi, substr(Semi$Date, 5, 6)=='10')
Semi.nov <- subset(Semi, substr(Semi$Date, 5, 6)=='11')
Semi.dec <- subset(Semi, substr(Semi$Date, 5, 6)=='12')

Semimean.jan <- sum(Semi.jan$Data)/360
Semimean.feb <- sum(Semi.feb$Data)/358
Semimean.mar <- sum(Semi.mar$Data)/359
Semimean.apr <- sum(Semi.apr$Data)/360
Semimean.may <- sum(Semi.may$Data)/359
Semimean.jun <- sum(Semi.jun$Data)/360
Semimean.jul <- sum(Semi.jul$Data)/359
Semimean.aug <- sum(Semi.aug$Data)/359
Semimean.sep <- sum(Semi.sep$Data)/359
Semimean.oct <- sum(Semi.oct$Data)/360
Semimean.nov <- sum(Semi.nov$Data)/359
Semimean.dec <- sum(Semi.dec$Data)/360

Semi.month.rain <- c(Semimean.jan, Semimean.feb, Semimean.mar, Semimean.apr, Semimean.may, Semimean.jun, Semimean.jul, Semimean.aug, Semimean.sep, Semimean.oct, Semimean.nov, Semimean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, Semi.month.rain, main = "monthly average, Semi")


#Histograms for all Uni
hist(Uni.jan$Data, freq = FALSE)
hist(Uni.feb$Data, freq = FALSE)
hist(Uni.mar$Data, freq = FALSE)
hist(Uni.apr$Data, freq = FALSE)
hist(Uni.may$Data, freq = FALSE)
hist(Uni.jun$Data, freq = FALSE)
hist(Uni.jul$Data, freq = FALSE)
hist(Uni.aug$Data, freq = FALSE)
hist(Uni.sep$Data, freq = FALSE)
hist(Uni.oct$Data, freq = FALSE)
hist(Uni.nov$Data, freq = FALSE)
hist(Uni.dec$Data, freq = FALSE)

Uni.mar.n <- subset(Uni.mar, Data < 60)
Uni.apr.n <- subset(Uni.apr, Data < 120)
Uni.sep.n <- subset(Uni.sep, Data < 120)
Uni.oct.n <- subset(Uni.oct, Data < 100)
Uni.jul.n <- subset(Uni.jul, Data < 120)

#Distribution fit per month
fit.gamma.unijan <- fitdist(Uni.jan$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.unifeb <- fitdist(Uni.feb$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.unimar <- fitdist(Uni.mar$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.unimar.n <- fitdist(Uni.mar.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.uniapr <- fitdist(Uni.apr$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.uniapr.n <- fitdist(Uni.apr.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.unimay <- fitdist(Uni.may$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.unijun <- fitdist(Uni.jun$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.unijul <- fitdist(Uni.jul.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.uniaug <- fitdist(Uni.aug$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.unisep <- fitdist(Uni.sep$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.unisep.n <- fitdist(Uni.sep.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.unioct <- fitdist(Uni.oct$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.unioct.n <- fitdist(Uni.oct.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.uninov <- fitdist(Uni.nov$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.unidec <- fitdist(Uni.dec$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))


plot(fit.gamma.unijan)
plot(fit.gamma.unifeb)
plot(fit.gamma.unimar.n)
plot(fit.gamma.uniapr.n)
plot(fit.gamma.unimay)
plot(fit.gamma.unijun)
plot(fit.gamma.unijul)
plot(fit.gamma.uniaug)
plot(fit.gamma.unisep.n)
plot(fit.gamma.unioct.n)
plot(fit.gamma.uninov)
plot(fit.gamma.unidec)

#Histograms for all Bi
hist(Bi.jan$Data, freq = FALSE)
hist(Bi.feb$Data, freq = FALSE)
hist(Bi.mar$Data, freq = FALSE)
hist(Bi.apr$Data, freq = FALSE)
hist(Bi.may$Data, freq = FALSE)
hist(Bi.jun$Data, freq = FALSE)
hist(Bi.jul$Data, freq = FALSE)
hist(Bi.aug$Data, freq = FALSE)
hist(Bi.sep$Data, freq = FALSE)
hist(Bi.oct$Data, freq = FALSE)
hist(Bi.nov$Data, freq = FALSE)
hist(Bi.dec$Data, freq = FALSE)

Bi.oct.n <- subset(Bi.oct, Data < 90)

#Distribution fit per month
fit.gamma.Bijan <- fitdist(Bi.jan$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Bifeb <- fitdist(Bi.feb$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Bimar.n <- fitdist(Bi.mar.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Biapr <- fitdist(Bi.apr$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Bimay <- fitdist(Bi.may$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Bijun.n <- fitdist(Bi.jun.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.wei.Bijul <- fitdist(Bi.jul.n$Data, "weibull",  method = "mle")
fit.wei.Biaug <- fitdist(Bi.aug.n$Data, "weibull",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Bisep.n <- fitdist(Bi.sep.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Bioct.n <- fitdist(Bi.oct.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Binov <- fitdist(Bi.nov$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Bidec <- fitdist(Bi.dec$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))


plot(fit.gamma.Bijan)
plot(fit.gamma.Bifeb)
plot(fit.gamma.Bimar.n)
plot(fit.gamma.Biapr)
plot(fit.gamma.Bimay)
plot(fit.gamma.Bijun.n)
plot(fit.wei.Bijul)
plot(fit.wei.Biaug)
plot(fit.gamma.Bisep.n)
plot(fit.gamma.Bioct.n)
plot(fit.gamma.Binov)
plot(fit.gamma.Bidec)


#Histograms for all Semi
hist(Semi.jan$Data, freq = FALSE)
hist(Semi.feb$Data, freq = FALSE)
hist(Semi.mar$Data, freq = FALSE)
hist(Semi.apr$Data, freq = FALSE)
hist(Semi.may$Data, freq = FALSE)
hist(Semi.jun$Data, freq = FALSE)
hist(Semi.jul$Data, freq = FALSE)
hist(Semi.aug$Data, freq = FALSE)
hist(Semi.sep$Data, freq = FALSE)
hist(Semi.oct$Data, freq = FALSE)
hist(Semi.nov$Data, freq = FALSE)
hist(Semi.dec$Data, freq = FALSE)

Semi.dec.n <- subset(Semi.oct, Data <80)

#Distribution fit per month
fit.gamma.Semijan.n <- fitdist(Semi.jan.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Semifeb.n <- fitdist(Semi.feb.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Semimar <- fitdist(Semi.mar$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Semiapr.n <- fitdist(Semi.apr.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Semimay.n <- fitdist(Semi.may.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Semijun.n <- fitdist(Semi.jun.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.wei.Semidec <- fitdist(Semi.dec.n$Data, "weibull",  method = "mle")

fit.gamma.Semijul.n <- fitdist(Semi.jul.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Semiaug <- fitdist(Semi.aug.n$Data, "lnorm",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Semisep <- fitdist(Semi.sep$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Semioct <- fitdist(Semi.oct.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Seminov <- fitdist(Semi.nov$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.Semidec <- fitdist(Semi.dec.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))


plot(fit.gamma.Semijan.n)
plot(fit.gamma.Semifeb.n)
plot(fit.gamma.Semimar)
plot(fit.gamma.Semiapr.n)
plot(fit.gamma.Semimay.n)
plot(fit.gamma.Semijun.n)
plot(fit.wei.Semidec)
plot(fit.gamma.Semijul.n)
plot(fit.gamma.Semiaug)
plot(fit.gamma.Semisep)
plot(fit.gamma.Semioct)
plot(fit.gamma.Seminov)
plot(fit.gamma.Semidec)
