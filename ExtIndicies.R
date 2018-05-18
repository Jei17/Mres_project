ano = seq(1983,2012)

#annual total precipitation (PRCPTOT)
annual.prec <- numeric(30)
for (i in 1:30) {
  x <- subset(dataNew.norm, year==ano[i])
  annual.prec[i] <- sum(x$Data)/16
}

#annual total days >= 1mm (R1mm)
annual.rain.d <- numeric(30)
for (i in 1:30) {
  b <- subset(dataNew.norm, year==ano[i])
  annual.rain.d[i] <- length(b$Data)/16
}

#annual total days >= 10mm (R10mm)
data.New10 <- subset(dataNew.norm, Data >= 10)
annual.rain.10 <- numeric(30)
for (i in 1:30) {
  z <- subset(data.New10, year==ano[i])
  annual.rain.10[i] <- length(z$Data)/16
}

#proportion of days
ar10.prop <- numeric(30)
for (i in 1:30){
  ar10.prop[i] <- annual.rain.10[i]/annual.rain.d[i]
}
ar10.prop.ts <- ts(ar10.prop, start=1983)

ar20.prop <- numeric(30)
for (i in 1:30){
  ar20.prop[i] <- annual.rain.20[i]/annual.rain.d[i]
}
ar20.prop.ts <- ts(ar20.prop, start=1983)


#annual total days >= 20mm (R20mm)
data.New20 <- subset(dataNew.norm, Data >= 20)
annual.rain.20 <- numeric(30)
for (i in 1:30) {
  a <- subset(data.New20, year==ano[i])
  annual.rain.20[i] <- length(a$Data)/16
}
ar.20.ts <- ts(annual.rain.20, start=1983)

plot(ano, annual.rain.10, type="n", main = "Total number of days >= 20 mm rain", xlab = "year", ylab = "Days")
abline(h = mean(annual.rain.20), col="red")
lines(ano, annual.rain.20, type="s")

# number of days rain > R95 in 84-93
y8493 <- subset(dataNew.norm, substr(dataNew.norm$Date, 1,4)==1993|substr(dataNew.norm$Date, 1,4)==1984|substr(dataNew.norm$Date, 1,4)==1985|substr(dataNew.norm$Date, 1,4)==1986|substr(dataNew.norm$Date, 1,4)==1987|substr(dataNew.norm$Date, 1,4)==1988|substr(dataNew.norm$Date, 1,4)==1989|substr(dataNew.norm$Date, 1,4)==1990|substr(dataNew.norm$Date, 1,4)==1991|substr(dataNew.norm$Date, 1,4)==1992)
quantile(y8493$Data, probs= c(0.95, 0.99), type=5)

data.New95 <- subset(dataNew.norm, Data > 47.2)
rain.95 <- numeric(30)
for (i in 1:30) {
  d <- subset(data.New95, year==ano[i])
  rain.95[i] <- sum(d$Data)/16
}

ext.rain.ratio95 <- numeric(30)
for (i in 1:30) {
  t <- subset(dataNew.norm, year==ano[i])
  quan <- quantile(t$Data, probs=0.95, type=5)
  ext.rain.ratio95[i] <- quan/annual.prec[i]
}
ext.rain.ratio95.ts <- ts(ext.rain.ratio95, start=1983)

data.New99 <- subset(dataNew.norm, Data > 84)
rain.99 <- numeric(30)
for (i in 1:30) {
  f <- subset(data.New99, year==ano[i])
  rain.99[i] <- sum(f$Data)/16
}

ext.rain.ratio99 <- numeric(30)
for (i in 1:30) {
  t <- subset(dataNew.norm, year==ano[i])
  quan <- quantile(t$Data, probs=0.99, type=5)
  ext.rain.ratio99[i] <- quan/annual.prec[i]
}
ext.rain.ratio99.ts <- ts(ext.rain.ratio99, start = 1983)


plot(ano, rain.95, type="n", main = "R95, base 1984-1983", xlab = "year", ylab = "Rain in mm")
#abline(h = , col="red")
lines(ano, rain.95, type="s")

#Modes

#4 stations, 10462 pos obs
Uni <- subset(dataNew.norm, Station=='07008KRA' | Station=='07000BOL' | Station=='07006TLE' | Station=='04003NAV1')
#6 stations, 21306 pos obs
Bi <- subset(dataNew.norm, Station=='21088ODA' | Station=='22050KDA' | Station=='17009KSI' | Station=='07017HO' | Station=='01032SUN'|Station=='01018WEN')
#6 stations, 16280 pos obs
Semi <- subset(dataNew.norm, Station=='23001AXM' | Station=='23003TDI' | Station=='23022SAL' | Station=='23016ACC' | Station=='23002ADA' | Station=='23024TEM')

#annual total precipitation (PRCPTOT)
annual.prec.u <- numeric(30)
for (i in 1:30) {
  x <- subset(Uni, year==ano[i])
  annual.prec.u[i] <- sum(x$Data)/4
}
annual.prec.uts <- ts(annual.prec.u, start=1983)

annual.prec.b <- numeric(30)
for (i in 1:30) {
  x <- subset(Bi, year==ano[i])
  annual.prec.b[i] <- sum(x$Data)/6
}
annual.prec.bts <- ts(annual.prec.b, start=1983)

annual.prec.s <- numeric(30)
for (i in 1:30) {
  x <- subset(Semi, year==ano[i])
  annual.prec.s[i] <- sum(x$Data)/6
}
annual.prec.sts <- ts(annual.prec.s, start=1983)

plot(ano ,annual.prec.b, ylim=c(400, 1500), type="n", main = "Annual rainfall (>= 1 mm)", xlab = "year", ylab = "Days")
abline(h = mean(annual.prec.u), col="darkred")
lines(ano, annual.prec.u, type="s", col="red")
abline(h = mean(annual.prec.b), col="darkblue")
lines(ano, annual.prec.b, type="s", col="blue")
abline(h = mean(annual.prec.s), col="darkgreen")
lines(ano, annual.prec.s, type="s", col="green")
legend(locator(1),legend= c("Uni", "Bi", "Semi"), col = c("red", "blue", "green"), lty=1, cex=0.7)


#annual total days >= 1mm (R1mm)
annual.precd.u <- numeric(30)
for (i in 1:30) {
  x <- subset(Uni, year==ano[i])
  annual.precd.u[i] <- length(x$Data)/4
}
annual.precd.uts <- ts(annual.precd.u, start=1983)

annual.precd.b <- numeric(30)
for (i in 1:30) {
  x <- subset(Bi, year==ano[i])
  annual.precd.b[i] <- length(x$Data)/6
}
annual.precd.bts <- ts(annual.precd.b, start=1983)

annual.precd.s <- numeric(30)
for (i in 1:30) {
  x <- subset(Semi, year==ano[i])
  annual.precd.s[i] <- length(x$Data)/6
}
annual.precd.sts <- ts(annual.precd.s, start=1983)

plot(ano ,annual.precd.b,ylim=c(40,110), type="n", main = "Annual total days of rain (>= 1 mm)", xlab = "year", ylab = "Days")
abline(h = mean(annual.precd.u), col="darkred")
lines(ano, annual.precd.u, type="s", col="red")
abline(h = mean(annual.precd.b), col="darkblue")
lines(ano, annual.precd.b, type="s", col="blue")
abline(h = mean(annual.precd.s), col="darkgreen")
lines(ano, annual.precd.s, type="s", col="green")
legend(locator(1),legend= c("Uni", "Bi", "Semi"), col = c("red", "blue", "green"), lty=1, cex=0.7)


#annual total days >= 10mm (R10mm)
Uni10 <- subset(Uni, Data >= 10)
annual.prec10.u <- numeric(30)
for (i in 1:30) {
  x <- subset(Uni10, year==ano[i])
  annual.prec10.u[i] <- length(x$Data)/4
}
annual.prec10.uts <- ts(annual.prec10.u, start=1983)

Bi10 <- subset(Bi, Data >= 10)
annual.prec10.b <- numeric(30)
for (i in 1:30) {
  x <- subset(Bi10, year==ano[i])
  annual.prec10.b[i] <- length(x$Data)/6
}
annual.prec10.bts <- ts(annual.prec10.b, start=1983)

Semi10 <- subset(Semi, Data >= 10)
annual.prec10.s <- numeric(30)
for (i in 1:30) {
  x <- subset(Semi10, year==ano[i])
  annual.prec10.s[i] <- length(x$Data)/6
}
annual.prec10.sts <- ts(annual.prec10.s, start=1983)

plot(ano ,annual.prec10.b, ylim = c(15, 50), type="n", main = "Annual total days of rain (>= 10 mm)", xlab = "year", ylab = "Days")
abline(h = mean(annual.prec10.u), col="darkred")
lines(ano, annual.prec10.u, type="s", col="red")
abline(h = mean(annual.prec10.b), col="darkblue")
lines(ano, annual.prec10.b, type="s", col="blue")
abline(h = mean(annual.prec10.s), col="darkgreen")
lines(ano, annual.prec10.s, type="s", col="green")
legend(locator(1),legend= c("Uni", "Bi", "Semi"), col = c("red", "blue", "green"), lty=1, cex=0.7)

#annual total days >= 20mm (R20mm)
Uni20 <- subset(Uni, Data >= 20)
annual.prec20.u <- numeric(30)
for (i in 1:30) {
  x <- subset(Uni20, year==ano[i])
  annual.prec20.u[i] <- length(x$Data)/4
}
annual.prec20.uts <- ts(annual.prec20.u, start=1983)

Bi20 <- subset(Bi, Data >= 20)
annual.prec20.b <- numeric(30)
for (i in 1:30) {
  x <- subset(Bi20, year==ano[i])
  annual.prec20.b[i] <- length(x$Data)/6
}
annual.prec20.bts <- ts(annual.prec20.b, start=1983)

Semi20 <- subset(Semi, Data >= 20)
annual.prec20.s <- numeric(30)
for (i in 1:30) {
  x <- subset(Semi20, year==ano[i])
  annual.prec20.s[i] <- length(x$Data)/6
}
annual.prec20.sts <- ts(annual.prec20.s, start=1983)

plot(ano ,annual.prec20.s,ylim = c(7, 27), type="n", main = "Annual total days of rain (>= 20 mm)", xlab = "year", ylab = "Days")
abline(h = mean(annual.prec20.u), col="darkred")
lines(ano, annual.prec20.u, type="s", col="red")
abline(h = mean(annual.prec20.b), col="darkblue")
lines(ano, annual.prec20.b, type="s", col="blue")
abline(h = mean(annual.prec20.s), col="darkgreen")
lines(ano, annual.prec20.s, type="s", col="green")
legend(locator(1),legend= c("Uni", "Bi", "Semi"), col = c("red", "blue", "green"), lty=1, cex=0.7)

#Lowess (smooting) plots
plot(r99.ts, main = "lowess(prec on days > 99%)")
lines(lowess(r99.ts, f=0.3), col = 3)
lines(lowess(r99.ts, f=0.4), col = 4)
legend(2000, 175, c(paste("f = ", c("0.3", "0.4"))), lty = 1, col = 3:4)

#Bootstrap quantiles
quantile = quantile(y0312$Data, probs= c(0.95, 0.99), type=5)
B = 5000
n = length(y0312$Data)

z <- rep(max(y0312$Data),5000)
df <- c(z ,sample(y0312$Data, size = B * (n-1), replace = TRUE))
boot.samples = matrix(df,B, n)
boot.samples = matrix(sample(y0312$Data, size = B * n, replace = TRUE), B, n)
boot.statistics = apply(boot.samples, 1, quantile, probs=c(0.95, 0.99), type=5)

require(ggplot2)
ggplot(data.frame(qV95 = boot.statistics[1,]),aes(x=qV95)) +
  geom_histogram(binwidth=0.25,aes(y=..density..)) +
  geom_density(color="red")

ggplot(data.frame(qV99 = boot.statistics[2,]),aes(x=qV99)) +
  geom_histogram(binwidth=0.25,aes(y=..density..)) +
  geom_density(color="red")

quantile(boot.statistics[1,], probs=c(0.025, 0.975))
quantile(boot.statistics[2,], probs=c(0.025, 0.975))



