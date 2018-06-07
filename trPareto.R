require(ReIns)
x_nn <- sort(dataNew.norm$Data, decreasing = TRUE)

x_400n <- x_nn[1:400]

y <- GPDfit(x_nn[1:1000])
#strong suggestions for gumbel, depending on no of observations used
#negative shape suggests T finite

r <- gpdSim(model=list(xi=0.0018, mu=0, beta= 26.8876), n=1000)
r <- rpareto(500, -0.575,140.682)


h <-trHill(x_100n, logk = TRUE, plot = TRUE)
trEndpoint(x_100n, gamma = h$gamma, plot=TRUE)

#must use Hill to estimate gamma because need gamma to be positive
GH <- genHill(x_nn[1:700], gamma=H$gamma, plot=TRUE, col="blue", ylim=c(-0.2,0.4))


H <- Hill(x_700n, plot=TRUE, col="blue")
# Add EPD estimator
EPD(x_nn, add=TRUE, col="orange", lty=2)
legend("bottomright", c("Hill","EPD"), col=c("blue","orange"), lty=1:2)

#Quantile estimation
p <- 0.0005
x <- x_nn[5:400]

#must use Hill to estimate gamma because need gamma to be positive
H <- Hill(x)
GH <- genHill(x, gamma=H$gamma)
QuantGH(x, gamma=GH$gamma,p=p, plot= TRUE, col="blue")
g <- GPDmle(x)
QuantGPD(x, gamma=g$gamma, sigma = g$sigma, p=p, add=TRUE, col="green")
M <- Moment(x)
QuantMOM(x, gamma=M$gamma, p=p, add=TRUE, col="orange")


#test for truncation
tM <- trMLE(x,start = c(1,1), plot=FALSE)
tTM <- trTestMLE(x, gamma=tM$gamma, tau=tM$tau, alpha = 0.05, plot = TRUE, main = "Test for truncation")

#trPareto estimation
x <- x_nn[1:600]

tH <- trHill(x, r = 1, tol = 1e-08, maxiter = 100, logk = FALSE,plot = FALSE, add = FALSE, main = "Estimates of the EVI")
tD <- trDT(x, r = 1, gamma=tH$gamma, plot = FALSE, add = FALSE, main = "Estimates of DT")
trParetoQQ(x, r = 1, DT=tD$DT, kstar = NULL, plot = TRUE, main = "TPa QQ-plot")

#right end point
trEndpoint(x, r = 1, gamma=tH$gamma, plot = TRUE,col="blue",main = "Estimates of endpoint")
trEndpointMLE(x, gamma=tM$gamma, tau=tM$tau, add = TRUE, col="orange")

trQuantMLE(x, gamma=tM$gamma, tau=tM$tau, DT=tD$DT, p=p, Y = FALSE, plot = TRUE,col="red", 
           main = "Estimates of extreme quantile", ylim=c(0,400))
trQuantMLE(x, gamma=tM$gamma, tau=tM$tau, DT=tD$DT, p=p, Y = TRUE, add = TRUE, col="green")


#Truncation analysis for stations
#pick station for analysis
x <- ADA

DJF <- subset(x, month==1| month==2|month==12)
MAM <- subset(x, month==3|month==4 |month==5)
JJA <- subset(x, month==6|month==7|month==8)
SON <- subset(x, month==9|month==10|month==11)

#plot time-serie/trend

#test for trend and stationarity
y <- SON
  
trend.test(y$Data)
cox.stuart.test(y$Data)
trend.test(y$Data, "diff.sign") #useless since rainy days are not smooth
adf.test(y$Data) #null hypothesis is non-stationary
kpss.test(y$Data) #null hypothesis data is trend-stationary
acf(y$Data)
MannKendall(y$Data) #not sure if useful since so many ties due to zeros


#trend consecutive measurments
n <- length(y$Data)
x_t <- y$Data[2:n]
x_t1 <- y$Data[1:n-1]
plot(x_t1, x_t)

#Pick out the tail data
require(evir)
y.pos <- subset(y, Data >= 1)
n <- floor(length(y.pos$Data)*0.3)
MeanExcess(y.pos$Data, plot=TRUE, k=TRUE)
n <- 80

y_nn <- sort(y$Data, decreasing = TRUE)
z <- y_nn[1:n]

#check for duplicated values, smoothing (make it cts). m=2 => +0.025, m=3 => 1/6, 1/2, 5/6
z[duplicated(z) | duplicated(z, fromLast=TRUE)]

#if duplicates at position z[a]
a <-which(z==18.2)[1]
# 2 ties
z[a+1] <- z[a+1] - 0.025
z[a] <- z[a] + 0.025

#3 ties
z[a] <- z[a] + 0.16
z[a+2] <- z[a+2] - 0.16

#Test for rough truncation
tM <- trMLE(z,start = c(0.1,1))
tT <- trTest(z, plot=TRUE) #must have >0 shape parameter
tTM <- trTestMLE(z, gamma=tM$gamma, tau=tM$tau, alpha = 0.05, plot = TRUE, main = "Test for truncation")


#Parameter estimation, end point, GPD fit

#Shape parameter estimation, non truncated
H <- Hill(z, plot=TRUE, main="Shape parameter estimation", col=2, ylim=c(-1,1))
GH <- genHill(z, gamma=H$gamma, add=TRUE, col=3)
g <- GPDmle(z, add=TRUE, col=4)
M <- Moment(z, add=TRUE, col=5)
abline(h=0)
legend("bottomright", c("Hill", "genHill", "MLE", "MOM"), col = c(2, 3, 4, 5), lty = 1, cex=0.7)

#GPD fit, non truncated
fit.gpd <- GPDfit(z, start=c(0.1,1)) #MLE
qplot(z,xi = pot$par.ests[1] , line=TRUE, main="QQ using POT shape estimate")
genQQ(z, gamma=H$gamma, plot=TRUE) #can only use Hill as input
GPDresiduals(z, t=y_nn[n], gamma = g$gamma[60], sigma = g$sigma[60], plot=TRUE)

pot <-pot(y$Data, nextremes = n, run = 5) #at least 5 days between extremes

#quantile estimation usgin GPDmle
p= 0.0001
QuantGH(z, gamma=GH$gamma, p=p, plot =TRUE, col=4)
QuantGPD(z, gamma=g$gamma, sigma=g$sigma,p=p, add = TRUE, col=2)
QuantMOM(z, gamma=M$gamma, p=p, add=TRUE, col=3)
legend("topright", c("GPD", "MOM", "genHill"), col = c(2, 3, 4), lty = 1, cex=0.7)

#Estimations if truncated
tH <- trHill(z, plot=TRUE, col=2, ylim=c(-1,1))
tT <- trEndpointMLE(z, gamma=tM$gamma, tau = tM$tau, plot=TRUE)
tDT <- trDT(z, gamma=tH$gamma, plot = TRUE)
tMDT <- trDTMLE(z, gamma=tM$gamma, tau=tM$gamma, plot=TRUE)

trParetoQQ(z, DT= tDT$DT, plot=TRUE)

#control no trend in extremes
w <- subset(y, Data > y_nn[n])
w <- w$Data

for (i in 1:length(w)) {
  w[i] <- w[i] - y_nn[n]
}
plot(w)