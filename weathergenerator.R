x <- ADA

DJF <- subset(x, month==1| month==2|month==12)
MAM <- subset(x, month==3|month==4 |month==5)
JJA <- subset(x, month==6|month==7|month==8)
SON <- subset(x, month==9|month==10|month==11)

e <- MAM$Data

#1 step Markov chain
num_rn = 0
for (i in 1:(length(e)-1)) {
  if ((e[i+1]>=1) & (e[i]<1)){
    num_rn <- num_rn + 1
  }
}
p_rn <- num_rn/length(e)

num_nr = 0
for (i in 1:(length(e)-1)) {
  if ((e[i+1]<1) & (e[i]>=1)){
    num_nr <- num_nr + 1
  }
}
p_nr <- num_nr/length(e)

num_nn = 0
for (i in 1:(length(e)-1)) {
  if ((e[i+1]<1) & (e[i]<1)){
    num_nn <- num_nn + 1
  }
}
p_nn <- num_nn/length(e)

num_rr = 0
for (i in 1:(length(e)-1)) {
  if ((e[i+1]>=1) & (e[i]>=1)){
    num_rr <- num_rr + 1
  }
}
p_rr <- num_rr/length(e)

#probability of rain
prr <- num_rr/(num_nr + num_rr)
prn <- num_rn/(num_rn + num_nn)

rainydays <- subset(e, e>=1)

require(fitdistrplus)
fit <- fitdist(rainydays, distr = "gamma", method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
qqcomp(fit)
summary(fit)
plot(fit)

k<- length(e)
occurence <- numeric(k)

rainonfirstday <- 1
occurence[1] <- rainonfirstday

number <- runif(k, 0, 1)
for (i in 2:(k-1)) {
  if((occurence[i-1]>0) & (number[i] < prr))
  {occurence[i] <- 1}
  else if ((occurence[i-1]==0) & (number[i] < prn))
    {occurence[i] <- 1}
  else {occurence[i] <- 0}
}

amount <- rgamma(k, shape = fit$estimate[2], scale=fit$estimate[1])

for (i in 1:length(amount)){
  if (amount[i]> z[50]){amount[i] <- rtgpd(1, gamma=pot$par.ests[1], mu =z[50], sigma=pot$par.ests[2], endpoint = qtgpd(0.99, gamma=pot$par.ests[1], mu=z[50], sigma=pot$par.ests[2], endpoint=Inf))}
  else {amount[i] <- amount[i]}
}

require(truncdist)
tr.amount<- rtrunc(k, spec="gamma", shape=fit$estimate[2], scale=fit$estimate[1],b= qgamma(.9, shape=fit$estimate[2], scale=fit$estimate[1]))
rainfall <- amount*occurence

plot(rainfall)
plot(MAM$Data)


#Extreme analysis
s <- rainfall

require(ReIns)
require(evir)
y.pos1 <- subset(s, s >= 1)
n1 <- floor(length(y.pos1)*0.3)
MeanExcess(y.pos1[1:n1], plot=TRUE, k=TRUE)
shape(y.pos1, models=30, start=30, end=n1)
n1 <- 60

y_nn1 <- sort(s, decreasing = TRUE)
v <- y_nn1[1:n1]

#check for duplicated values, smoothing (make it cts). m=2 => +0.025, m=3 => 1/6, 1/2, 5/6
v[duplicated(v) | duplicated(v, fromLast=TRUE)]

#if duplicates at position v[a]
a <-which(v==30.5)[1]
# 2 ties
v[a+1] <- v[a+1] - 0.025
v[a] <- v[a] + 0.025

#3 ties
v[a] <- v[a] + 0.16
v[a+2] <- v[a+2] - 0.16

#Test for rough truncation
tM <- trMLE(v,start = c(0.1,1), plot=FALSE)
#tT <- trTest(v, plot=TRUE, main = "Hill test truncation") #must have >0 shape parameter, T,B
tTM <- trTestMLE(v, gamma=tM$gamma, tau=tM$tau, alpha = 0.05, plot = TRUE, main = "MLE test for truncation")


#Parameter estimation, end point, GPD fit

#Shape parameter estimation, non truncated
H <- Hill(v, plot=TRUE, main="Shape parameter estimation, gamma/GPD", col=2, ylim=c(-1,1)) #Normal hill
tH <- trHill(v, add=FALSE, col=6)
GH <- genHill(v, gamma=H$gamma, add=TRUE, col=3)
g <- GPDmle(v, add=TRUE, col=4) #non declustered
M <- Moment(v, add=TRUE, col=5)
abline(h=0)
legend("bottomright", c("Hill", "genHill", "MLE", "MOM"), col = c(2, 3, 4, 5), lty = 1, cex=0.7)

#GPD fit, non truncated
fit.gpd <- GPDfit(v[1:48], start=c(0.1,1)) #MLE, gives really poor fit
pot.w <-pot(s, threshold = v[40], run = 5) #at least 5 days between extremes


#Quantiles
pv <- c(0.05, 0.01, 0.005, 0.001, 0.0001, 0.00001, 0.000001)
q.mom <- numeric(6)
q.mle <-numeric(6) 

for (i in 1:length(pv)) {
  q <- pv[i]
  qg <- QuantGPD(v, gamma=g$gamma, sigma=g$sigma, p=q)
  qm <- QuantMOM(v, gamma=M$gamma, p=q)
  q.mle[i] <- qg$Q[40]
  q.mom[i] <- qm$Q[40]
  
}

plot(log10(pv), q.mle,type = "l", main= paste("Quantile estimation, thres=", signif(v[40], digits = 3)),xlab = "log p-value", ylab="Quantile", col = 2)
lines(log10(pv), q.mom, col =3)
legend("topright", c(paste("MLE", signif(g$gamma[40], digits=4)), paste("MOM", signif(M$gamma[40], digits = 4))),col =  c(2,3), lty=1)
