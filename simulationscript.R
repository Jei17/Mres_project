k <- length(e)
probs <- 60/length(rainydays)

occurence <- numeric(k)
occurence[1] <- rainonfirstday
number <- runif(k, 0, 1)
for (i in 2:(k-1)) {
  if((occurence[i-1]>0) & (number[i] < prr))
  {occurence[i] <- 1}
  else if ((occurence[i-1]==0) & (number[i] < prn))
  {occurence[i] <- 1}
  else {occurence[i] <- 0}
}
amount <- numeric(k)
num <- runif(k, 0, 1)
for (i in 1:k){
  if (num[i] >= probs) {amount[i]  <- rtrunc(1, spec="gamma", shape=fit$estimate[2], scale=fit$estimate[1],b=z[60])}
  else {amount[i] <- rtgpd(1, gamma=pot$par.ests[1], mu =z[60] , sigma=pot$par.ests[2], endpoint = Inf)}
}
rainfall <- amount*occurence
mean(rainfall)
s <- rainfall
y.pos1 <- subset(s, s >= 1)
n1 <- floor(length(y.pos1)*0.3)
length(which(s >z[60]))
MeanExcess(y.pos1[1:n1], plot=TRUE, k=TRUE)

n1 <- length(which(rainfall >z[60])) + 30
y_nn1 <- sort(s, decreasing = TRUE)
v <- y_nn1[1:n1]

tM <- trMLE(v,start = c(0.1,1), plot=FALSE)
#tT <- trTest(v, plot=TRUE, main = "Hill test truncation") #must have >0 shape parameter, T,B
tTM <- trTestMLE(v, gamma=tM$gamma, tau=tM$tau, alpha = 0.05, plot = TRUE, main = "MLE test for truncation")

pot.w <-pot(s, nextremes = 60, run=5)
v[60]

#Quantiles
pv <- c(0.05, 0.01, 0.005, 0.001, 0.0001, 0.00001, 0.000001)
q.mom <- numeric(6)
q.mle <-numeric(6) 
g <- GPDmle(v, col=4) #non declustered
M <- Moment(v, col=5)
for (i in 1:length(pv)) {
  q <- pv[i]
  qg <- QuantGPD(v, gamma=g$gamma, sigma=g$sigma, p=q)
  qm <- QuantMOM(v, gamma=M$gamma, p=q)
  q.mle[i] <- qg$Q[170]
  q.mom[i] <- qm$Q[170]
  
}
plot(log10(pv), q.mle,type = "l", main= paste("Quantile estimation, thres=", signif(v[170], digits = 3)),xlab = "log p-value", ylab="Quantile", col = 2)
lines(log10(pv), q.mom, col =3)
legend("topright", c(paste("MLE", signif(g$gamma[170], digits=4)), paste("MOM", signif(M$gamma[170], digits = 4))),col =  c(2,3), lty=1)

tT <- trEndpointMLE(v[20:170], gamma=tM$gamma[20:170], tau = tM$tau[20:170], plot=TRUE)
plot(tT$k, tT$Tk, ylim=c(60, 100))



#increasing sample size and truncation
x <- KRA

DJF <- subset(x, month==1| month==2|month==12)
MAM <- subset(x, month==3|month==4 |month==5)
JJA <- subset(x, month==6|month==7|month==8)
SON <- subset(x, month==9|month==10|month==11)

y <- JJA
#Pick out the tail data
require(ReIns)
require(evir)
y.pos <- subset(y, Data >= 1)
n <- floor(length(y.pos$Data)*0.3)
MeanExcess(y.pos$Data[1:n], plot=TRUE, k=TRUE)
shape(y.pos$Data, models=30, start=30, end=n)

n <- 140 #Pick a suitable threshold 
y_nn <- sort(y$Data, decreasing = TRUE)
z <- y_nn[1:n] #Pick out the data above the threshold
pot <-pot(y$Data, threshold = z[n], run = 5) #at least 5 days between extremes

e <- JJA$Data #Pick out all data
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

tr.res <- numeric(1000)
probs <- n/length(rainydays) #probability of being above the threshold

thres <- c(1,2,5,10,30,50,70,100,120,150,200,250)
output <- numeric(length(thres))

k <- 50000 #sample size
for (m in 1:length(thres)){
for (j in 1:1000){
  occurence <- numeric(k)
  occurence[1] <- 1
  number <- runif(k, 0, 1)
  for (i in 2:(k-1)) {
    if((occurence[i-1]>0) & (number[i] < prr))
    {occurence[i] <- 1}
    else if ((occurence[i-1]==0) & (number[i] < prn))
    {occurence[i] <- 1}
    else {occurence[i] <- 0}
  }
  
  amount <- numeric(k)
  num <- runif(k, 0, 1)
  for (i in 1:k){
    if (num[i] >= probs) {amount[i]  <- rtrunc(1, spec="gamma", shape=fit$estimate[2], scale=fit$estimate[1], b=z[n])}
    else {amount[i] <- rtgpd(1, gamma=pot$par.ests[1], mu =z[n] , sigma=pot$par.ests[2], endpoint = (qtgpd(0.9999, gamma=pot$par.ests[1], mu=z[n], sigma=pot$par.ests[2], endpoint=Inf)-thres[m]))}
  }
  rainfall <- amount*occurence
  y_nn1 <- sort(rainfall, decreasing = TRUE)
  v <- y_nn1[1:floor((probs*k))]
  
  tM <- trMLE(v,start = c(0.1,1), plot=FALSE)
  tTM <- trTestMLE(v, gamma=tM$gamma, tau=tM$tau, alpha = 0.05, plot = FALSE)
  
  l <- length(which(occurence==1))
  trP <- tTM$reject[floor(l*probs)]
  if (trP==FALSE) {tr.res[j] <- 0} else {tr.res[j] <- 1}
  
  output[m] <- length(which(tr.res==1))
}}

length(which(tr.res==1))

test <- rtgpd(500, gamma=pot$par.ests[1], mu =z[n] , sigma=pot$par.ests[2], endpoint = (qtgpd(0.9999, gamma=pot$par.ests[1], mu=z[n], sigma=pot$par.ests[2], endpoint=Inf)-50))
test1 <-rtgpd(500, gamma=pot$par.ests[1], mu =z[n] , sigma=pot$par.ests[2], endpoint = Inf)
