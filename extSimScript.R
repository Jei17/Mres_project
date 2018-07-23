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
probs <- n/length(e) #probability of being above the threshold

thres <- c(0,1,2,5,10,30,50,70,100,120,150,200, 250)
output <- numeric(length(thres))

k <- 10000 #pick a sample size
start.time <- Sys.time()
for (m in 1:length(thres)){
    for (j in 1:1000){
    occurence <- numeric(k)
    occurence[1] <- 1
    number <- runif(k, 0, 1)
    for (i in 2:(k-1)) {
      if((occurence[i-1]>0) & (number[i] < 0.419))# prr
      {occurence[i] <- 1}
      else if ((occurence[i-1]==0) & (number[i] < 0.366)) #pnr
      {occurence[i] <- 1}
      else {occurence[i] <- 0}
    }
    
    amount <- numeric(k)
    num <- runif(k, 0, 1)
    for (i in 1:k){
      if (num[i] >= 0.131) {amount[i]  <- rtrunc(1, spec="gamma", shape=0.876, scale=20.938, b=39.6)}
      else {amount[i] <- rtgpd(1, gamma=-0.04, mu =39.6 , sigma=34.0, endpoint = (qtgpd(0.9999, gamma=-0.04, mu=39.6, sigma=34.0, endpoint=Inf)-thres[m]))}
    }
    rainfall <- amount*occurence
    y_nn1 <- sort(rainfall, decreasing = TRUE)
    v <- y_nn1[1:floor((probs*k))]
    
    tM <- trMLE(v,start = c(0.1,1), plot=FALSE)
    tTM <- trTestMLE(v, gamma=tM$gamma, tau=tM$tau, alpha = 0.05, plot = FALSE)
    
    i <- length(which(occurence==1))
    trP <- tTM$reject[floor(i*probs)]
    if (trP==FALSE) {tr.res[j] <- 0} else {tr.res[j] <- 1}
    
    output[m] <- length(which(tr.res==1))
  }}
end.time <- Sys.time()


require(reshape2)
s500 <- c(345, 362, 376, 369, 337, 376, 393, 359, 401, 441, 473, 727, 915)
s1000 <- c(275, 288, 267, 271, 286, 268, 293, 279, 319, 356, 498, 839, 956)
s3000 <- c(181, 216, 199, 218, 210, 197, 241, 263, 352, 519, 788, 919, 1000)
s5000 <- c(165, 182, 182, 188, 177, 185, 238, 229, 470, 711, 890, 957, 1000)
s10000 <- c(176, 159, 149, 165, 139, 201, 200, 352, 730, 895, 980, 998, 1000)
df <- data.frame(thres, s500, s1000, s3000, s5000, s10000)
df <- melt(df, id="thres")
light.trunc <- qtgpd(0.99, gamma=-0.04, mu=39.6, sigma=34.0, endpoint=Inf)
yinter.light <- qtgpd(0.9999, gamma=-0.04, mu=39.6, sigma=34.0, endpoint=Inf) - light.trunc
rough.trunc <- qtgpd(0.9, gamma=-0.04, mu=39.6, sigma=34.0, endpoint=Inf)
yinter.rough <- qtgpd(0.9999, gamma=-0.04, mu=39.6, sigma=34.0, endpoint=Inf) - rough.trunc

require(ggplot2)
ggplot(df, aes(thres, value, col=variable)) + geom_line() +geom_vline(xintercept = yinter.light, col=1) + geom_vline(xintercept = yinter.rough, col=1) + ylab("Number of rejections") + xlab("Threshold in mm") + labs(title="KRA, JJA, est end.point=301.54")