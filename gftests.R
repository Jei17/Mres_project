ad.test(Uni.oct.n$Data, "pgamma", scale=12.805, shape=0.681)

Bi.oct <- subset(Bi.oct, Data <80)

cvm.test(Semi.jun$Data, "pgamma", scale=30.324, shape=0.529)
cvm.test(Semi.jun$Data, "pweibull",scale=11.451, shape=0.646)

x <-rgamma(1000, scale = 16.644, shape=0.666)

fit.gamma.semioct <- fitdist(Semi.may$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
Semi.apr.n <- subset(Semi.may, Data <90)
fit.gamma.semiapr.n <- fitdist(Semi.apr.n$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))

