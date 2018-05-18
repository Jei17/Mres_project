#Fit different distributions
fit.gamma.mle <- fitdist(data.norm$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
fit.gamma.mge <- fitdist(data.norm$Data, "gamma",  method = "mge", gof = "CvM")
#gives nearly identical results, mle slightly better with a little bit lower AIC

summary(fit.gamma.mle)
summary(fit.gamma.mge)
plot(fit.gamma.mle)
plot(fit.gamma.mge)

fit.lognorm <- fitdist(data.norm$Data, "lnorm", method = "mle")
fit.exp <- fitdist(data.norm$Data, dexp, method = "mle")

summary(fit.lognorm)
summary(fit.exp)

#Different thresholds
norm40 <- subset(data.pos, Data < 40)
fit.gamma40 <- fitdist(norm40$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
norm50 <- subset(data.pos, Data < 50)
fit.gamma50 <- fitdist(norm50$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
norm60 <- subset(data.pos, Data < 60)
fit.gamma60 <- fitdist(norm60$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
norm70 <- subset(data.pos, Data < 70)
fit.gamma70 <- fitdist(norm70$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
norm80 <- subset(data.pos, Data < 80)
fit.gamma80 <- fitdist(norm80$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
norm90 <- subset(data.pos, Data < 90)
fit.gamma90 <- fitdist(norm90$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
norm100 <- subset(data.pos, Data < 100)
fit.gamma100 <- fitdist(norm100$Data, "gamma",  method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
norm150 <- subset(data.pos, Data < 150)
fit.lnorm150 <- fitdist(norm150$Data, "lnorm",  method = "mle")

summary(fit.gamma40)
summary(fit.gamma50)
summary(fit.gamma60)
summary(fit.gamma70)
summary(fit.gamma80)
summary(fit.gamma90)
summary(fit.gamma100)
summary(fit.lnorm150)

#car
qqPlot(norm40$Data, distribution = "gamma", shape=0.6975)
qqPlot(norm50$Data, distribution = "gamma", shape=0.670)
qqPlot(norm60$Data, distribution = "gamma", shape=0.6518)
qqPlot(norm70$Data, distribution = "gamma", shape=0.6393)
qqPlot(norm80$Data, distribution = "gamma", shape=0.63)
qqPlot(norm90$Data, distribution = "gamma", shape=0.63)
qqPlot(norm100$Data, distribution = "gamma", shape=0.63)
qqPlot(norm150$Data, distribution = "lnorm", meanlog=1.41, sdlog=1.625)

fit.gamma <- fitdist(data.pos$Data, "gamma", method = "mle", lower= c(0,0), start = list(scale = 1, shape = 1))
summary(fit.gamma)
qqPlot(data.pos$Data, distribution = "gamma", shape=0.6073)

#GoF test, CvM
num_of_samples = length(norm40$Data)
y <- rgamma(num_of_samples, shape = 0.6975157, scale = 11.8787312)
res <- cvm(norm40$Data,y)
pvalue = 1/6*exp(-60.953)

gofstat(fit.gamma40)
gofstat(fit.gamma100)
gofstat(fit.gamma)