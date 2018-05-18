year <- seq(1983, 2012)
month <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")


semi.dec.c <- numeric(30)
for (i in 1:30) {
  semi.dec.c[i] <- sum(subset(Semi.dec$Data, year==year[i]))/6
}
semi.nov.c[1] <-sum(subset(Semi.nov$Data, year==year[1]))/5

bxs <-boxplot(Uni.jan.c, Uni.feb.c, Uni.mar.c, Uni.apr.c, Uni.may.c, Uni.jun.c, Uni.jul.c, Uni.aug.c, Uni.sep.c, Uni.oct.c, Uni.nov.c, Uni.dec.c, main = "Uni modal, normal", names = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

Semi.jan.c <- numeric(30)
 for (i in 1:30) {
    Semi.jan.c[i] <- sum(subset(Semi.jan$Data, year==year[i]))/6
  }
 Semi.feb.c <- numeric(30)
 for (i in 1:30) {
    Semi.feb.c[i] <- sum(subset(Semi.feb$Data, year==year[i]))/6
   }
 Semi.mar.c <- numeric(30)
for (i in 1:30) {
     Semi.mar.c[i] <- sum(subset(Semi.mar$Data, year==year[i]))/6
   }
 Semi.apr.c <- numeric(30)
 for (i in 1:30) {
     Semi.apr.c[i] <- sum(subset(Semi.apr$Data, year==year[i]))/6
   }
Semi.may.c <- numeric(30)
 for (i in 1:30) {
     Semi.may.c[i] <- sum(subset(Semi.may$Data, year==year[i]))/6
   }
 Semi.jun.c <- numeric(30)
 for (i in 1:30) {
     Semi.jun.c[i] <- sum(subset(Semi.jun$Data, year==year[i]))/6
   }
 Semi.jul.c <- numeric(30)
 for (i in 1:30) {
     Semi.jul.c[i] <- sum(subset(Semi.jul$Data, year==year[i]))/6
   }
Semi.aug.c <- numeric(30)
 for (i in 1:30) {
     Semi.aug.c[i] <- sum(subset(Semi.aug$Data, year==year[i]))/6
   }
 Semi.sep.c <- numeric(30)
 for (i in 1:30) {
     Semi.sep.c[i] <- sum(subset(Semi.sep$Data, year==year[i]))/6
   }
 Semi.oct.c <- numeric(30)
 for (i in 1:30) {
     Semi.oct.c[i] <- sum(subset(Semi.oct$Data, year==year[i]))/6
   }
 Semi.nov.c <- numeric(30)
 for (i in 1:30) {
     Semi.nov.c[i] <- sum(subset(Semi.nov$Data, year==year[i]))/6
   }
 Semi.dec.c <- numeric(30)
 for (i in 1:30) {
     Semi.dec.c[i] <- sum(subset(Semi.dec$Data, year==year[i]))/6
 }
 Semi.feb.c[28] <-sum(subset(Semi.feb$Data, year==year[28]))/4
 Semi.mar.c[2] <-sum(subset(Semi.mar$Data, year==year[2]))/5
 Semi.may.c[3] <-sum(subset(Semi.may$Data, year==year[3]))/5
 Semi.jul.c[4] <-sum(subset(Semi.jul$Data, year==year[4]))/5
 Semi.aug.c[1] <-sum(subset(Semi.aug$Data, year==year[1]))/5
 Semi.sep.c[3] <-sum(subset(Semi.sep$Data, year==year[3]))/5
 Semi.nov.c[1] <-sum(subset(Semi.nov$Data, year==year[1]))/5
 
 Bi.may.c[12] <- sum(subset(Bi.may$Data, year==year[12]))/5
 Bi.sep.c[30] <- sum(subset(Bi.sep$Data, year==year[30]))/5
 Bi.sep.c[16] <- sum(subset(Bi.sep$Data, year==year[16]))/5
 
 Uni.nov.c[10]<- sum(subset(Uni.nov$Data, year==year[10]))/3
 Uni.dec.c[10]<- sum(subset(Uni.dec$Data, year==year[10]))/3
 
 
 v <- list(Semi.jan.c, Semi.feb.c, Semi.mar.c, Semi.apr.c, Semi.may.c, Semi.jun.c, Semi.jul.c, Semi.aug.c, Semi.sep.c, Semi.oct.c, Semi.nov.c, Semi.dec.c)
 w <- as.data.frame(v, col.names = month)
 
 for (n in 1:12) {
   print(describe(w[n]))
 }