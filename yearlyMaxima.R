year <- seq(1983, 2012, 1)

for (i in year) {x <- subset(KRA, substr(data$Date,1, 4)==i)
                  yearlymax <- max(x$Data)}