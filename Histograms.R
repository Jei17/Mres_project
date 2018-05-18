HistA <- hist(data.norm$Data, freq = FALSE)
HistH <- hist(higher.norm$Data, freq = FALSE)
HistL <- hist(lower.norm$Data, freq = FALSE)

#plot(0,0, type='n', xlim=c(0,80), ylim = c(0,0.20), xlab = "rain mm", ylab = "density", main = "Density plot")
plot(HistA, col="red", density = 10)
plot(HistH, col="green", density = 10, add=TRUE)
plot(HistL, col="blue", density = 10, add=TRUE)

## calculate the range of the graph
xlim <- range(HistA$breaks,HistH$breaks, HistL$breaks)
ylim <- range(0,HistA$density,
              HistH$density, HistL$density)
## plot the first graph
plot(HistH,xlim = xlim, ylim = ylim,
     col = rgb(0.6, 0, 0, alpha = 0.3),xlab = 'Rain mm',
     freq = FALSE, ## relative, not absolute frequency
     main = 'Histogram of rainfall')
## plot the second graph on top of this
opar <- par(new = FALSE)
plot(HistA,xlim = xlim, ylim = ylim,
     xaxt = 'n', yaxt = 'n', ## don't add axes
     col = rgb(0, 0.4, 0, alpha = 0.3), add = TRUE,
     freq = FALSE) ## relative, not absolute frequency
## add a legend in the corner
plot(HistL,xlim = xlim, ylim = ylim,
     xaxt = 'n', yaxt = 'n', ## don't add axes
     col = rgb(0, 0.2, 0.3,alpha =  0.3), add = TRUE,
     freq = FALSE) ## relative, not absolute frequency
## add a legend in the corner
legend('topright',c('High values','All values', 'Low values'),
       fill = c('red', 'blue', 'green'), bty = 'n',
       border = NA)
par(opar)