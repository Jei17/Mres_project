df <- data.frame(x=ano, y=annual.rain.20)

g <- ggplot(df, aes(x=ano)) + geom_point(aes(y=annual.rain.20)) +geom_line(aes(y=annual.rain.20)) 
ggMarginal(g, type = "density", margins  = "y", bw=0.3, fill = "transparent")

plot(density(annual.rain.20, bw=0.1, kernel = "biweight"))

