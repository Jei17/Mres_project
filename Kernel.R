df <- data.frame(x=ano, y=annual.rain.10)

require(ggExtra)
g <- ggplot(df, aes(x=ano)) + geom_point(aes(y=annual.rain.10)) +geom_line(aes(y=annual.rain.10)) 
ggMarginal(g, type = "density", margins  = "y", bw=0.5, fill = "transparent")

plot(density(annual.rain.10, bw=0.2, kernel = "biweight"))

plot(density(ar20.prop,bw=0.003, kernel="biweight"))