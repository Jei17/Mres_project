#Split into decades
y8392 <- subset(dataNew.pos, year==1983 |year==1984|year==1985|year==1986|year==1987|year==1988|year==1989|year==1990|year==1991|year==1992)
y9302 <- subset(dataNew.pos, year==1993|year==1994|year==1995|year==1996|year==1997|year==1998|year==1999|year==2000|year==2001|year==2002)
y0312 <- subset(dataNew.pos, year==2003|year==2004|year==2005|year==2006|year==2007|year==2008|year==2009|year==2010|year==2011|year==2012)

#Pick out norm values
y8392.norm <- subset(y8392, Data <80)
y9302.norm <- subset(y9302, Data < 80)
y0312.norm <- subset(y0312, Data <80)

#Months y8392
y8392.norm.jan <- subset(y8392.norm, substr(y8392.norm$Date, 5, 6)=='01')
d.y8392.normjan <- as.Date(as.character(y8392.norm.jan$Date), format='%Y%m%d')
y8392.norm.feb <- subset(y8392.norm, substr(y8392.norm$Date, 5, 6)=='02')
d.y8392.normfeb <- as.Date(as.character(y8392.norm.feb$Date), format='%Y%m%d')
y8392.norm.mar <- subset(y8392.norm, substr(y8392.norm$Date, 5, 6)=='03')
d.y8392.normmar <- as.Date(as.character(y8392.norm.mar$Date), format='%Y%m%d')
y8392.norm.apr <- subset(y8392.norm, substr(y8392.norm$Date, 5, 6)=='04')
d.y8392.normapr <- as.Date(as.character(y8392.norm.apr$Date), format='%Y%m%d')
y8392.norm.may <- subset(y8392.norm, substr(y8392.norm$Date, 5, 6)=='05')
d.y8392.normmay <- as.Date(as.character(y8392.norm.may$Date), format='%Y%m%d')
y8392.norm.jun <- subset(y8392.norm, substr(y8392.norm$Date, 5, 6)=='06')
d.y8392.normjun <- as.Date(as.character(y8392.norm.jun$Date), format='%Y%m%d')
y8392.norm.jul <- subset(y8392.norm, substr(y8392.norm$Date, 5, 6)=='07')
d.y8392.normjul <- as.Date(as.character(y8392.norm.jul$Date), format='%Y%m%d')
y8392.norm.aug <- subset(y8392.norm, substr(y8392.norm$Date, 5, 6)=='08')
d.y8392.normaug <- as.Date(as.character(y8392.norm.aug$Date), format='%Y%m%d')
y8392.norm.sep <- subset(y8392.norm, substr(y8392.norm$Date, 5, 6)=='09')
d.y8392.normsep <- as.Date(as.character(y8392.norm.sep$Date), format='%Y%m%d')
y8392.norm.oct <- subset(y8392.norm, substr(y8392.norm$Date, 5, 6)=='10')
d.y8392.normoct <- as.Date(as.character(y8392.norm.oct$Date), format='%Y%m%d')
y8392.norm.nov <- subset(y8392.norm, substr(y8392.norm$Date, 5, 6)=='11')
d.y8392.normnov <- as.Date(as.character(y8392.norm.nov$Date), format='%Y%m%d')
y8392.norm.dec <- subset(y8392.norm, substr(y8392.norm$Date, 5, 6)=='12')
d.y8392.normdec <- as.Date(as.character(y8392.norm.dec$Date), format='%Y%m%d')

#months y9302
y9302.norm.jan <- subset(y9302.norm, substr(y9302.norm$Date, 5, 6)=='01')
d.y9302.normjan <- as.Date(as.character(y9302.norm.jan$Date), format='%Y%m%d')
y9302.norm.feb <- subset(y9302.norm, substr(y9302.norm$Date, 5, 6)=='02')
d.y9302.normfeb <- as.Date(as.character(y9302.norm.feb$Date), format='%Y%m%d')
y9302.norm.mar <- subset(y9302.norm, substr(y9302.norm$Date, 5, 6)=='03')
d.y9302.normmar <- as.Date(as.character(y9302.norm.mar$Date), format='%Y%m%d')
y9302.norm.apr <- subset(y9302.norm, substr(y9302.norm$Date, 5, 6)=='04')
d.y9302.normapr <- as.Date(as.character(y9302.norm.apr$Date), format='%Y%m%d')
y9302.norm.may <- subset(y9302.norm, substr(y9302.norm$Date, 5, 6)=='05')
d.y9302.normmay <- as.Date(as.character(y9302.norm.may$Date), format='%Y%m%d')
y9302.norm.jun <- subset(y9302.norm, substr(y9302.norm$Date, 5, 6)=='06')
d.y9302.normjun <- as.Date(as.character(y9302.norm.jun$Date), format='%Y%m%d')
y9302.norm.jul <- subset(y9302.norm, substr(y9302.norm$Date, 5, 6)=='07')
d.y9302.normjul <- as.Date(as.character(y9302.norm.jul$Date), format='%Y%m%d')
y9302.norm.aug <- subset(y9302.norm, substr(y9302.norm$Date, 5, 6)=='08')
d.y9302.normaug <- as.Date(as.character(y9302.norm.aug$Date), format='%Y%m%d')
y9302.norm.sep <- subset(y9302.norm, substr(y9302.norm$Date, 5, 6)=='09')
d.y9302.normsep <- as.Date(as.character(y9302.norm.sep$Date), format='%Y%m%d')
y9302.norm.oct <- subset(y9302.norm, substr(y9302.norm$Date, 5, 6)=='10')
d.y9302.normoct <- as.Date(as.character(y9302.norm.oct$Date), format='%Y%m%d')
y9302.norm.nov <- subset(y9302.norm, substr(y9302.norm$Date, 5, 6)=='11')
d.y9302.normnov <- as.Date(as.character(y9302.norm.nov$Date), format='%Y%m%d')
y9302.norm.dec <- subset(y9302.norm, substr(y9302.norm$Date, 5, 6)=='12')
d.y9302.normdec <- as.Date(as.character(y9302.norm.dec$Date), format='%Y%m%d')

#months y0312
y0312.norm.jan <- subset(y0312.norm, substr(y0312.norm$Date, 5, 6)=='01')
d.y0312.normjan <- as.Date(as.character(y0312.norm.jan$Date), format='%Y%m%d')
y0312.norm.feb <- subset(y0312.norm, substr(y0312.norm$Date, 5, 6)=='02')
d.y0312.normfeb <- as.Date(as.character(y0312.norm.feb$Date), format='%Y%m%d')
y0312.norm.mar <- subset(y0312.norm, substr(y0312.norm$Date, 5, 6)=='03')
d.y0312.normmar <- as.Date(as.character(y0312.norm.mar$Date), format='%Y%m%d')
y0312.norm.apr <- subset(y0312.norm, substr(y0312.norm$Date, 5, 6)=='04')
d.y0312.normapr <- as.Date(as.character(y0312.norm.apr$Date), format='%Y%m%d')
y0312.norm.may <- subset(y0312.norm, substr(y0312.norm$Date, 5, 6)=='05')
d.y0312.normmay <- as.Date(as.character(y0312.norm.may$Date), format='%Y%m%d')
y0312.norm.jun <- subset(y0312.norm, substr(y0312.norm$Date, 5, 6)=='06')
d.y0312.normjun <- as.Date(as.character(y0312.norm.jun$Date), format='%Y%m%d')
y0312.norm.jul <- subset(y0312.norm, substr(y0312.norm$Date, 5, 6)=='07')
d.y0312.normjul <- as.Date(as.character(y0312.norm.jul$Date), format='%Y%m%d')
y0312.norm.aug <- subset(y0312.norm, substr(y0312.norm$Date, 5, 6)=='08')
d.y0312.normaug <- as.Date(as.character(y0312.norm.aug$Date), format='%Y%m%d')
y0312.norm.sep <- subset(y0312.norm, substr(y0312.norm$Date, 5, 6)=='09')
d.y0312.normsep <- as.Date(as.character(y0312.norm.sep$Date), format='%Y%m%d')
y0312.norm.oct <- subset(y0312.norm, substr(y0312.norm$Date, 5, 6)=='10')
d.y0312.normoct <- as.Date(as.character(y0312.norm.oct$Date), format='%Y%m%d')
y0312.norm.nov <- subset(y0312.norm, substr(y0312.norm$Date, 5, 6)=='11')
d.y0312.normnov <- as.Date(as.character(y0312.norm.nov$Date), format='%Y%m%d')
y0312.norm.dec <- subset(y0312.norm, substr(y0312.norm$Date, 5, 6)=='12')
d.y0312.normdec <- as.Date(as.character(y0312.norm.dec$Date), format='%Y%m%d')

#monthly average y8392
y8392sum.jan <- sum(y8392.norm.jan$Data)/160
y8392sum.feb <- sum(y8392.norm.feb$Data)/160
y8392sum.mar <- sum(y8392.norm.mar$Data)/160
y8392sum.apr <- sum(y8392.norm.apr$Data)/160
y8392sum.may <- sum(y8392.norm.may$Data)/160
y8392sum.jun <- sum(y8392.norm.jun$Data)/160
y8392sum.jul <- sum(y8392.norm.jul$Data)/160
y8392sum.aug <- sum(y8392.norm.aug$Data)/160
y8392sum.sep <- sum(y8392.norm.sep$Data)/160
y8392sum.oct <- sum(y8392.norm.oct$Data)/160
y8392sum.nov <- sum(y8392.norm.nov$Data)/160
y8392sum.dec <- sum(y8392.norm.dec$Data)/160

y8392.month.rain <- c(y8392sum.jan, y8392sum.feb, y8392sum.mar, y8392sum.apr, y8392sum.may, y8392sum.jun, y8392sum.jul, y8392sum.aug, y8392sum.sep, y8392sum.oct, y8392sum.nov, y8392sum.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, y8392.month.rain, main = "monthly average, y8392")

#monthly average y9302
y9302sum.jan <- sum(y9302.norm.jan$Data)/160
y9302sum.feb <- sum(y9302.norm.feb$Data)/160
y9302sum.mar <- sum(y9302.norm.mar$Data)/160
y9302sum.apr <- sum(y9302.norm.apr$Data)/160
y9302sum.may <- sum(y9302.norm.may$Data)/160
y9302sum.jun <- sum(y9302.norm.jun$Data)/160
y9302sum.jul <- sum(y9302.norm.jul$Data)/160
y9302sum.aug <- sum(y9302.norm.aug$Data)/160
y9302sum.sep <- sum(y9302.norm.sep$Data)/160
y9302sum.oct <- sum(y9302.norm.oct$Data)/160
y9302sum.nov <- sum(y9302.norm.nov$Data)/160
y9302sum.dec <- sum(y9302.norm.dec$Data)/160

y9302.month.rain <- c(y9302sum.jan, y9302sum.feb, y9302sum.mar, y9302sum.apr, y9302sum.may, y9302sum.jun, y9302sum.jul, y9302sum.aug, y9302sum.sep, y9302sum.oct, y9302sum.nov, y9302sum.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, y9302.month.rain, main = "monthly average, y9302")

#monthly average y0312
y0312sum.jan <- sum(y0312.norm.jan$Data)/160
y0312sum.feb <- sum(y0312.norm.feb$Data)/160
y0312sum.mar <- sum(y0312.norm.mar$Data)/160
y0312sum.apr <- sum(y0312.norm.apr$Data)/160
y0312sum.may <- sum(y0312.norm.may$Data)/160
y0312sum.jun <- sum(y0312.norm.jun$Data)/160
y0312sum.jul <- sum(y0312.norm.jul$Data)/160
y0312sum.aug <- sum(y0312.norm.aug$Data)/160
y0312sum.sep <- sum(y0312.norm.sep$Data)/160
y0312sum.oct <- sum(y0312.norm.oct$Data)/160
y0312sum.nov <- sum(y0312.norm.nov$Data)/160
y0312sum.dec <- sum(y0312.norm.dec$Data)/160

y0312.month.rain <- c(y0312sum.jan, y0312sum.feb, y0312sum.mar, y0312sum.apr, y0312sum.may, y0312sum.jun, y0312sum.jul, y0312sum.aug, y0312sum.sep, y0312sum.oct, y0312sum.nov, y0312sum.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, y0312.month.rain, main = "monthly average, y0312")

wilcox.test(y8392.month.rain, y9302.month.rain, paired = TRUE, conf.int = TRUE)
wilcox.test(y9302.month.rain, y0312.month.rain, paired = TRUE, conf.int = TRUE)
wilcox.test(y0312.month.rain, y8392.month.rain, paired = TRUE, conf.int = TRUE)