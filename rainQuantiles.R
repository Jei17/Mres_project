
y8392 <- subset(dataNew.norm, substr(dataNew.norm$Date, 1,4)==1983|substr(dataNew.norm$Date, 1,4)==1984|substr(dataNew.norm$Date, 1,4)==1985|substr(dataNew.norm$Date, 1,4)==1986|substr(dataNew.norm$Date, 1,4)==1987|substr(dataNew.norm$Date, 1,4)==1988|substr(dataNew.norm$Date, 1,4)==1989|substr(dataNew.norm$Date, 1,4)==1990|substr(dataNew.norm$Date, 1,4)==1991|substr(dataNew.norm$Date, 1,4)==1992)
y9302 <- subset(dataNew.norm, substr(dataNew.norm$Date, 1, 4)==1993|substr(dataNew.norm$Date, 1, 4)==1994|substr(dataNew.norm$Date, 1, 4)==1995|substr(dataNew.norm$Date, 1, 4)==1996|substr(dataNew.norm$Date, 1, 4)==1997|substr(dataNew.norm$Date, 1, 4)==1998|substr(dataNew.norm$Date, 1, 4)==1999|substr(dataNew.norm$Date, 1, 4)==2000|substr(dataNew.norm$Date, 1, 4)==2001|substr(dataNew.norm$Date, 1, 4)==2002)
y0312 <- subset(dataNew.norm, substr(dataNew.norm$Date, 1, 4)==2003|substr(dataNew.norm$Date, 1, 4)==2004|substr(dataNew.norm$Date, 1, 4)==2005|substr(dataNew.norm$Date, 1, 4)==2006|substr(dataNew.norm$Date, 1, 4)==2007|substr(dataNew.norm$Date, 1, 4)==2008|substr(dataNew.norm$Date, 1, 4)==2009|substr(dataNew.norm$Date, 1, 4)==2010|substr(dataNew.norm$Date, 1, 4)==2011|substr(dataNew.norm$Date, 1, 4)==2012)


Uni8392 <- subset(y8392, Station=='07008KRA' | Station=='07000BOL' | Station=='07006TLE' | Station=='04003NAV1')
#6 stations, 21306 pos obs
Bi8392 <- subset(y8392, Station=='21088ODA' | Station=='22050KDA' | Station=='17009KSI' | Station=='07017HO' | Station=='01032SUN'|Station=='01018WEN')
#6 stations, 16280 pos obs
Semi8392 <- subset(y8392, Station=='23001AXM' | Station=='23003TDI' | Station=='23022SAL' | Station=='23016ACC' | Station=='23002ADA' | Station=='23024TEM')

quantile(Uni8392$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Bi8392$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Semi8392$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)

Uni9302 <- subset(y9302, Station=='07008KRA' | Station=='07000BOL' | Station=='07006TLE' | Station=='04003NAV1')
#6 stations, 21306 pos obs
Bi9302 <- subset(y9302, Station=='21088ODA' | Station=='22050KDA' | Station=='17009KSI' | Station=='07017HO' | Station=='01032SUN'|Station=='01018WEN')
#6 stations, 16280 pos obs
Semi9302 <- subset(y9302, Station=='23001AXM' | Station=='23003TDI' | Station=='23022SAL' | Station=='23016ACC' | Station=='23002ADA' | Station=='23024TEM')

quantile(Uni9302$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Bi9302$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Semi9302$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)

Uni0312 <- subset(y0312, Station=='07008KRA' | Station=='07000BOL' | Station=='07006TLE' | Station=='04003NAV1')
#6 stations, 21306 pos obs
Bi0312 <- subset(y0312, Station=='21088ODA' | Station=='22050KDA' | Station=='17009KSI' | Station=='07017HO' | Station=='01032SUN'|Station=='01018WEN')
#6 stations, 16280 pos obs
Semi0312 <- subset(y0312, Station=='23001AXM' | Station=='23003TDI' | Station=='23022SAL' | Station=='23016ACC' | Station=='23002ADA' | Station=='23024TEM')

quantile(Uni0312$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Bi0312$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Semi0312$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)


y8392AS <- subset(y8392, substr(y8392$Date,5,6)=='04'|substr(y8392$Date,5,6)=='05'|substr(y8392$Date,5,6)=='06'|substr(y8392$Date,5,6)=='07'|substr(y8392$Date,5,6)=='08'|substr(y8392$Date,5,6)=='09')
y8392JD <- subset(y8392, substr(y8392$Date,5,6)=='01'|substr(y8392$Date,5,6)=='02'|substr(y8392$Date,5,6)=='03'|substr(y8392$Date,5,6)=='10'|substr(y8392$Date,5,6)=='11'|substr(y8392$Date,5,6)=='12')
y9302AS <- subset(y9302, substr(y9302$Date,5,6)=='04'|substr(y9302$Date,5,6)=='05'|substr(y9302$Date,5,6)=='06'|substr(y9302$Date,5,6)=='07'|substr(y9302$Date,5,6)=='08'|substr(y9302$Date,5,6)=='09')
y9302JD <- subset(y9302, substr(y9302$Date,5,6)=='01'|substr(y9302$Date,5,6)=='02'|substr(y9302$Date,5,6)=='03'|substr(y9302$Date,5,6)=='10'|substr(y9302$Date,5,6)=='11'|substr(y9302$Date,5,6)=='12')
y0312AS <- subset(y0312, substr(y0312$Date,5,6)=='04'|substr(y0312$Date,5,6)=='05'|substr(y0312$Date,5,6)=='06'|substr(y0312$Date,5,6)=='07'|substr(y0312$Date,5,6)=='08'|substr(y0312$Date,5,6)=='09')
y0312JD <- subset(y0312, substr(y0312$Date,5,6)=='01'|substr(y0312$Date,5,6)=='02'|substr(y0312$Date,5,6)=='03'|substr(y0312$Date,5,6)=='10'|substr(y0312$Date,5,6)=='11'|substr(y0312$Date,5,6)=='12')

Uni8392AS <- subset(y8392AS, Station=='07008KRA' | Station=='07000BOL' | Station=='07006TLE' | Station=='04003NAV1')
#6 stations, 21306 pos obs
Bi8392AS <- subset(y8392AS, Station=='21088ODA' | Station=='22050KDA' | Station=='17009KSI' | Station=='07017HO' | Station=='01032SUN'|Station=='01018WEN')
#6 stations, 16280 pos obs
Semi8392AS <- subset(y8392AS, Station=='23001AXM' | Station=='23003TDI' | Station=='23022SAL' | Station=='23016ACC' | Station=='23002ADA' | Station=='23024TEM')
Uni8392JD <- subset(y8392JD, Station=='07008KRA' | Station=='07000BOL' | Station=='07006TLE' | Station=='04003NAV1')
#6 stations, 21306 pos obs
Bi8392JD <- subset(y8392JD, Station=='21088ODA' | Station=='22050KDA' | Station=='17009KSI' | Station=='07017HO' | Station=='01032SUN'|Station=='01018WEN')
#6 stations, 16280 pos obs
Semi8392JD <- subset(y8392JD, Station=='23001AXM' | Station=='23003TDI' | Station=='23022SAL' | Station=='23016ACC' | Station=='23002ADA' | Station=='23024TEM')

quantile(Uni8392AS$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Bi8392AS$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Semi8392AS$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Uni8392JD$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Bi8392JD$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Semi8392JD$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)

Uni9302AS <- subset(y9302AS, Station=='07008KRA' | Station=='07000BOL' | Station=='07006TLE' | Station=='04003NAV1')
#6 stations, 21306 pos obs
Bi9302AS <- subset(y9302AS, Station=='21088ODA' | Station=='22050KDA' | Station=='17009KSI' | Station=='07017HO' | Station=='01032SUN'|Station=='01018WEN')
#6 stations, 16280 pos obs
Semi9302AS <- subset(y9302AS, Station=='23001AXM' | Station=='23003TDI' | Station=='23022SAL' | Station=='23016ACC' | Station=='23002ADA' | Station=='23024TEM')
Uni9302JD <- subset(y9302JD, Station=='07008KRA' | Station=='07000BOL' | Station=='07006TLE' | Station=='04003NAV1')
#6 stations, 21306 pos obs
Bi9302JD <- subset(y9302JD, Station=='21088ODA' | Station=='22050KDA' | Station=='17009KSI' | Station=='07017HO' | Station=='01032SUN'|Station=='01018WEN')
#6 stations, 16280 pos obs
Semi9302JD <- subset(y9302JD, Station=='23001AXM' | Station=='23003TDI' | Station=='23022SAL' | Station=='23016ACC' | Station=='23002ADA' | Station=='23024TEM')

quantile(Uni9302AS$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Bi9302AS$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Semi9302AS$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Uni9302JD$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Bi9302JD$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Semi9302JD$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)

Uni0312AS <- subset(y0312AS, Station=='07008KRA' | Station=='07000BOL' | Station=='07006TLE' | Station=='04003NAV1')
#6 stations, 21306 pos obs
Bi0312AS <- subset(y0312AS, Station=='21088ODA' | Station=='22050KDA' | Station=='17009KSI' | Station=='07017HO' | Station=='01032SUN'|Station=='01018WEN')
#6 stations, 16280 pos obs
Semi0312AS <- subset(y0312AS, Station=='23001AXM' | Station=='23003TDI' | Station=='23022SAL' | Station=='23016ACC' | Station=='23002ADA' | Station=='23024TEM')
Uni0312JD <- subset(y0312JD, Station=='07008KRA' | Station=='07000BOL' | Station=='07006TLE' | Station=='04003NAV1')
#6 stations, 21306 pos obs
Bi0312JD <- subset(y0312JD, Station=='21088ODA' | Station=='22050KDA' | Station=='17009KSI' | Station=='07017HO' | Station=='01032SUN'|Station=='01018WEN')
#6 stations, 16280 pos obs
Semi0312JD <- subset(y0312JD, Station=='23001AXM' | Station=='23003TDI' | Station=='23022SAL' | Station=='23016ACC' | Station=='23002ADA' | Station=='23024TEM')

quantile(Uni0312AS$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Bi0312AS$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Semi0312AS$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Uni0312JD$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Bi0312JD$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)
quantile(Semi0312JD$Data, probs = c(0.10, 0.50, 0.90, 0.99), type = 5)

