AXM.jan <- subset(AXM, substr(AXM$Date, 5, 6)=='01')
AXM.feb <- subset(AXM, substr(AXM$Date, 5, 6)=='02')
AXM.mar <- subset(AXM, substr(AXM$Date, 5, 6)=='03')
AXM.apr <- subset(AXM, substr(AXM$Date, 5, 6)=='04')
AXM.may <- subset(AXM, substr(AXM$Date, 5, 6)=='05')
AXM.jun <- subset(AXM, substr(AXM$Date, 5, 6)=='06')
AXM.jul <- subset(AXM, substr(AXM$Date, 5, 6)=='07')
AXM.aug <- subset(AXM, substr(AXM$Date, 5, 6)=='08')
AXM.sep <- subset(AXM, substr(AXM$Date, 5, 6)=='09')
AXM.oct <- subset(AXM, substr(AXM$Date, 5, 6)=='10')
AXM.nov <- subset(AXM, substr(AXM$Date, 5, 6)=='11')
AXM.dec <- subset(AXM, substr(AXM$Date, 5, 6)=='12')

AXMmean.jan <- sum(AXM.jan$Data)/30
AXMmean.feb <- sum(AXM.feb$Data)/30
AXMmean.mar <- sum(AXM.mar$Data)/30
AXMmean.apr <- sum(AXM.apr$Data)/30
AXMmean.may <- sum(AXM.may$Data)/30
AXMmean.jun <- sum(AXM.jun$Data)/30
AXMmean.jul <- sum(AXM.jul$Data)/30
AXMmean.aug <- sum(AXM.aug$Data)/30
AXMmean.sep <- sum(AXM.sep$Data)/30
AXMmean.oct <- sum(AXM.oct$Data)/30
AXMmean.nov <- sum(AXM.nov$Data)/30
AXMmean.dec <- sum(AXM.dec$Data)/30

AXM.month.rain <- c(AXMmean.jan, AXMmean.feb, AXMmean.mar, AXMmean.apr, AXMmean.may, AXMmean.jun, AXMmean.jul, AXMmean.aug, AXMmean.sep, AXMmean.oct, AXMmean.nov, AXMmean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, AXM.month.rain, main = "monthly average, AXM")

ODA.jan <- subset(ODA, substr(ODA$Date, 5, 6)=='01')
ODA.feb <- subset(ODA, substr(ODA$Date, 5, 6)=='02')
ODA.mar <- subset(ODA, substr(ODA$Date, 5, 6)=='03')
ODA.apr <- subset(ODA, substr(ODA$Date, 5, 6)=='04')
ODA.may <- subset(ODA, substr(ODA$Date, 5, 6)=='05')
ODA.jun <- subset(ODA, substr(ODA$Date, 5, 6)=='06')
ODA.jul <- subset(ODA, substr(ODA$Date, 5, 6)=='07')
ODA.aug <- subset(ODA, substr(ODA$Date, 5, 6)=='08')
ODA.sep <- subset(ODA, substr(ODA$Date, 5, 6)=='09')
ODA.oct <- subset(ODA, substr(ODA$Date, 5, 6)=='10')
ODA.nov <- subset(ODA, substr(ODA$Date, 5, 6)=='11')
ODA.dec <- subset(ODA, substr(ODA$Date, 5, 6)=='12')

ODAmean.jan <- sum(ODA.jan$Data)/30
ODAmean.feb <- sum(ODA.feb$Data)/30
ODAmean.mar <- sum(ODA.mar$Data)/30
ODAmean.apr <- sum(ODA.apr$Data)/30
ODAmean.may <- sum(ODA.may$Data)/30
ODAmean.jun <- sum(ODA.jun$Data)/30
ODAmean.jul <- sum(ODA.jul$Data)/30
ODAmean.aug <- sum(ODA.aug$Data)/30
ODAmean.sep <- sum(ODA.sep$Data)/30
ODAmean.oct <- sum(ODA.oct$Data)/30
ODAmean.nov <- sum(ODA.nov$Data)/30
ODAmean.dec <- sum(ODA.dec$Data)/30

ODA.month.rain <- c(ODAmean.jan, ODAmean.feb, ODAmean.mar, ODAmean.apr, ODAmean.may, ODAmean.jun, ODAmean.jul, ODAmean.aug, ODAmean.sep, ODAmean.oct, ODAmean.nov, ODAmean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, ODA.month.rain, main = "monthly average, ODA")

KDA.jan <- subset(KDA, substr(KDA$Date, 5, 6)=='01')
KDA.feb <- subset(KDA, substr(KDA$Date, 5, 6)=='02')
KDA.mar <- subset(KDA, substr(KDA$Date, 5, 6)=='03')
KDA.apr <- subset(KDA, substr(KDA$Date, 5, 6)=='04')
KDA.may <- subset(KDA, substr(KDA$Date, 5, 6)=='05')
KDA.jun <- subset(KDA, substr(KDA$Date, 5, 6)=='06')
KDA.jul <- subset(KDA, substr(KDA$Date, 5, 6)=='07')
KDA.aug <- subset(KDA, substr(KDA$Date, 5, 6)=='08')
KDA.sep <- subset(KDA, substr(KDA$Date, 5, 6)=='09')
KDA.oct <- subset(KDA, substr(KDA$Date, 5, 6)=='10')
KDA.nov <- subset(KDA, substr(KDA$Date, 5, 6)=='11')
KDA.dec <- subset(KDA, substr(KDA$Date, 5, 6)=='12')

KDAmean.jan <- sum(KDA.jan$Data)/30
KDAmean.feb <- sum(KDA.feb$Data)/30
KDAmean.mar <- sum(KDA.mar$Data)/30
KDAmean.apr <- sum(KDA.apr$Data)/30
KDAmean.may <- sum(KDA.may$Data)/30
KDAmean.jun <- sum(KDA.jun$Data)/30
KDAmean.jul <- sum(KDA.jul$Data)/30
KDAmean.aug <- sum(KDA.aug$Data)/30
KDAmean.sep <- sum(KDA.sep$Data)/30
KDAmean.oct <- sum(KDA.oct$Data)/30
KDAmean.nov <- sum(KDA.nov$Data)/30
KDAmean.dec <- sum(KDA.dec$Data)/30

KDA.month.rain <- c(KDAmean.jan, KDAmean.feb, KDAmean.mar, KDAmean.apr, KDAmean.may, KDAmean.jun, KDAmean.jul, KDAmean.aug, KDAmean.sep, KDAmean.oct, KDAmean.nov, KDAmean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, KDA.month.rain, main = "monthly average, KDA")

KSI.jan <- subset(KSI, substr(KSI$Date, 5, 6)=='01')
KSI.feb <- subset(KSI, substr(KSI$Date, 5, 6)=='02')
KSI.mar <- subset(KSI, substr(KSI$Date, 5, 6)=='03')
KSI.apr <- subset(KSI, substr(KSI$Date, 5, 6)=='04')
KSI.may <- subset(KSI, substr(KSI$Date, 5, 6)=='05')
KSI.jun <- subset(KSI, substr(KSI$Date, 5, 6)=='06')
KSI.jul <- subset(KSI, substr(KSI$Date, 5, 6)=='07')
KSI.aug <- subset(KSI, substr(KSI$Date, 5, 6)=='08')
KSI.sep <- subset(KSI, substr(KSI$Date, 5, 6)=='09')
KSI.oct <- subset(KSI, substr(KSI$Date, 5, 6)=='10')
KSI.nov <- subset(KSI, substr(KSI$Date, 5, 6)=='11')
KSI.dec <- subset(KSI, substr(KSI$Date, 5, 6)=='12')

KSImean.jan <- sum(KSI.jan$Data)/30
KSImean.feb <- sum(KSI.feb$Data)/30
KSImean.mar <- sum(KSI.mar$Data)/30
KSImean.apr <- sum(KSI.apr$Data)/30
KSImean.may <- sum(KSI.may$Data)/30
KSImean.jun <- sum(KSI.jun$Data)/30
KSImean.jul <- sum(KSI.jul$Data)/30
KSImean.aug <- sum(KSI.aug$Data)/30
KSImean.sep <- sum(KSI.sep$Data)/30
KSImean.oct <- sum(KSI.oct$Data)/30
KSImean.nov <- sum(KSI.nov$Data)/30
KSImean.dec <- sum(KSI.dec$Data)/30

KSI.month.rain <- c(KSImean.jan, KSImean.feb, KSImean.mar, KSImean.apr, KSImean.may, KSImean.jun, KSImean.jul, KSImean.aug, KSImean.sep, KSImean.oct, KSImean.nov, KSImean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, KSI.month.rain, main = "monthly average, KSI")

HO.jan <- subset(HO, substr(HO$Date, 5, 6)=='01')
HO.feb <- subset(HO, substr(HO$Date, 5, 6)=='02')
HO.mar <- subset(HO, substr(HO$Date, 5, 6)=='03')
HO.apr <- subset(HO, substr(HO$Date, 5, 6)=='04')
HO.may <- subset(HO, substr(HO$Date, 5, 6)=='05')
HO.jun <- subset(HO, substr(HO$Date, 5, 6)=='06')
HO.jul <- subset(HO, substr(HO$Date, 5, 6)=='07')
HO.aug <- subset(HO, substr(HO$Date, 5, 6)=='08')
HO.sep <- subset(HO, substr(HO$Date, 5, 6)=='09')
HO.oct <- subset(HO, substr(HO$Date, 5, 6)=='10')
HO.nov <- subset(HO, substr(HO$Date, 5, 6)=='11')
HO.dec <- subset(HO, substr(HO$Date, 5, 6)=='12')

HOmean.jan <- sum(HO.jan$Data)/30
HOmean.feb <- sum(HO.feb$Data)/30
HOmean.mar <- sum(HO.mar$Data)/30
HOmean.apr <- sum(HO.apr$Data)/30
HOmean.may <- sum(HO.may$Data)/30
HOmean.jun <- sum(HO.jun$Data)/30
HOmean.jul <- sum(HO.jul$Data)/30
HOmean.aug <- sum(HO.aug$Data)/30
HOmean.sep <- sum(HO.sep$Data)/30
HOmean.oct <- sum(HO.oct$Data)/30
HOmean.nov <- sum(HO.nov$Data)/30
HOmean.dec <- sum(HO.dec$Data)/30

HO.month.rain <- c(HOmean.jan, HOmean.feb, HOmean.mar, HOmean.apr, HOmean.may, HOmean.jun, HOmean.jul, HOmean.aug, HOmean.sep, HOmean.oct, HOmean.nov, HOmean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, HO.month.rain, main = "monthly average, HO")

TDI.jan <- subset(TDI, substr(TDI$Date, 5, 6)=='01')
TDI.feb <- subset(TDI, substr(TDI$Date, 5, 6)=='02')
TDI.mar <- subset(TDI, substr(TDI$Date, 5, 6)=='03')
TDI.apr <- subset(TDI, substr(TDI$Date, 5, 6)=='04')
TDI.may <- subset(TDI, substr(TDI$Date, 5, 6)=='05')
TDI.jun <- subset(TDI, substr(TDI$Date, 5, 6)=='06')
TDI.jul <- subset(TDI, substr(TDI$Date, 5, 6)=='07')
TDI.aug <- subset(TDI, substr(TDI$Date, 5, 6)=='08')
TDI.sep <- subset(TDI, substr(TDI$Date, 5, 6)=='09')
TDI.oct <- subset(TDI, substr(TDI$Date, 5, 6)=='10')
TDI.nov <- subset(TDI, substr(TDI$Date, 5, 6)=='11')
TDI.dec <- subset(TDI, substr(TDI$Date, 5, 6)=='12')

TDImean.jan <- sum(TDI.jan$Data)/30
TDImean.feb <- sum(TDI.feb$Data)/29
TDImean.mar <- sum(TDI.mar$Data)/30
TDImean.apr <- sum(TDI.apr$Data)/30
TDImean.may <- sum(TDI.may$Data)/30
TDImean.jun <- sum(TDI.jun$Data)/30
TDImean.jul <- sum(TDI.jul$Data)/30
TDImean.aug <- sum(TDI.aug$Data)/30
TDImean.sep <- sum(TDI.sep$Data)/30
TDImean.oct <- sum(TDI.oct$Data)/30
TDImean.nov <- sum(TDI.nov$Data)/30
TDImean.dec <- sum(TDI.dec$Data)/30

TDI.month.rain <- c(TDImean.jan, TDImean.feb, TDImean.mar, TDImean.apr, TDImean.may, TDImean.jun, TDImean.jul, TDImean.aug, TDImean.sep, TDImean.oct, TDImean.nov, TDImean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, TDI.month.rain, main = "monthly average, TDI")

SUN.jan <- subset(SUN, substr(SUN$Date, 5, 6)=='01')
SUN.feb <- subset(SUN, substr(SUN$Date, 5, 6)=='02')
SUN.mar <- subset(SUN, substr(SUN$Date, 5, 6)=='03')
SUN.apr <- subset(SUN, substr(SUN$Date, 5, 6)=='04')
SUN.may <- subset(SUN, substr(SUN$Date, 5, 6)=='05')
SUN.jun <- subset(SUN, substr(SUN$Date, 5, 6)=='06')
SUN.jul <- subset(SUN, substr(SUN$Date, 5, 6)=='07')
SUN.aug <- subset(SUN, substr(SUN$Date, 5, 6)=='08')
SUN.sep <- subset(SUN, substr(SUN$Date, 5, 6)=='09')
SUN.oct <- subset(SUN, substr(SUN$Date, 5, 6)=='10')
SUN.nov <- subset(SUN, substr(SUN$Date, 5, 6)=='11')
SUN.dec <- subset(SUN, substr(SUN$Date, 5, 6)=='12')

SUNmean.jan <- sum(SUN.jan$Data)/30
SUNmean.feb <- sum(SUN.feb$Data)/30
SUNmean.mar <- sum(SUN.mar$Data)/30
SUNmean.apr <- sum(SUN.apr$Data)/30
SUNmean.may <- sum(SUN.may$Data)/30
SUNmean.jun <- sum(SUN.jun$Data)/30
SUNmean.jul <- sum(SUN.jul$Data)/30
SUNmean.aug <- sum(SUN.aug$Data)/30
SUNmean.sep <- sum(SUN.sep$Data)/30
SUNmean.oct <- sum(SUN.oct$Data)/30
SUNmean.nov <- sum(SUN.nov$Data)/30
SUNmean.dec <- sum(SUN.dec$Data)/30

SUN.month.rain <- c(SUNmean.jan, SUNmean.feb, SUNmean.mar, SUNmean.apr, SUNmean.may, SUNmean.jun, SUNmean.jul, SUNmean.aug, SUNmean.sep, SUNmean.oct, SUNmean.nov, SUNmean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, SUN.month.rain, main = "monthly average, SUN")

WEN.jan <- subset(WEN, substr(WEN$Date, 5, 6)=='01')
WEN.feb <- subset(WEN, substr(WEN$Date, 5, 6)=='02')
WEN.mar <- subset(WEN, substr(WEN$Date, 5, 6)=='03')
WEN.apr <- subset(WEN, substr(WEN$Date, 5, 6)=='04')
WEN.may <- subset(WEN, substr(WEN$Date, 5, 6)=='05')
WEN.jun <- subset(WEN, substr(WEN$Date, 5, 6)=='06')
WEN.jul <- subset(WEN, substr(WEN$Date, 5, 6)=='07')
WEN.aug <- subset(WEN, substr(WEN$Date, 5, 6)=='08')
WEN.sep <- subset(WEN, substr(WEN$Date, 5, 6)=='09')
WEN.oct <- subset(WEN, substr(WEN$Date, 5, 6)=='10')
WEN.nov <- subset(WEN, substr(WEN$Date, 5, 6)=='11')
WEN.dec <- subset(WEN, substr(WEN$Date, 5, 6)=='12')

WENmean.jan <- sum(WEN.jan$Data)/30
WENmean.feb <- sum(WEN.feb$Data)/30
WENmean.mar <- sum(WEN.mar$Data)/30
WENmean.apr <- sum(WEN.apr$Data)/30
WENmean.may <- sum(WEN.may$Data)/30
WENmean.jun <- sum(WEN.jun$Data)/30
WENmean.jul <- sum(WEN.jul$Data)/30
WENmean.aug <- sum(WEN.aug$Data)/30
WENmean.sep <- sum(WEN.sep$Data)/30
WENmean.oct <- sum(WEN.oct$Data)/30
WENmean.nov <- sum(WEN.nov$Data)/30
WENmean.dec <- sum(WEN.dec$Data)/30

WEN.month.rain <- c(WENmean.jan, WENmean.feb, WENmean.mar, WENmean.apr, WENmean.may, WENmean.jun, WENmean.jul, WENmean.aug, WENmean.sep, WENmean.oct, WENmean.nov, WENmean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, WEN.month.rain, main = "monthly average, WEN")

KRA.jan <- subset(KRA, substr(KRA$Date, 5, 6)=='01')
KRA.feb <- subset(KRA, substr(KRA$Date, 5, 6)=='02')
KRA.mar <- subset(KRA, substr(KRA$Date, 5, 6)=='03')
KRA.apr <- subset(KRA, substr(KRA$Date, 5, 6)=='04')
KRA.may <- subset(KRA, substr(KRA$Date, 5, 6)=='05')
KRA.jun <- subset(KRA, substr(KRA$Date, 5, 6)=='06')
KRA.jul <- subset(KRA, substr(KRA$Date, 5, 6)=='07')
KRA.aug <- subset(KRA, substr(KRA$Date, 5, 6)=='08')
KRA.sep <- subset(KRA, substr(KRA$Date, 5, 6)=='09')
KRA.oct <- subset(KRA, substr(KRA$Date, 5, 6)=='10')
KRA.nov <- subset(KRA, substr(KRA$Date, 5, 6)=='11')
KRA.dec <- subset(KRA, substr(KRA$Date, 5, 6)=='12')

KRAmean.jan <- sum(KRA.jan$Data)/30
KRAmean.feb <- sum(KRA.feb$Data)/30
KRAmean.mar <- sum(KRA.mar$Data)/30
KRAmean.apr <- sum(KRA.apr$Data)/30
KRAmean.may <- sum(KRA.may$Data)/30
KRAmean.jun <- sum(KRA.jun$Data)/30
KRAmean.jul <- sum(KRA.jul$Data)/30
KRAmean.aug <- sum(KRA.aug$Data)/30
KRAmean.sep <- sum(KRA.sep$Data)/30
KRAmean.oct <- sum(KRA.oct$Data)/30
KRAmean.nov <- sum(KRA.nov$Data)/30
KRAmean.dec <- sum(KRA.dec$Data)/30

KRA.month.rain <- c(KRAmean.jan, KRAmean.feb, KRAmean.mar, KRAmean.apr, KRAmean.may, KRAmean.jun, KRAmean.jul, KRAmean.aug, KRAmean.sep, KRAmean.oct, KRAmean.nov, KRAmean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, KRA.month.rain, main = "monthly average, KRA")

BOL.jan <- subset(BOL, substr(BOL$Date, 5, 6)=='01')
BOL.feb <- subset(BOL, substr(BOL$Date, 5, 6)=='02')
BOL.mar <- subset(BOL, substr(BOL$Date, 5, 6)=='03')
BOL.apr <- subset(BOL, substr(BOL$Date, 5, 6)=='04')
BOL.may <- subset(BOL, substr(BOL$Date, 5, 6)=='05')
BOL.jun <- subset(BOL, substr(BOL$Date, 5, 6)=='06')
BOL.jul <- subset(BOL, substr(BOL$Date, 5, 6)=='07')
BOL.aug <- subset(BOL, substr(BOL$Date, 5, 6)=='08')
BOL.sep <- subset(BOL, substr(BOL$Date, 5, 6)=='09')
BOL.oct <- subset(BOL, substr(BOL$Date, 5, 6)=='10')
BOL.nov <- subset(BOL, substr(BOL$Date, 5, 6)=='11')
BOL.dec <- subset(BOL, substr(BOL$Date, 5, 6)=='12')

BOLmean.jan <- sum(BOL.jan$Data)/30
BOLmean.feb <- sum(BOL.feb$Data)/30
BOLmean.mar <- sum(BOL.mar$Data)/30
BOLmean.apr <- sum(BOL.apr$Data)/30
BOLmean.may <- sum(BOL.may$Data)/30
BOLmean.jun <- sum(BOL.jun$Data)/30
BOLmean.jul <- sum(BOL.jul$Data)/30
BOLmean.aug <- sum(BOL.aug$Data)/30
BOLmean.sep <- sum(BOL.sep$Data)/30
BOLmean.oct <- sum(BOL.oct$Data)/30
BOLmean.nov <- sum(BOL.nov$Data)/30
BOLmean.dec <- sum(BOL.dec$Data)/30

BOL.month.rain <- c(BOLmean.jan, BOLmean.feb, BOLmean.mar, BOLmean.apr, BOLmean.may, BOLmean.jun, BOLmean.jul, BOLmean.aug, BOLmean.sep, BOLmean.oct, BOLmean.nov, BOLmean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, BOL.month.rain, main = "monthly average, BOL")

SAL.jan <- subset(SAL, substr(SAL$Date, 5, 6)=='01')
SAL.feb <- subset(SAL, substr(SAL$Date, 5, 6)=='02')
SAL.mar <- subset(SAL, substr(SAL$Date, 5, 6)=='03')
SAL.apr <- subset(SAL, substr(SAL$Date, 5, 6)=='04')
SAL.may <- subset(SAL, substr(SAL$Date, 5, 6)=='05')
SAL.jun <- subset(SAL, substr(SAL$Date, 5, 6)=='06')
SAL.jul <- subset(SAL, substr(SAL$Date, 5, 6)=='07')
SAL.aug <- subset(SAL, substr(SAL$Date, 5, 6)=='08')
SAL.sep <- subset(SAL, substr(SAL$Date, 5, 6)=='09')
SAL.oct <- subset(SAL, substr(SAL$Date, 5, 6)=='10')
SAL.nov <- subset(SAL, substr(SAL$Date, 5, 6)=='11')
SAL.dec <- subset(SAL, substr(SAL$Date, 5, 6)=='12')

SALmean.jan <- sum(SAL.jan$Data)/30
SALmean.feb <- sum(SAL.feb$Data)/30
SALmean.mar <- sum(SAL.mar$Data)/30
SALmean.apr <- sum(SAL.apr$Data)/30
SALmean.may <- sum(SAL.may$Data)/30
SALmean.jun <- sum(SAL.jun$Data)/30
SALmean.jul <- sum(SAL.jul$Data)/30
SALmean.aug <- sum(SAL.aug$Data)/30
SALmean.sep <- sum(SAL.sep$Data)/30
SALmean.oct <- sum(SAL.oct$Data)/30
SALmean.nov <- sum(SAL.nov$Data)/30
SALmean.dec <- sum(SAL.dec$Data)/30

SAL.month.rain <- c(SALmean.jan, SALmean.feb, SALmean.mar, SALmean.apr, SALmean.may, SALmean.jun, SALmean.jul, SALmean.aug, SALmean.sep, SALmean.oct, SALmean.nov, SALmean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, SAL.month.rain, main = "monthly average, SAL")

TLE.jan <- subset(TLE, substr(TLE$Date, 5, 6)=='01')
TLE.feb <- subset(TLE, substr(TLE$Date, 5, 6)=='02')
TLE.mar <- subset(TLE, substr(TLE$Date, 5, 6)=='03')
TLE.apr <- subset(TLE, substr(TLE$Date, 5, 6)=='04')
TLE.may <- subset(TLE, substr(TLE$Date, 5, 6)=='05')
TLE.jun <- subset(TLE, substr(TLE$Date, 5, 6)=='06')
TLE.jul <- subset(TLE, substr(TLE$Date, 5, 6)=='07')
TLE.aug <- subset(TLE, substr(TLE$Date, 5, 6)=='08')
TLE.sep <- subset(TLE, substr(TLE$Date, 5, 6)=='09')
TLE.oct <- subset(TLE, substr(TLE$Date, 5, 6)=='10')
TLE.nov <- subset(TLE, substr(TLE$Date, 5, 6)=='11')
TLE.dec <- subset(TLE, substr(TLE$Date, 5, 6)=='12')

TLEmean.jan <- sum(TLE.jan$Data)/30
TLEmean.feb <- sum(TLE.feb$Data)/30
TLEmean.mar <- sum(TLE.mar$Data)/30
TLEmean.apr <- sum(TLE.apr$Data)/30
TLEmean.may <- sum(TLE.may$Data)/30
TLEmean.jun <- sum(TLE.jun$Data)/30
TLEmean.jul <- sum(TLE.jul$Data)/30
TLEmean.aug <- sum(TLE.aug$Data)/30
TLEmean.sep <- sum(TLE.sep$Data)/30
TLEmean.oct <- sum(TLE.oct$Data)/30
TLEmean.nov <- sum(TLE.nov$Data)/30
TLEmean.dec <- sum(TLE.dec$Data)/30

TLE.month.rain <- c(TLEmean.jan, TLEmean.feb, TLEmean.mar, TLEmean.apr, TLEmean.may, TLEmean.jun, TLEmean.jul, TLEmean.aug, TLEmean.sep, TLEmean.oct, TLEmean.nov, TLEmean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, TLE.month.rain, main = "monthly average, TLE")

NAV1.jan <- subset(NAV1, substr(NAV1$Date, 5, 6)=='01')
NAV1.feb <- subset(NAV1, substr(NAV1$Date, 5, 6)=='02')
NAV1.mar <- subset(NAV1, substr(NAV1$Date, 5, 6)=='03')
NAV1.apr <- subset(NAV1, substr(NAV1$Date, 5, 6)=='04')
NAV1.may <- subset(NAV1, substr(NAV1$Date, 5, 6)=='05')
NAV1.jun <- subset(NAV1, substr(NAV1$Date, 5, 6)=='06')
NAV1.jul <- subset(NAV1, substr(NAV1$Date, 5, 6)=='07')
NAV1.aug <- subset(NAV1, substr(NAV1$Date, 5, 6)=='08')
NAV1.sep <- subset(NAV1, substr(NAV1$Date, 5, 6)=='09')
NAV1.oct <- subset(NAV1, substr(NAV1$Date, 5, 6)=='10')
NAV1.nov <- subset(NAV1, substr(NAV1$Date, 5, 6)=='11')
NAV1.dec <- subset(NAV1, substr(NAV1$Date, 5, 6)=='12')

NAV1mean.jan <- sum(NAV1.jan$Data)/30
NAV1mean.feb <- sum(NAV1.feb$Data)/30
NAV1mean.mar <- sum(NAV1.mar$Data)/30
NAV1mean.apr <- sum(NAV1.apr$Data)/30
NAV1mean.may <- sum(NAV1.may$Data)/30
NAV1mean.jun <- sum(NAV1.jun$Data)/30
NAV1mean.jul <- sum(NAV1.jul$Data)/30
NAV1mean.aug <- sum(NAV1.aug$Data)/30
NAV1mean.sep <- sum(NAV1.sep$Data)/30
NAV1mean.oct <- sum(NAV1.oct$Data)/30
NAV1mean.nov <- sum(NAV1.nov$Data)/30
NAV1mean.dec <- sum(NAV1.dec$Data)/30

NAV1.month.rain <- c(NAV1mean.jan, NAV1mean.feb, NAV1mean.mar, NAV1mean.apr, NAV1mean.may, NAV1mean.jun, NAV1mean.jul, NAV1mean.aug, NAV1mean.sep, NAV1mean.oct, NAV1mean.nov, NAV1mean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, NAV1.month.rain, main = "monthly average, NAV1")

ACC.jan <- subset(ACC, substr(ACC$Date, 5, 6)=='01')
ACC.feb <- subset(ACC, substr(ACC$Date, 5, 6)=='02')
ACC.mar <- subset(ACC, substr(ACC$Date, 5, 6)=='03')
ACC.apr <- subset(ACC, substr(ACC$Date, 5, 6)=='04')
ACC.may <- subset(ACC, substr(ACC$Date, 5, 6)=='05')
ACC.jun <- subset(ACC, substr(ACC$Date, 5, 6)=='06')
ACC.jul <- subset(ACC, substr(ACC$Date, 5, 6)=='07')
ACC.aug <- subset(ACC, substr(ACC$Date, 5, 6)=='08')
ACC.sep <- subset(ACC, substr(ACC$Date, 5, 6)=='09')
ACC.oct <- subset(ACC, substr(ACC$Date, 5, 6)=='10')
ACC.nov <- subset(ACC, substr(ACC$Date, 5, 6)=='11')
ACC.dec <- subset(ACC, substr(ACC$Date, 5, 6)=='12')

ACCmean.jan <- sum(ACC.jan$Data)/30
ACCmean.feb <- sum(ACC.feb$Data)/30
ACCmean.mar <- sum(ACC.mar$Data)/30
ACCmean.apr <- sum(ACC.apr$Data)/30
ACCmean.may <- sum(ACC.may$Data)/30
ACCmean.jun <- sum(ACC.jun$Data)/30
ACCmean.jul <- sum(ACC.jul$Data)/30
ACCmean.aug <- sum(ACC.aug$Data)/30
ACCmean.sep <- sum(ACC.sep$Data)/30
ACCmean.oct <- sum(ACC.oct$Data)/30
ACCmean.nov <- sum(ACC.nov$Data)/30
ACCmean.dec <- sum(ACC.dec$Data)/30

ACC.month.rain <- c(ACCmean.jan, ACCmean.feb, ACCmean.mar, ACCmean.apr, ACCmean.may, ACCmean.jun, ACCmean.jul, ACCmean.aug, ACCmean.sep, ACCmean.oct, ACCmean.nov, ACCmean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, ACC.month.rain, main = "monthly average, ACC")

ADA.jan <- subset(ADA, substr(ADA$Date, 5, 6)=='01')
ADA.feb <- subset(ADA, substr(ADA$Date, 5, 6)=='02')
ADA.mar <- subset(ADA, substr(ADA$Date, 5, 6)=='03')
ADA.apr <- subset(ADA, substr(ADA$Date, 5, 6)=='04')
ADA.may <- subset(ADA, substr(ADA$Date, 5, 6)=='05')
ADA.jun <- subset(ADA, substr(ADA$Date, 5, 6)=='06')
ADA.jul <- subset(ADA, substr(ADA$Date, 5, 6)=='07')
ADA.aug <- subset(ADA, substr(ADA$Date, 5, 6)=='08')
ADA.sep <- subset(ADA, substr(ADA$Date, 5, 6)=='09')
ADA.oct <- subset(ADA, substr(ADA$Date, 5, 6)=='10')
ADA.nov <- subset(ADA, substr(ADA$Date, 5, 6)=='11')
ADA.dec <- subset(ADA, substr(ADA$Date, 5, 6)=='12')

ADAmean.jan <- sum(ADA.jan$Data)/30
ADAmean.feb <- sum(ADA.feb$Data)/30
ADAmean.mar <- sum(ADA.mar$Data)/30
ADAmean.apr <- sum(ADA.apr$Data)/30
ADAmean.may <- sum(ADA.may$Data)/30
ADAmean.jun <- sum(ADA.jun$Data)/30
ADAmean.jul <- sum(ADA.jul$Data)/30
ADAmean.aug <- sum(ADA.aug$Data)/30
ADAmean.sep <- sum(ADA.sep$Data)/30
ADAmean.oct <- sum(ADA.oct$Data)/30
ADAmean.nov <- sum(ADA.nov$Data)/30
ADAmean.dec <- sum(ADA.dec$Data)/30

ADA.month.rain <- c(ADAmean.jan, ADAmean.feb, ADAmean.mar, ADAmean.apr, ADAmean.may, ADAmean.jun, ADAmean.jul, ADAmean.aug, ADAmean.sep, ADAmean.oct, ADAmean.nov, ADAmean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, ADA.month.rain, main = "monthly average, ADA")

TEM.jan <- subset(TEM, substr(TEM$Date, 5, 6)=='01')
TEM.feb <- subset(TEM, substr(TEM$Date, 5, 6)=='02')
TEM.mar <- subset(TEM, substr(TEM$Date, 5, 6)=='03')
TEM.apr <- subset(TEM, substr(TEM$Date, 5, 6)=='04')
TEM.may <- subset(TEM, substr(TEM$Date, 5, 6)=='05')
TEM.jun <- subset(TEM, substr(TEM$Date, 5, 6)=='06')
TEM.jul <- subset(TEM, substr(TEM$Date, 5, 6)=='07')
TEM.aug <- subset(TEM, substr(TEM$Date, 5, 6)=='08')
TEM.sep <- subset(TEM, substr(TEM$Date, 5, 6)=='09')
TEM.oct <- subset(TEM, substr(TEM$Date, 5, 6)=='10')
TEM.nov <- subset(TEM, substr(TEM$Date, 5, 6)=='11')
TEM.dec <- subset(TEM, substr(TEM$Date, 5, 6)=='12')

TEMmean.jan <- sum(TEM.jan$Data)/30
TEMmean.feb <- sum(TEM.feb$Data)/30
TEMmean.mar <- sum(TEM.mar$Data)/30
TEMmean.apr <- sum(TEM.apr$Data)/30
TEMmean.may <- sum(TEM.may$Data)/30
TEMmean.jun <- sum(TEM.jun$Data)/30
TEMmean.jul <- sum(TEM.jul$Data)/30
TEMmean.aug <- sum(TEM.aug$Data)/30
TEMmean.sep <- sum(TEM.sep$Data)/30
TEMmean.oct <- sum(TEM.oct$Data)/30
TEMmean.nov <- sum(TEM.nov$Data)/30
TEMmean.dec <- sum(TEM.dec$Data)/30

TEM.month.rain <- c(TEMmean.jan, TEMmean.feb, TEMmean.mar, TEMmean.apr, TEMmean.may, TEMmean.jun, TEMmean.jul, TEMmean.aug, TEMmean.sep, TEMmean.oct, TEMmean.nov, TEMmean.dec)
months <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
plot(months, TEM.month.rain, main = "monthly average, TEM")

