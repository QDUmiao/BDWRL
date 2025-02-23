library(tidyverse)
library(caret)
library(pROC)
library(glmnet)
library(DMwR2)
library(rmda)
library(ggpubr)
library(ModelGood)
library(rms)
library(mRMRe)
library(DescTools)
library(Boruta)
library(sva)
library(e1071)
library(survcomp)
setwd("E:/1修/3.calibrationcurve")
dt_train_pre1 <- read_csv('DLCS_train.csv')
dt_test_pre1 <- read_csv('DLCS_test.csv')
dt_train_pre2 <- read_csv('DL_train.csv')
dt_test_pre2 <- read_csv('DL_test.csv')
dt_train_pre3 <- read_csv('Clinical_train.csv')
dt_test_pre3 <- read_csv('Clinical_test.csv')
dt_train_pre4 <- read_csv('EORTC_train.csv')
dt_test_pre4 <- read_csv('EORTC_test.csv')
dt_train_pre5 <- read_csv('CUETO_train.csv')
dt_test_pre5 <- read_csv('CUETO_test.csv')
dt_train_pre6 <- read_csv('EAU_train.csv')
dt_test_pre6 <- read_csv('EAU_test.csv')
dt_train_pre7 <- read_csv('AJCC_train.csv')
dt_test_pre7 <- read_csv('AJCC_test.csv')
dt_train_pre8 <- read_csv('Kim_train.csv')
dt_test_pre8 <- read_csv('Kim_test.csv')

# Calibration plot
#DLCS
units(dt_train_pre1$TIME) <- "Month"
dd=datadist(dt_train_pre1)
options(datadist="dd")
f1 <- cph(Surv(dt_train_pre1$TIME,dt_train_pre1$Status==1)~number+DL.pred, data=dt_train_pre1, x=TRUE, y=TRUE, surv = TRUE, time.inc = 48)
cal1 <- calibrate(f1, cmethod = "KM", method="boot",u=48,m=100,B=1000)

units(dt_test_pre1$TIME) <- "Month"
dd=datadist(dt_test_pre1)
options(datadist="dd")
f2 <- cph(Surv(dt_test_pre1$TIME,dt_test_pre1$Status==1)~number+DL.pred, data=dt_test_pre1, x=TRUE, y=TRUE, surv = TRUE, time.inc = 48)
cal2 <- calibrate(f2, cmethod = "KM", method="boot",u=48,m=50,B=1000)
#DL
units(dt_train_pre2$TIME) <- "Month"
dd=datadist(dt_train_pre2)
options(datadist="dd")
f3 <- cph(Surv(dt_train_pre2$TIME,dt_train_pre2$Status==1)~prob, data=dt_train_pre2, x=TRUE, y=TRUE, surv = TRUE, time.inc = 48)
cal3 <- calibrate(f3, cmethod = "KM", method="boot",u=48,m=100,B=1000)

units(dt_test_pre2$TIME) <- "Month"
dd=datadist(dt_test_pre2)
options(datadist="dd")
f4 <- cph(Surv(dt_test_pre2$TIME,dt_test_pre2$Status==1)~prob, data=dt_test_pre2, x=TRUE, y=TRUE, surv = TRUE, time.inc = 48)
cal4 <- calibrate(f4, cmethod = "KM", method="boot",u=48,m=50,B=1000)
#clinical
units(dt_train_pre3$TIME) <- "Month"
dd=datadist(dt_train_pre3)
options(datadist="dd")
f5 <- cph(Surv(dt_train_pre3$TIME,dt_train_pre3$Status==1)~number, data=dt_train_pre3, x=TRUE, y=TRUE, surv = TRUE, time.inc = 48)
cal5 <- calibrate(f5, cmethod = "KM", method="boot",u=48,m=100,B=1000)

units(dt_test_pre3$TIME) <- "Month"
dd=datadist(dt_test_pre3)
options(datadist="dd")
f6 <- cph(Surv(dt_test_pre3$TIME,dt_test_pre3$Status==1)~number, data=dt_test_pre3, x=TRUE, y=TRUE, surv = TRUE, time.inc = 48)
cal6 <- calibrate(f6, cmethod = "KM", method="boot",u=48,m=50,B=1000)
#EORTC
units(dt_train_pre4$TIME) <- "Month"
dd=datadist(dt_train_pre4)
options(datadist="dd")
f7 <- cph(Surv(dt_train_pre4$TIME,dt_train_pre4$Status==1)~G+T+Tis+size+number, data=dt_train_pre4, x=TRUE, y=TRUE, surv = TRUE, time.inc =48)
cal7 <- calibrate(f7, cmethod = "KM", method="boot",u=48,m=100,B=1000)

units(dt_test_pre4$TIME) <- "Month"
dd=datadist(dt_test_pre4)
options(datadist="dd")
f8 <- cph(Surv(dt_test_pre4$TIME,dt_test_pre4$Status==1)~G+T+Tis+size+number, data=dt_test_pre4, x=TRUE, y=TRUE, surv = TRUE, time.inc = 48)
cal8 <- calibrate(f8, cmethod = "KM", method="boot",u=48,m=50,B=1000)
#CUETO
units(dt_train_pre5$TIME) <- "Month"
dd=datadist(dt_train_pre5)
options(datadist="dd")
f9 <- cph(Surv(dt_train_pre5$TIME,dt_train_pre5$Status==1)~G1+Tis1+number1+Age+Gender, data=dt_train_pre5, x=TRUE, y=TRUE, surv = TRUE, time.inc =48)
cal9 <- calibrate(f9, cmethod = "KM", method="boot",u=48,m=100,B=1000)

units(dt_test_pre5$TIME) <- "Month"
dd=datadist(dt_test_pre5)
options(datadist="dd")
f10 <- cph(Surv(dt_test_pre5$TIME,dt_test_pre5$Status==1)~G1+Tis1+number1+Age+Gender, data=dt_test_pre5, x=TRUE, y=TRUE, surv = TRUE, time.inc = 48)
cal10 <- calibrate(f10, cmethod = "KM", method="boot",u=48,m=50,B=1000)
#EAU
units(dt_train_pre6$TIME) <- "Month"
dd=datadist(dt_train_pre6)
options(datadist="dd")
f11 <- cph(Surv(dt_train_pre6$TIME,dt_train_pre6$Status==1)~risk, data=dt_train_pre6, x=TRUE, y=TRUE, surv = TRUE, time.inc =48)
cal11 <- calibrate(f11, cmethod = "KM", method="boot",u=48,m=100,B=1000)

units(dt_test_pre6$TIME) <- "Month"
dd=datadist(dt_test_pre6)
options(datadist="dd")
f12 <- cph(Surv(dt_test_pre6$TIME,dt_test_pre6$Status==1)~risk, data=dt_test_pre6, x=TRUE, y=TRUE, surv = TRUE, time.inc = 48)
cal12 <- calibrate(f12, cmethod = "KM", method="boot",u=48,m=50,B=1000)
#AJCC
units(dt_train_pre7$TIME) <- "Month"
dd=datadist(dt_train_pre7)
options(datadist="dd")
f13 <- cph(Surv(dt_train_pre7$TIME,dt_train_pre7$Status==1)~AJCC, data=dt_train_pre7, x=TRUE, y=TRUE, surv = TRUE, time.inc =48)
cal13 <- calibrate(f13, cmethod = "KM", method="boot",u=48,m=100,B=1000)

units(dt_test_pre7$TIME) <- "Month"
dd=datadist(dt_test_pre7)
options(datadist="dd")
f14 <- cph(Surv(dt_test_pre7$TIME,dt_test_pre7$Status==1)~AJCC, data=dt_test_pre7, x=TRUE, y=TRUE, surv = TRUE, time.inc = 48)
cal14 <- calibrate(f14, cmethod = "KM", method="boot",u=48,m=50,B=1000)

#kim
units(dt_train_pre8$TIME) <- "Month"
dd=datadist(dt_train_pre8)
options(datadist="dd")
f15 <- cph(Surv(dt_train_pre8$TIME,dt_train_pre8$Status==1)~GHU+Intravesical+number+G2+T2+UTUC, data=dt_train_pre8, x=TRUE, y=TRUE, surv = TRUE, time.inc =48)
cal15 <- calibrate(f15, cmethod = "KM", method="boot",u=48,m=100,B=1000)

units(dt_test_pre8$TIME) <- "Month"
dd=datadist(dt_test_pre8)
options(datadist="dd")
f16 <- cph(Surv(dt_test_pre8$TIME,dt_test_pre8$Status==1)~GHU+Intravesical+number+G2+T2+UTUC, data=dt_test_pre8, x=TRUE, y=TRUE, surv = TRUE, time.inc = 48)
cal16 <- calibrate(f16, cmethod = "KM", method="boot",u=48,m=50,B=1000)

opar <- par(no.readonly=TRUE)
par(mfrow = c(1, 2))

plot(cal1, errbar.col = "Yellow",lwd = 2,lty=2, cex.axis =1.2,
     cex.lab = 1.2,xlab="Model predicted survival probability", ylab="Observed survival (probability)",
     xlim = c(0,1),ylim = c(0,1), subtitles = FALSE)
lines(cal1[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "#dadae4",pch = 16)
par(new=TRUE)
lines(cal3[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "red",pch = 16)
par(new=TRUE)
lines(cal5[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "#eac47c",pch = 16)
par(new=TRUE)
lines(cal7[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "#dbc0af",pch = 16)
par(new=TRUE)
lines(cal9[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "#c4c9c2",pch = 16)
par(new=TRUE)
lines(cal11[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "#7e8cad",pch = 16)
par(new=TRUE)
lines(cal13[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "#a5c2cd",pch = 16)
par(new=TRUE)
lines(cal15[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "#9BC2A5",pch = 16)
par(new=TRUE)
legend("topleft", c("DLCS","DL","Clinical","EORTC","CUETO","EAU","AJCC","Kim"),
       lty = c(1,1,1,1), lwd = c(2,2,2,2), col = c("#dadae4","red","#eac47c","#dbc0af","#c4c9c2","#7e8cad","#a5c2cd","#9BC2A5"), bty = "n")
abline(0,1,col="black",lty=2,lwd=1)

plot(cal2, errbar.col = "Yellow",lwd = 2,lty=2, cex.axis =1.2,
     cex.lab = 1.2,xlab="Model predicted survival probability", ylab="Observed survival (probability)",
     xlim = c(0,1),ylim = c(0,1), subtitles = FALSE)
lines(cal2[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "#dadae4",pch = 16)
par(new=TRUE)
lines(cal4[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "red",pch = 16)
par(new=TRUE)
lines(cal6[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "#eac47c",pch = 16)
par(new=TRUE)
lines(cal8[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "#dbc0af",pch = 16)
par(new=TRUE)
lines(cal10[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "#c4c9c2",pch = 16)
par(new=TRUE)
lines(cal12[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "#7e8cad",pch = 16)
par(new=TRUE)
lines(cal14[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "#a5c2cd",pch = 16)
par(new=TRUE)
lines(cal16[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "#9BC2A5",pch = 16)
par(new=TRUE)
legend("topleft", c("DLCS","DL","Clinical","EORTC","CUETO","EAU","AJCC","Kim"),
       lty = c(1,1,1,1), lwd = c(2,2,2,2), col = c("#dadae4","red","#eac47c","#dbc0af","#c4c9c2","#7e8cad","#a5c2cd","#9BC2A5"), bty = "n")
abline(0,1,col="black",lty=2,lwd=1)

surv <- Survival(f3)
nom <- nomogram(f3, fun = list(function(x) surv(12,x),function(x) surv(36,x)), fun.at = c(0.01, 0.2, 0.5,0.8,0.9,.99), funlabel = c("1-years Survival Probability", "3-years Survival Probability"), lp = F)
plot(nom)
