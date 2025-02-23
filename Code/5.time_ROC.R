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
library(timeROC)
setwd("E:/1修/5.time_ROC")

dt_adj <- read_csv('test.csv')

opar <- par(no.readonly=TRUE)

par(mar=c(4, 4, 1, 0.5), xpd=TRUE)
par(pin=c(3,3))
par(mfrow = c(1, 1))
plot(dt_adj$Time, dt_adj$DL,type = "l",col = "red", xlab="Time after surgery (months)", ylab="Area under the ROC curve",
     xaxt = "n", ylim = c(0,1), pch = 16,lwd = 2,lty=1, cex.axis =1.2,cex.lab = 1.2)
axis(1,at=seq(0,48,6))
lines(dt_adj$Time, dt_adj$DLCS,type = "l",col = "#dbc0af",lwd = 2,lty=1)
lines(dt_adj$Time, dt_adj$Clinical,type = "l",col = "#17793E",lwd = 2,lty=1)
lines(dt_adj$Time, dt_adj$EORTC,type = "l",col = "#A0E2E1",lwd = 2,lty=1)
lines(dt_adj$Time, dt_adj$CUETO,type = "l",col = "#5C5D9E",lwd = 2,lty=1)
lines(dt_adj$Time, dt_adj$EAU,type = "l",col = "#F7C2CD",lwd = 2,lty=1)
lines(dt_adj$Time, dt_adj$AJCC,type = "l",col = "#E3A025",lwd = 2,lty=1)
lines(dt_adj$Time, dt_adj$Kim,type = "l",col = "#b0d992",lwd = 2,lty=1)

legend('right',inset=c(-0.6,0), c("DL","DLCS","Clinical","EORTC","CUETO","EAU","AJCC","Kim"),
       lty = c(1,1,1), lwd = c(2,2,2), col = c("red","#dbc0af","#17793E","#A0E2E1","#5C5D9E","#F7C2CD","#E3A025","#b0d992"), bty = "n",cex = 1)

