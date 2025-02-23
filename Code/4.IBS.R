#IBS
library(pec)
library(riskRegression)
library(rms)
library(Hmisc)
setwd("4.IBS")
dt_train_pre<- read.csv('test.csv')
Models <- list("Model1"= coxph(Surv(TIME,Status)~DLCS.pred, data=dt_train_pre, x=TRUE, y=TRUE),
               "Model2"= coxph(Surv(TIME,Status)~DL.pred, data=dt_train_pre, x=TRUE, y=TRUE),
               "Model3"= coxph(Surv(TIME,Status)~Clinical.pred, data=dt_train_pre, x=TRUE, y=TRUE),
               "Model4"= coxph(Surv(TIME,Status)~EORTC.pred, data=dt_train_pre, x=TRUE, y=TRUE),
               "Model5"= coxph(Surv(TIME,Status)~CUETO.pred, data=dt_train_pre, x=TRUE, y=TRUE),
               "Model6"= coxph(Surv(TIME,Status)~EAU.pred, data=dt_train_pre, x=TRUE, y=TRUE),
               "Model7"= coxph(Surv(TIME,Status)~AJCC.pred, data=dt_train_pre, x=TRUE, y=TRUE),
               "Model8"= coxph(Surv(TIME,Status)~Kim.pred, data=dt_train_pre, x=TRUE, y=TRUE))
p <- pec(object = Models,formula=Surv(TIME,Status)~Clinical.pred, data=dt_train_pre, splitMethod="Boot632plus",
         B=1000,reference = FALSE)
print(p, times=seq(0,36,6))

opar <- par(no.readonly=TRUE)
par(mfrow = c(1, 1))

plot(p,type="l",smooth=TRUE,legend = FALSE,xlim=c(0,36),axis1.at=seq(0,36,6), ylim=c(0,0.2),
     xlab="Follow-up Time (months)", ylab="Prediction error",col = c("#b0d992","red","#eac47c","#dbc0af","#e9b6be","#7e8cad","#a5c2cd","#c4c9c2"),
     lwd = c(3,3,3),lty = c(1,1,1))
legend("topleft", c("DLCS", "DL", "Clinical","EORTC","CUETO","EAU","AJCC","Kim"),
       lty = c(1,1,1), lwd = c(3,3,3), col = c("#b0d992","red","#eac47c","#dbc0af","#e9b6be","#7e8cad","#a5c2cd","#c4c9c2"), bty = "n")

