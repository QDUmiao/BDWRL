rm(list = ls())
library(timeROC)
library(survival)
setwd("14.delong")
train<- read.csv('train.csv')
test <- read.csv('test.csv')


ROC.Clinical <- timeROC(T=train$TIME,
                    delta=train$Status,   
                    marker=train$Clinical,   
                    cause=1,                
                    weighting="marginal",   
                    times=c(6,12,24,36,48),
                    iid=TRUE)

ROC.DL <- timeROC(T=train$TIME,   
                   delta=train$Status,   
                   marker=train$DL,   
                   cause=1,   
                   weighting="marginal",   
                   times=c(6,12,24,36,48),
                   iid=TRUE)

ROC.DLCS <- timeROC(T=train$TIME,   
                  delta=train$Status,   
                  marker=train$DLCS,   
                  cause=1,   
                  weighting="marginal",   
                  times=c(6,12,24,36,48),
                  iid=TRUE)

ROC.EORTC <- timeROC(T=train$TIME,   
                  delta=train$Status,   
                  marker=train$EORTC,   
                  cause=1,   
                  weighting="marginal",   
                  times=c(6,12,24,36,48),
                  iid=TRUE)

ROC.CUETO <- timeROC(T=train$TIME,   
                     delta=train$Status,   
                     marker=train$CUETO,   
                     cause=1,   
                     weighting="marginal",   
                     times=c(6,12,24,36,48),
                     iid=TRUE)

ROC.EAU <- timeROC(T=train$TIME,   
                     delta=train$Status,   
                     marker=train$EAU,   
                     cause=1,   
                     weighting="marginal",   
                     times=c(6,12,24,36,48),
                     iid=TRUE)

ROC.AJCC <- timeROC(T=train$TIME,   
                   delta=train$Status,   
                   marker=train$AJCC,   
                   cause=1,   
                   weighting="marginal",   
                   times=c(6,12,24,36,48),
                   iid=TRUE)

ROC.Kim <- timeROC(T=train$TIME,   
                    delta=train$Status,   
                    marker=train$Kim,   
                    cause=1,   
                    weighting="marginal",   
                    times=c(6,12,24,36,48),
                    iid=TRUE)


compare(ROC.DL, ROC.Clinical)
compare(ROC.DL, ROC.Clinical, adjusted = T) 

compare(ROC.DL, ROC.DLCS)
compare(ROC.DL, ROC.DLCS, adjusted = T)  

compare(ROC.DL, ROC.EORTC)
compare(ROC.DL, ROC.EORTC, adjusted = T)  

compare(ROC.DL, ROC.CUETO)
compare(ROC.DL, ROC.CUETO, adjusted = T)  

compare(ROC.DL, ROC.EAU)
compare(ROC.DL, ROC.EAU, adjusted = T)  

compare(ROC.DL, ROC.AJCC)
compare(ROC.DL, ROC.AJCC, adjusted = T) 

compare(ROC.DL, ROC.Kim)
compare(ROC.DL, ROC.Kim, adjusted = T)  




ROC.Clinical <- timeROC(T=test$TIME,
                        delta=test$Status,   
                        marker=test$Clinical,   
                        cause=1,                
                        weighting="marginal",   
                        times=c(6,12,24,36,48),
                        iid=TRUE)

ROC.DL <- timeROC(T=test$TIME,   
                  delta=test$Status,   
                  marker=test$DL,   
                  cause=1,   
                  weighting="marginal",   
                  times=c(6,12,24,36,48),
                  iid=TRUE)

ROC.DLCS <- timeROC(T=test$TIME,   
                    delta=test$Status,   
                    marker=test$DLCS,   
                    cause=1,   
                    weighting="marginal",   
                    times=c(6,12,24,36,48),
                    iid=TRUE)

ROC.EORTC <- timeROC(T=test$TIME,   
                     delta=test$Status,   
                     marker=test$EORTC,   
                     cause=1,   
                     weighting="marginal",   
                     times=c(6,12,24,36,48),
                     iid=TRUE)

ROC.CUETO <- timeROC(T=test$TIME,   
                     delta=test$Status,   
                     marker=test$CUETO,   
                     cause=1,   
                     weighting="marginal",   
                     times=c(6,12,24,36,48),
                     iid=TRUE)

ROC.EAU <- timeROC(T=test$TIME,   
                   delta=test$Status,   
                   marker=test$EAU,   
                   cause=1,   
                   weighting="marginal",   
                   times=c(6,12,24,36,48),
                   iid=TRUE)

ROC.AJCC <- timeROC(T=test$TIME,   
                   delta=test$Status,   
                   marker=test$AJCC,   
                   cause=1,   
                   weighting="marginal",   
                   times=c(6,12,24,36,48),
                   iid=TRUE)

ROC.Kim <- timeROC(T=test$TIME,   
                    delta=test$Status,   
                    marker=test$Kim,   
                    cause=1,   
                    weighting="marginal",   
                    times=c(6,12,24,36,48),
                    iid=TRUE)



compare(ROC.DL, ROC.Clinical)
compare(ROC.DL, ROC.Clinical, adjusted = T)  

compare(ROC.DL, ROC.DLCS)
compare(ROC.DL, ROC.DLCS, adjusted = T) 

compare(ROC.DL, ROC.EORTC)
compare(ROC.DL, ROC.EORTC, adjusted = T) 

compare(ROC.DL, ROC.CUETO)
compare(ROC.DL, ROC.CUETO, adjusted = T)  

compare(ROC.DL, ROC.EAU)
compare(ROC.DL, ROC.EAU, adjusted = T)  

compare(ROC.DL, ROC.AJCC)
compare(ROC.DL, ROC.AJCC, adjusted = T)  

compare(ROC.DL, ROC.Kim)
compare(ROC.DL, ROC.Kim, adjusted = T) 


