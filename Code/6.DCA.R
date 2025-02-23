#library(dcurves)
library(survival)
source("dca.R")
source("stdca.R")
df_surv <- read.csv("DCA.csv",header = T)

dim(df_surv)
str(df_surv)

DL <- coxph(Surv(TIME, Status) ~ prob, 
                  data = df_surv)
DLCS <- coxph(Surv(TIME, Status) ~ number+DL.pred, data = df_surv)
Clinical <- coxph(Surv(TIME, Status) ~ number, data = df_surv)
EORTC <- coxph(Surv(TIME, Status) ~ G+T+Tis+size+number, data = df_surv)
CUETO <- coxph(Surv(TIME, Status) ~ G1+Tis1+number1+Age+Gender, data = df_surv)
EAU <- coxph(Surv(TIME, Status) ~ EAU, data = df_surv)
AJCC <- coxph(Surv(TIME, Status) ~ AJCC, data = df_surv)
Kim <- coxph(Surv(TIME, Status) ~ GHU+Intravesical+number2+G2+T2+UTUC, data = df_surv)

df_surv$DL <- c(1-(summary(survfit(DL, newdata=df_surv), times=48)$surv))
df_surv$DLCS <- c(1-(summary(survfit(DLCS, newdata=df_surv), times=48)$surv))
df_surv$Clinical <- c(1-(summary(survfit(Clinical, newdata=df_surv), times=48)$surv))
df_surv$EORTC <- c(1-(summary(survfit(EORTC, newdata=df_surv), times=48)$surv))
df_surv$CUETO <- c(1-(summary(survfit(CUETO, newdata=df_surv), times=48)$surv))
df_surv$EAU <- c(1-(summary(survfit(EAU, newdata=df_surv), times=48)$surv))
df_surv$AJCC <- c(1-(summary(survfit(AJCC, newdata=df_surv), times=48)$surv))
df_surv$Kim <- c(1-(summary(survfit(Kim, newdata=df_surv), times=48)$surv))


stdca(data=df_surv, 
      outcome="Status", 
      ttoutcome="TIME", 
      timepoint=48, 
      predictors=c("DL","DLCS","Clinical","EORTC","CUETO","EAU","AJCC","Kim"),  
      smooth=TRUE
)

