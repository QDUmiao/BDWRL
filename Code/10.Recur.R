setwd("km")
library(survminer)
library(survival)
library(units) 

dt_adj <- read.csv('RFS.csv')
dt_adj$TIME <- as.numeric(dt_adj$TIME) 
units(dt_adj$TIME) <- "Month"
dt_adj$Status <- as.numeric(dt_adj$Status) 

dt_adj$RISK1 <- factor(dt_adj$risk,labels=c("Low Risk", "High Risk"))
dt_train_pre <- dt_adj[c(1:408),]
dt_test_pre <- dt_adj[c(409:731),]
dt_train_pre <- dt_train_pre[,c(-1,-2)]
dt_test_pre <- dt_test_pre[,c(-1,-2)]
colnames(dt_test_pre) <- colnames(dt_train_pre)

dt_train_pre$TIME <- as.numeric(dt_train_pre$TIME)  
fit <- survfit(Surv(TIME, Status) ~ risk, data = dt_train_pre)
dt_train_pre$Status <- as.integer(dt_train_pre$Status)

fit <- survfit(Surv(TIME,Status)~ risk, data = dt_train_pre)
fit
summary(fit)

surv_summary(fit)

surv_summary <- summary(fit)
surv_data <- data.frame(
  Time = surv_summary$time,
  Survival = surv_summary$surv,
  Lower_CI = surv_summary$lower,
  Upper_CI = surv_summary$upper,
  n_risk = surv_summary$n.risk,
  n_event = surv_summary$n.event,
  n_censor = surv_summary$n.censor
)

print(surv_data)

write.csv(surv_data, "surv_summary.csv", row.names = FALSE)

ggsurvplot(fit, 
           data = dt_train_pre, 
           conf.int = TRUE, 
           xlim =c(0,60), break.x.by = 12, ylim =c(0,1), break.y.by = 0.2,
           pval = TRUE, label.x = 3,label.y = 38,
           risk.table = TRUE, 
           legend.labs = c("Low Risk", "High Risk"), 
           xlab = "Follow up time(months)", 
           ylab = "Survival probability", 
           surv.median.line = "hv", 
           add.all = F,
           fun = "cumhaz")         
survdiff(Surv(TIME,Status)~ RISK1, dt_train_pre)
pairwise_survdiff(Surv(TIME,Status)~ RISK1, dt_train_pre,p.adjust.method= "BH")
?ggsurvplot

dt_test_pre$TIME <- as.numeric(dt_test_pre$TIME)  
fit <- survfit(Surv(TIME, Status) ~ risk, data = dt_test_pre)
dt_test_pre$Status <- as.integer(dt_test_pre$Status)
fit2 <- survfit(Surv(TIME,Status)~ RISK1, data = dt_test_pre)
fit2
summary(fit2)

surv_summary(fit2)

surv_summary <- summary(fit2)

surv_data <- data.frame(
  Time = surv_summary$time,
  Survival = surv_summary$surv,
  Lower_CI = surv_summary$lower,
  Upper_CI = surv_summary$upper,
  n_risk = surv_summary$n.risk,
  n_event = surv_summary$n.event,
  n_censor = surv_summary$n.censor
)

print(surv_data)

write.csv(surv_data, "surv_summary.csv", row.names = FALSE)

ggsurvplot(fit2,#ggsurvpl
           data = dt_test_pre, 
           conf.int = TRUE,
           xlim =c(0,60), break.x.by = 12, ylim =c(0,1), break.y.by = 0.2,
           pval = TRUE, label.x = 3,label.y = 38,
           risk.table = TRUE, 
           legend.labs = c("Low Risk", "High Risk"), 
           xlab = "Follow up time(months)", 
           ylab = "Survival probability", 
           surv.median.line = "hv", 
           add.all = F,
           fun = "cumhaz") 
survdiff(Surv(TIME,Status)~ RISK1, dt_test_pre)
pairwise_survdiff(Surv(TIME,Status)~ RISK1, dt_test_pre,p.adjust.method = "BH")

ggsurvplot(fit, 
           data = dt_train_pre, 
           conf.int = TRUE,
           pval = TRUE, label.x = 3,label.y = 38,
          risk.table = TRUE, 
           surv.median.line = "hv", 
           add.all = F)            


setwd("PFS")
#install.packages("survminer")
library(survminer)
library(survival)
library(units) 
library(cmprsk)

dt_adj <- read.csv('PFS.csv')
dt_adj$TIME <- as.numeric(dt_adj$TIME) 
units(dt_adj$TIME) <- "Month"
dt_adj$Status <- as.numeric(dt_adj$Status) 
#str(dt_adj) 
dt_adj$RISK1 <- factor(dt_adj$risk,labels=c("Low Risk", "High Risk"))
dt_train_pre <- dt_adj[c(1:408),]
dt_test_pre <- dt_adj[c(409:731),]
#dt_train_pre <- dt_train_pre[,c(-1,-2)]
#dt_test_pre <- dt_test_pre[,c(-1,-2)]
colnames(dt_test_pre) <- colnames(dt_train_pre)

dt_train_pre$TIME <- as.numeric(dt_train_pre$TIME) 
fit <- survfit(Surv(TIME, Status) ~ risk, data = dt_train_pre)
ggsurvplot(fit, 
           data = dt_train_pre, 
           xlim =c(0,60), break.x.by = 6, ylim =c(0,1), break.y.by = 0.2,
           pval = TRUE, label.x = 3,label.y = 38,
           conf.int = TRUE,
           surv.median.line = "hv",
           risk.table = TRUE,
           legend.labs = c("Low Risk", "High Risk"), 
           xlab = "Follow up time(months)", 
           ylab = "Survival probability",
           #ncensor.plot = TRUE,
           fun = "cumhaz")


dt_test_pre$TIME <- as.numeric(dt_test_pre$TIME)  
fit <- survfit(Surv(TIME, Status) ~ risk, data = dt_test_pre)
ggsurvplot(fit, 
           data = dt_train_pre, 
           xlim =c(0,60), break.x.by = 6, ylim =c(0,1), break.y.by = 0.2,
           pval = TRUE, label.x = 3,label.y = 38,
           conf.int = TRUE,
           surv.median.line = "hv",
           risk.table = TRUE,
           legend.labs = c("Low Risk", "High Risk"), 
           xlab = "Follow up time(months)", 
           ylab = "Survival probability", 
           #ncensor.plot = TRUE,
           fun = "cumhaz")
