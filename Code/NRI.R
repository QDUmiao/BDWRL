library(tidyverse)
library(survival)
library(nricens)

file_path <- "NRI.csv"
data <- read.csv(file_path)

train_index <- 1:408  
test_index <- 409:nrow(data)  

train <- data[train_index, -1]  
test <- data[test_index, -1] 

cox6 <- coxph(Surv(TIME, Status) ~ prob , 
              x = TRUE, data = data)

cox4 <- coxph(Surv(TIME, Status) ~ G+T+Tis+size+number , 
              x = TRUE, data = data)

set.seed(123)
nri_result <- nricens(mdl.std = cox4,
                      mdl.new = cox6,
                      t0 = 12,   
                      cut = c(0.05),  
                      updown = "diff",  
                      niter = 1000)


#print(nri_result)  

nri_estimate <- 1.36598526    
nri_lower <- 1.22729049  
nri_upper <- 1.48365777  

nri_se <- (nri_upper - nri_lower) / (2 * qnorm(0.975))  

z_value <- nri_estimate / nri_se

p_value <- 2 * (1 - pnorm(abs(z_value)))

cat("p:", p_value, "\n")
