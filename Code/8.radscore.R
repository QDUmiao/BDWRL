library(ggrisk)
library(rms)
data <- read.csv("DL_train.csv", header = TRUE)


fcox <- cph(Surv(TIME, Status) ~ prob,  
            x = T, 
            y = T,  
            surv = T,  
            data = data) 

ggrisk(fcox,                 
       heatmap.genes = c("prob"),
       cutoff.value = 0.637, 
       cutoff.x = 240,     
       cutoff.y = 2)
