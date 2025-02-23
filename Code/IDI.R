library(survIDINRI)
library(survC1)
library(tidyverse)
library(survival)
library(nricens)

file_path <- "16.NRI IDI/NRI.csv"
data <- read.csv(file_path)

train_index <- 1:408  
test_index <- 409:nrow(data)  


#train <- data[train_index, -1]  
#test <- data[test_index, -1]  


y = data[, c("TIME", "Status")]

set.seed(123)

new = as.matrix(data[, c("prob")])

old = as.matrix(data[, c( "AJCC")])

x <- IDI.INF(y, old, new, t0 = 36, npert = 1000) 

IDI.INF.OUT(x)

IDI.INF.GRAPH(x)

