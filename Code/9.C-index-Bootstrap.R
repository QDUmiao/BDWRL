library(survival)
library(ggplot2)

data <- read.csv("test.csv")

time <- data$TIME
status <- data$Status

model_columns <- colnames(data)[3:ncol(data)] 

results <- data.frame(Model = character(),
                      C_Index = numeric(),
                      CI_Lower = numeric(),
                      CI_Upper = numeric(),
                      stringsAsFactors = FALSE)

for (model in model_columns) {
  predictions <- data[[model]]
  
  surv_obj <- Surv(time, status)
  
  c_index <- survConcordance(surv_obj ~ predictions)
  
  c_index_value <- c_index$concordance
  
  set.seed(123) 
  n_iterations <- 2000
  boot_c_indices <- numeric(n_iterations)
  
  for (i in 1:n_iterations) {
    sample_indices <- sample(1:length(time), replace = TRUE)
    sample_time <- time[sample_indices]
    sample_status <- status[sample_indices]
    sample_predictions <- predictions[sample_indices]
    
    boot_surv_obj <- Surv(sample_time, sample_status)
    
    # 计算Bootstrapped C-index
    boot_c_index <- survConcordance(boot_surv_obj ~ sample_predictions)
    boot_c_indices[i] <- boot_c_index$concordance
  }
  
  ci_lower <- quantile(boot_c_indices, 0.025)
  ci_upper <- quantile(boot_c_indices, 0.975)
  
  results <- rbind(results, data.frame(Model = model,
                                       C_Index = c_index_value,
                                       CI_Lower = ci_lower,
                                       CI_Upper = ci_upper))
}

print(results)
set.seed(456)
random_samples <- data.frame()

for (model in model_columns) {
  predictions <- data[[model]]
  
  for (i in 1:100) {
    sample_indices <- sample(1:length(time), replace = TRUE)
    sample_time <- time[sample_indices]
    sample_status <- status[sample_indices]
    sample_predictions <- predictions[sample_indices]
    
    sample_surv_obj <- Surv(sample_time, sample_status)
    sample_c_index <- survConcordance(sample_surv_obj ~ sample_predictions)$concordance
    
    random_samples <- rbind(random_samples, data.frame(Model = model, C_Index = sample_c_index))
  }
}


custom_colors <- c("DL" = "#fccccb", 
                   "DLCS" = "#bdb5e1", 
                   "Clinical" = "#b0d992", 
                   "EORTC" = "#f9d580",
                   "CUETO" = "#99b9e9", 
                   "EAU" = "#e3716e", 
                   "AJCC" = "#eca680",
                   "Kim" = "#FFE2CE")
results <- results[order(results$C_Index), ]

ggplot(results, aes(x = reorder(Model, C_Index), y = C_Index, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "black") +
  geom_jitter(data = random_samples, aes(y = C_Index), shape = 21, color = "black", fill = "black", size = 0.8, alpha = 0.8, width = 0.2) +
  labs(title = "C-index for Each Model with CI and Random Samples",
       x = "Model",
       y = "C-index") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("train.pdf", plot = p, width = 8, height = 6)


