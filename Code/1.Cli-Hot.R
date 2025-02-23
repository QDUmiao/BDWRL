library(ComplexHeatmap)
library(tidyverse)
library(grid)
library(circlize)

data <- read.csv("zongbiaoge1.csv", header = TRUE)
str(data)

#survdata <- data.frame(row.names = rownames(data), Time = data$Time)  
#data <- data[, -which(names(data) == "Time")]

data$Cohort<- factor(data$Cohort, levels = c("Train", "Test"))
data$Status <- factor(data$Status, levels = c("No", "Yes"))
data$T <- factor(data$T, levels = c("Ta", "T1"))
data$Tis <- factor(data$Tis, levels = c("Yes", "No"))
data$G <- factor(data$G, levels = c("G1", "G2", "G3"))
data$Number <- factor(data$Number, levels = c("1", "2~7", ">=8"))
data$Size <- factor(data$Size, levels = c("<3", ">=3"))
data$Age <- factor(data$Age, levels = c("<60", "60-70", ">70"))
data$Gender <- factor(data$Gender, levels = c("Male", "Female"))
data$Focus.location <- factor(data$Focus.location, levels = c("Trigone", "Lateral","Posterior","Anterior","Parietal","Lower","Multiple"))
data$Shape <- factor(data$Shape, levels = c("Cauliflower", "Mounded","Papillary"))
data$Tumor.boundary <- factor(data$Tumor.boundary, levels = c("Clear", "Ambiguous"))
data$Stalk <- factor(data$Stalk, levels = c("Absent", "Present"))
data$Calcification <- factor(data$Calcification, levels = c("Absent", "Present"))
data$Cystic.necrosis <- factor(data$Cystic.necrosis, levels = c("Absent", "Present"))
continuous_vars <- data[, c("DL.score",
                            "LCTV_C",
                            "LCTV_N", 
                            "LCTV_E",
                            "RFS")]
continuous_matrix <- as.matrix(continuous_vars)

col_fun_DL.score <- colorRamp2(  
  c(0, 0.5, 1),  
  c("#DC0000FF", "grey", "#1f78b4")
)
col_fun_LCTV_C <- colorRamp2(  
  c(0, 100, 200),  
  c("#DC0000FF", "grey", "#1f78b4")
)
col_fun_LCTV_N <- colorRamp2(  
  c(0, 100, 200),  
  c("#DC0000FF", "grey", "#1f78b4")
)
col_fun_LCTV_E <- colorRamp2(  
  c(0, 100, 200),  
  c("#DC0000FF", "grey", "#1f78b4")
)
col_fun_RFS <- colorRamp2(  
  c(0, 60, 120),  
  c("#DC0000FF", "grey", "#1f78b4")
)
str(data)
colnames(data)
ha <- HeatmapAnnotation(
  Cohort = data$Cohort,
  Age = data$Age,
  T = data$T,
  Tis = data$Tis,
  G = data$G,
  Number = data$Number,
  Gender = data$Gender,
  Size = data$Size,
  Shape = data$Shape,
  Tumor.boundary = data$Tumor.boundary,
  Stalk = data$Stalk,
  Calcification = data$Calcification,
  Cystic.necrosis = data$Cystic.necrosis,
  Focus = data$Focus.location,
  DL.score = data$DL.score,
  LCTV_C = data$LCTV_C,
  LCTV_N = data$LCTV_N,
  LCTV_E = data$LCTV_E,
  Status = data$Status,
  RFS = data$RFS,
  col = list(
    Cohort = c("Train" = "#3575A2","Test" = "#E1822B"),
    Age = c("<60" = "#EFC99B", "60-70" = "#E8B574", ">70" = "#E19D49"), 
    T = c("Ta" = "#EDB8B0", "T1" = "#E69191"),  # 设置生存状态的颜色
    Tis = c("Yes" = "#EDB", "No" = "#E69"),  # 设置生存状态的颜色
    G = c("G1" = "#CCE4EF", "G2" = "#92B5CA", "G3" = "#599CB4"),  # 设置生存状态的颜色
    Number = c("1" = "#E6EDB2", "2~7" = "#C5DC89", ">=8" = "#6BBC47"),  # 设置生存状态的颜色
    Shape = c("Cauliflower"="#CCD3E5", "Mounded"="#C2B3D3","Papillary"="#C79DC9"),
    Gender = c("Male" = "#F5D18B", "Female" = "#ECA427"), 
    Size = c("<3" = "#DBDBA7", ">=3" = "#79902D") ,
    Tumor.boundary = c("Clear"="#EABFBB", "Ambiguous"="#9A4942"),
    Stalk = c("Absent"="#C8E0EF", "Present"="#ABC8E5"),
    Calcification = c("Absent"="#D2BBD5", "Present"="#AF76A2"),
    Cystic.necrosis = c("Absent"="#E9CDDF", "Present"="#D8A0C1"),
    Focus = c("Trigone" = "#FCB2AF", "Lateral"="#FFCC4F","Posterior"="#FFE2CE","Anterior"="#C4D8E9","Parietal"="#BEBCDF","Lower"="#B2D3A4","Multiple"="#94DBBB"),
    DL.score = col_fun_DL.score,
    LCTV_C = col_fun_LCTV_C,
    LCTV_N = col_fun_LCTV_N,
    LCTV_E = col_fun_LCTV_E,
    Status = c("No" = "#008A00", "Yes" = "#FF2500"),
    RFS = col_fun_RFS
  )
)

heat<-Heatmap(matrix(nrow=0, ncol=nrow(data)),top_annotation = ha)

#draw(heat, annotation_legend_side = "bottom")
draw(heat,
     heatmap_legend_side = "bottom",
     annotation_legend_side = "bottom", 
     width = unit(100, "cm"), 
     height = unit(0.01, "cm"), 
     )

