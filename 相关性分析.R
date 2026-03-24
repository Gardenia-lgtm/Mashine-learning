
library(readxl)
library(corrplot)
library(ggplot2)
library(Hmisc)
ECES
# 读取数据
df <-ECES
data_cor <- df[, c("LTD","HTD","ERD","EED","Disaster_affected_area","Grain_output")]
# Spearman correlation
cor_matrix <- cor(data_cor,
                  method = "spearman",
                  use = "complete.obs")

print(cor_matrix)


# Pearson correlation matrix
cor_matrix <- cor(data_cor,
                  method = "pearson",
                  use = "complete.obs")

print(cor_matrix)