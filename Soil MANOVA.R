library(readxl)
library(tidyverse)
library(mvnormtest)

# Data
Soil_data <- read_excel("soil Analysis summery.xlsx")

# MANOVA model
manova_model <- manova(cbind(`Moisture content`, OMC, IMC, Bulk.D) ~ `Vegetation type`, 
                            data = Soil_data)
summary(manova_model)
# p-value = 0.6436, not significant.

# Look to see which differ
summary.aov(manova_model)


# Multivariate Normality
C <- t(manova_data[1:47, 3:17])
mshapiro.test(C)
