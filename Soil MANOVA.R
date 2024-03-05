library(readxl)
library(tidyverse)
library(mvnormtest)
library(patchwork)

# Data
Soil_data <- read_excel("soil Analysis summery.xlsx")

# MANOVA model
manova_model <- manova(cbind(`Moisture content`, OMC, IMC, Bulk.D) ~ `Vegetation type`, 
                            data = Soil_data)
summary(manova_model)
# p-value = 0.6436, not significant.

# Look to see which differ
summary.aov(manova_model)

#-------------------------------------------------------------------------------
# Normality assumption
# Multivariate Normality
# C <- t(Soil_data[1:47, 3:6])
# mshapiro.test(C)


# Shapirowilk Test
shapiro.test(sqrt(Soil_data$`Moisture content`))
shapiro.test(sqrt(Soil_data$OMC))
shapiro.test(sqrt(Soil_data$IMC))
shapiro.test(sqrt(Soil_data$Bulk.D))

p1 <- ggplot(Soil_data, aes(sample=Soil_data$`Moisture content`)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Moisture content", x = "Expected", y = "Observed")

p2 <- ggplot(Soil_data, aes(sample=Soil_data$OMC)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "OMC", x = "Expected", y = "Observed")

p3 <- ggplot(Soil_data, aes(sample=Soil_data$IMC)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "IMC", x = "Expected", y = "Observed")

p4 <- ggplot(Soil_data, aes(sample=Soil_data$Bulk.D)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Bulk.D", x = "Expected", y = "Observed")


# Plot
p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2, nrow = 2, axis_titles = 'collect') +
  plot_annotation(title = "Normal Probability Plot")



