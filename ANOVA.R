# Loading libraries
library(readxl)
library(tidyverse)
 
# Loading Dataset
water_quality_data <- read_excel("water quality monthly variation.xlsx")

# Creating new df
mean_data <- water_quality_data %>% pivot_longer(col = 4:18, names_to = "Parameter", 
                                          values_to = "Value") %>%
      group_by(Location, Parameter, `Vegetation type`) %>% summarise(Mean.Value = mean(Value))

mean_data$`Vegetation type` <- relevel(as.factor(mean_data$`Vegetation type`), ref = "WWI")
  
# ANOVA

# Alkalin
data.Alkalin <- mean_data %>% filter(Parameter == "Av.Alkalin")
model.Alkalin <- lm(Mean.Value~`Vegetation type`, data = data.Alkalin)
summary(model.Alkalin)
# p-value: 0.004985, significant

# BOD
data.BOD <- mean_data %>% filter(Parameter == "Av.BOD")
model.BOD <- lm(Mean.Value~`Vegetation type`, data = data.BOD)
summary(model.BOD)
# p-value: 2.948e-05, significant

# COD
data.COD <- mean_data %>% filter(Parameter == "Av.COD")
model.COD <- lm(Mean.Value~`Vegetation type`, data = data.COD)
summary(model.COD)
# p-value: 0.0001126, significant

# DO
data.DO <- mean_data %>% filter(Parameter == "Av.DO")
model.DO <- lm(Mean.Value~`Vegetation type`, data = data.DO)
summary(model.DO)
# p-value: 0.2855, not significant

# EC
data.EC <- mean_data %>% filter(Parameter == "Av.EC")
model.EC <- lm(Mean.Value~`Vegetation type`, data = data.EC)
summary(model.EC)
# p-value: 0.9794, not significant

# NH4
data.NH4 <- mean_data %>% filter(Parameter == "Av.NH4")
model.NH4 <- lm(Mean.Value~`Vegetation type`, data = data.NH4)
summary(model.NH4)
# p-value: 0.000164, significant

# NO3
data.NO3 <- mean_data %>% filter(Parameter == "Av.NO3")
model.NO3 <- lm(Mean.Value~`Vegetation type`, data = data.NO3)
summary(model.NO3)
# p-value: 0.00129, significant

# PO4
data.PO4 <- mean_data %>% filter(Parameter == "Av.PO4")
model.PO4 <- lm(Mean.Value~`Vegetation type`, data = data.PO4)
summary(model.PO4)
# p-value: 2.991e-05, significant

# TDS
data.TDS <- mean_data %>% filter(Parameter == "Av.TDS")
model.TDS <- lm(Mean.Value~`Vegetation type`, data = data.TDS)
summary(model.TDS)
# p-value = 0.1306, Not significant

# TSS
data.TSS <- mean_data %>% filter(Parameter == "Av.TSS")
model.TSS <- lm(Mean.Value~`Vegetation type`, data = data.TSS)
summary(model.TSS)
# p-value = 0.008069, Not significant

# Temp
data.Temp <- mean_data %>% filter(Parameter == "Av.Temp")
model.Temp <- lm(Mean.Value~`Vegetation type`, data = data.Temp)
summary(model.Temp)
# p-value = 0.1505, Not significant

# Ph
data.Ph <- mean_data %>% filter(Parameter == "Av.pH")
model.Ph <- lm(Mean.Value~`Vegetation type`, data = data.Ph)
summary(model.Ph)
# p-value = 7.17e-06, Significant

# Cd
data.Cd <- mean_data %>% filter(Parameter == "Cd")
model.Cd <- lm(Mean.Value~`Vegetation type`, data = data.Cd)
summary(model.Cd)
# p-value = 1.246e-11, Significant

# Cr
data.Cr <- mean_data %>% filter(Parameter == "Cr")
model.Cr <- lm(Mean.Value~`Vegetation type`, data = data.Cr)
summary(model.Cr)
# p-value: < 2.2e-16, Significant

# Pb        
data.Pb <- mean_data %>% filter(Parameter == "Pb")
model.Pb <- lm(Mean.Value~`Vegetation type`, data = data.Pb)
summary(model.Pb)
# p-value = 5.145e-08, Significant



