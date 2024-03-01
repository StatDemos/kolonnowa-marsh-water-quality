# Loading libraries
library(readxl)
library(tidyverse)
library(broom)
library(lmtest)
library(car)

# Loading Dataset
water_quality_data <- read_excel("water quality monthly variation.xlsx")
View(water_quality_data)

# Loading Dataset
water_quality_data <- read_excel("water quality monthly variation.xlsx")

# Creating new df
mean_data <- water_quality_data %>% 
  pivot_longer(col = 4:18, names_to = "Parameter", values_to = "Value") %>%
  group_by(Location, Parameter, `Vegetation type`) %>% 
  summarise(Mean.Value = mean(Value))

mean_data$`Vegetation type` <- relevel(as.factor(mean_data$`Vegetation type`), 
                                       ref = "WWI")

## Data

# EC
data.EC <- mean_data %>% filter(Parameter == "Av.EC")

# NH4
data.NH4 <- mean_data %>% filter(Parameter == "Av.NH4")

# NO3
data.NO3 <- mean_data %>% filter(Parameter == "Av.NO3")

# PO4
data.PO4 <- mean_data %>% filter(Parameter == "Av.PO4")

# ------------------------------------------------------------------------------

## EC

model.EC <- lm(Mean.Value~`Vegetation type`, data = data.EC)
summary(model.EC)
# p-value: 0.9794, not significant

# model
model.EC_fitresid <- augment(model.EC)

# shapiro wilk
shapiro.test(model.EC_fitresid$.std.resid)
# p-value = 5.121e-05, reject Ho - not normal

# durbin watson
durbinWatsonTest(model.EC)
# p-value = 0.022, reject Ho - data are correlated

# constant variance
bptest(model.EC)
# p-value = 0.06231, do not reject Ho - variance constant

# ------------------------------------------------------------------------------

## NH4

model.NH4 <- lm(Mean.Value~`Vegetation type`, data = data.NH4)
summary(model.NH4)
# p-value: 0.000164, significant

# model
model.NH4_fitresid <- augment(model.NH4)

# shapiro wilk
shapiro.test(model.NH4_fitresid$.std.resid)
# p-value = 0.00284, reject Ho - not normal

# durbin watson
durbinWatsonTest(model.NH4)
# p-value = 0.036, reject Ho - data are correlated

# constant variance
bptest(model.NH4)
# p-value = 0.6523, do not reject Ho - variance constant

# ------------------------------------------------------------------------------

## NO3

model.NO3 <- lm(Mean.Value~`Vegetation type`, data = data.NO3)
summary(model.NO3)
# p-value: 0.00129, significant

# model
model.NO3_fitresid <- augment(model.NO3)

# shapiro wilk
shapiro.test(model.NO3_fitresid$.std.resid)
# p-value = 7.356e-10, reject Ho - not normal

# durbin watson
durbinWatsonTest(model.NO3)
# p-value = 0.24, do not reject Ho - data are not correlated

# constant variance
bptest(model.NO3)
# p-value = 0.09839, do not reject Ho - variance constant

# ------------------------------------------------------------------------------

## PO4

model.PO4 <- lm(Mean.Value~`Vegetation type`, data = data.PO4)
summary(model.PO4)
# p-value: 2.991e-05, significant

# model
model.PO4_fitresid <- augment(model.PO4)

# shapiro wilk
shapiro.test(model.PO4_fitresid$.std.resid)
# p-value = 8.293e-06, reject Ho - not normal

# durbin watson
durbinWatsonTest(model.PO4)
# p-value = 0.06, do not reject Ho - data are not correlated

# constant variance
bptest(model.PO4)
# p-value = 0.1373, do not reject Ho - variance constant

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# trying log transformation

# ------------------------------------------------------------------------------ 

# EC log

model.EC.log <- lm(log(Mean.Value) ~ `Vegetation type`, data = data.EC)
summary(model.EC.log)
# p-value: 0.9974, not significant

# model
model.EC.log_fitresid <- augment(model.EC.log)

# shapiro wilk
shapiro.test(model.EC.log_fitresid$.std.resid)
# p-value = 1.367e-05, reject Ho - not normal

# durbin watson
durbinWatsonTest(model.EC.log)
# p-value = 0.038, reject Ho - data are correlated

# constant variance
bptest(model.EC.log)
# p-value = 0.1245, do not reject Ho - variance constant

# ------------------------------------------------------------------------------

# NH4 log

model.NH4.log <- lm(log(Mean.Value) ~ `Vegetation type`, data = data.NH4)
summary(model.NH4.log)
# p-value: 0.006449, significant

# model
model.NH4.log_fitresid <- augment(model.NH4.log)

# shapiro wilk
shapiro.test(model.NH4.log_fitresid$.std.resid)
# p-value = 0.6917, do not reject Ho - normal

# durbin watson
durbinWatsonTest(model.NH4.log)
# p-value = 0.004, reject Ho - data are correlated

# constant variance
bptest(model.NH4.log)
# p-value = 0.09683, do not reject Ho - variance constant

# ------------------------------------------------------------------------------

# NO3 log
model.NO3.log <- lm(log(Mean.Value) ~ `Vegetation type`, data = data.NO3)
summary(model.NO3.log)
# p-value: 0.007465, significant

# model
model.NO3.log_fitresid <- augment(model.NO3.log)

# shapiro wilk
shapiro.test(model.NO3.log_fitresid$.std.resid)
# p-value = 0.003487, reject Ho - not normal

# durbin watson
durbinWatsonTest(model.NO3.log)
# p-value = 0.056, do not reject Ho - data are not correlated

# constant variance
bptest(model.NO3.log)
# p-value = 0.7615, do not reject Ho - variance constant

# ------------------------------------------------------------------------------

# PO4 log
model.PO4.log <- lm(log(Mean.Value) ~ `Vegetation type`, data = data.PO4)
summary(model.PO4.log)
# p-value: 0.001947, significant

# model
model.PO4.log_fitresid <- augment(model.PO4.log)

# shapiro wilk
shapiro.test(model.PO4.log_fitresid$.std.resid)
# p-value = 0.003083, reject Ho - not normal

# durbin watson
durbinWatsonTest(model.PO4.log)
# p-value = 0.154, do not reject Ho - data are not correlated

# constant variance
bptest(model.PO4.log)
# p-value = 0.01648, reject Ho - variance not constant


