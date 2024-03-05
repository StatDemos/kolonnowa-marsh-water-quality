# Normality Test

library(readxl)
library(tidyverse)
library(broom)
library(car)
library(lmtest)

# Loading Dataset
water_quality_data <- read_excel("water quality monthly variation.xlsx")

# Creating new df
mean_data <- water_quality_data %>% pivot_longer(col = 4:18, 
                                                 names_to = "Parameter", 
                                                 values_to = "Value") %>%
  group_by(Location, Parameter, `Vegetation type`) %>% 
  summarise(Mean.Value = mean(Value))

mean_data$`Vegetation type` <- relevel(as.factor(mean_data$`Vegetation type`), 
                                       ref = "WWI")

#-------------------------------------------------------------------------------

# TDS
data.TDS <- mean_data %>% filter(Parameter == "Av.TDS")
model.TDS <- lm(Mean.Value~`Vegetation type`, data = data.TDS)
summary(model.TDS)
# p-value = 0.1306, Not significant

model.TDS_fitresid <- augment(model.TDS)

# shapiro wilk
shapiro.test(model.TDS_fitresid$.std.resid)
# p-value = 0.2821, Normal.
# QQ plot  ## OKAY
ggplot(model.TDS_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

durbinWatsonTest(model.TDS)
# p-value = 0.024, residuals are correlated.

# constant variance
bptest(model.TDS)
# p-value = 0.02257, variance is not constant.


#-------------------------------------------------------------------------------
# TSS
data.TSS <- mean_data %>% filter(Parameter == "Av.TSS")
model.TSS <- lm(Mean.Value~`Vegetation type`, data = data.TSS)
summary(model.TSS)
# p-value = 0.008069, Significant

model.TSS_fitresid <- augment(model.TSS)

# shapiro wilk
shapiro.test(model.TSS_fitresid$.std.resid)
# p-value = 2.528e-06, Not Normal.
# QQ plot  ## OKAY
ggplot(model.TSS_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

durbinWatsonTest(model.TSS)
# p-value = 0.204, residuals are not correlated.

# constant variance
bptest(model.TSS)
# p-value = 0.6258, variance is constant.


#-------------------------------------------------------------------------------
# Temp
data.Temp <- mean_data %>% filter(Parameter == "Av.Temp")
model.Temp <- lm(Mean.Value~`Vegetation type`, data = data.Temp)
summary(model.Temp)
# p-value = 0.1505, Not significant

model.Temp_fitresid <- augment(model.Temp)

# shapiro wilk
shapiro.test(model.Temp_fitresid$.std.resid)
# p-value = 0.8538, Normal.
# QQ plot  ## OKAY
ggplot(model.Temp_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

durbinWatsonTest(model.Temp)
# p-value = 0, residuals are correlated.

# constant variance
bptest(model.Temp)
# p-value = 0.03106, variance is not constant.


#-------------------------------------------------------------------------------
# Ph
data.Ph <- mean_data %>% filter(Parameter == "Av.pH")
model.Ph <- lm(Mean.Value~`Vegetation type`, data = data.Ph)
summary(model.Ph)
# p-value = 7.17e-06, Significant

model.Ph_fitresid <- augment(model.Ph)

# shapiro wilk
shapiro.test(model.Ph_fitresid$.std.resid)
# p-value = 0.9756, Normal.
# QQ plot  ## OKAY
ggplot(model.Ph_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

durbinWatsonTest(model.Ph)
# p-value =  0.094, residuals are not correlated.

# constant variance
bptest(model.Ph)
# p-value = 0.4782, variance is constant.



################################################################################

# After log transformation


# TDS
data.TDS <- mean_data %>% filter(Parameter == "Av.TDS")
model.TDS <- lm(log(Mean.Value)~`Vegetation type`, data = data.TDS)
summary(model.TDS)
# p-value = 0.2348, Not significant

model.TDS_fitresid <- augment(model.TDS)

# shapiro wilk
shapiro.test(model.TDS_fitresid$.std.resid)
# p-value = 0.4965, Normal.
# QQ plot  ## OKAY
ggplot(model.TDS_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")


durbinWatsonTest(model.TDS)
# p-value = 0.026, residuals are correlated.

# constant variance
bptest(model.TDS)
# p-value = 0.07118, variance is constant.


#-------------------------------------------------------------------------------
# TSS
data.TSS <- mean_data %>% filter(Parameter == "Av.TSS")
model.TSS <- lm(log(Mean.Value)~`Vegetation type`, data = data.TSS)
summary(model.TSS)
# p-value = 0.01742, Significant

model.TSS_fitresid <- augment(model.TSS)

# shapiro wilk
shapiro.test(model.TSS_fitresid$.std.resid)
# p-value = 0.01468, Not Normal.
# QQ plot  ## OKAY
ggplot(model.TSS_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")


durbinWatsonTest(model.TSS)
# p-value = 0.142, residuals are not correlated.

# constant variance
bptest(model.TSS)
# p-value = 0.03598, variance is not constant.


#-------------------------------------------------------------------------------
# Temp
data.Temp <- mean_data %>% filter(Parameter == "Av.Temp")
model.Temp <- lm(log(Mean.Value)~`Vegetation type`, data = data.Temp)
summary(model.Temp)
# p-value = 0.1431, Not significant

model.Temp_fitresid <- augment(model.Temp)

# shapiro wilk
shapiro.test(model.Temp_fitresid$.std.resid)
# p-value = 0.8916, Normal.
# QQ plot  ## OKAY
ggplot(model.Temp_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")


durbinWatsonTest(model.Temp)
# p-value = 0, residuals are correlated.

# constant variance
bptest(model.Temp)
# p-value = 0.02669, variance is not constant.


#-------------------------------------------------------------------------------
# Ph
data.Ph <- mean_data %>% filter(Parameter == "Av.pH")
model.Ph <- lm(log(Mean.Value)~`Vegetation type`, data = data.Ph)
summary(model.Ph)
# p-value = 7.262e-06, Significant

model.Ph_fitresid <- augment(model.Ph)

# shapiro wilk
shapiro.test(model.Ph_fitresid$.std.resid)
# p-value = 0.9641, Normal.
# QQ plot  ## OKAY
ggplot(model.Ph_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")


durbinWatsonTest(model.Ph)
# p-value =  0.086, residuals are not correlated.

# constant variance
bptest(model.Ph)
# p-value = 0.4536, variance is constant.






