# Normality Test

library(broom)

#-------------------------------------------------------------------------------
# TDS
model.TDS_fitresid <- augment(model.TDS)

# shapiro wilk
shapiro.test(model.TDS_fitresid$.std.resid)
# p-value = 0.2821, Normal.

library(car)
durbinWatsonTest(model.TDS)
# p-value = 0.024, residuals are correlated.

# constant variance
library(lmtest)
bptest(model.TDS)
# p-value = 0.02257, variance is not constant.


#-------------------------------------------------------------------------------
# TSS
model.TSS_fitresid <- augment(model.TSS)

# shapiro wilk
shapiro.test(model.TSS_fitresid$.std.resid)
# p-value = 2.528e-06, Not Normal.

library(car)
durbinWatsonTest(model.TSS)
# p-value = 0.204, residuals are not correlated.

# constant variance
library(lmtest)
bptest(model.TSS)
# p-value = 0.6258, variance is constant.


#-------------------------------------------------------------------------------
# Temp
model.Temp_fitresid <- augment(model.Temp)

# shapiro wilk
shapiro.test(model.Temp_fitresid$.std.resid)
# p-value = 0.8538, Normal.

library(car)
durbinWatsonTest(model.Temp)
# p-value = 0, residuals are correlated.

# constant variance
library(lmtest)
bptest(model.Temp)
# p-value = 0.03106, variance is not constant.


#-------------------------------------------------------------------------------
# Ph
model.Ph_fitresid <- augment(model.Ph)

# shapiro wilk
shapiro.test(model.Ph_fitresid$.std.resid)
# p-value = 0.9756, Normal.

library(car)
durbinWatsonTest(model.Ph)
# p-value =  0.094, residuals are not correlated.

# constant variance
library(lmtest)
bptest(model.Ph)
# p-value = 0.4782, variance is constant.


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# After log transformation

# TDS
data.TDS <- mean_data %>% filter(Parameter == "Av.TDS")
model.TDS <- lm(log(Mean.Value)~`Vegetation type`, data = data.TDS)
summary(model.TDS)
# p-value = 0.2348, Not significant

# TSS
data.TSS <- mean_data %>% filter(Parameter == "Av.TSS")
model.TSS <- lm(log(Mean.Value)~`Vegetation type`, data = data.TSS)
summary(model.TSS)
# p-value = 0.01742, Significant

# Temp
data.Temp <- mean_data %>% filter(Parameter == "Av.Temp")
model.Temp <- lm(log(Mean.Value)~`Vegetation type`, data = data.Temp)
summary(model.Temp)
# p-value = 0.1431, Not significant

# Ph
data.Ph <- mean_data %>% filter(Parameter == "Av.pH")
model.Ph <- lm(log(Mean.Value)~`Vegetation type`, data = data.Ph)
summary(model.Ph)
# p-value = 7.262e-06, Significant


#-------------------------------------------------------------------------------
# TDS
model.TDS_fitresid <- augment(model.TDS)

# shapiro wilk
shapiro.test(model.TDS_fitresid$.std.resid)
# p-value = 0.4965, Normal.

library(car)
durbinWatsonTest(model.TDS)
# p-value = 0.026, residuals are correlated.

# constant variance
library(lmtest)
bptest(model.TDS)
# p-value = 0.07118, variance is constant.


#-------------------------------------------------------------------------------
# TSS
model.TSS_fitresid <- augment(model.TSS)

# shapiro wilk
shapiro.test(model.TSS_fitresid$.std.resid)
# p-value = 0.01468, Not Normal.

library(car)
durbinWatsonTest(model.TSS)
# p-value = 0.142, residuals are not correlated.

# constant variance
library(lmtest)
bptest(model.TSS)
# p-value = 0.03598, variance is not constant.


#-------------------------------------------------------------------------------
# Temp
model.Temp_fitresid <- augment(model.Temp)

# shapiro wilk
shapiro.test(model.Temp_fitresid$.std.resid)
# p-value = 0.8916, Normal.

library(car)
durbinWatsonTest(model.Temp)
# p-value = 0, residuals are correlated.

# constant variance
library(lmtest)
bptest(model.Temp)
# p-value = 0.02669, variance is not constant.


#-------------------------------------------------------------------------------
# Ph
model.Ph_fitresid <- augment(model.Ph)

# shapiro wilk
shapiro.test(model.Ph_fitresid$.std.resid)
# p-value = 0.9641, Normal.

library(car)
durbinWatsonTest(model.Ph)
# p-value =  0.086, residuals are not correlated.

# constant variance
library(lmtest)
bptest(model.Ph)
# p-value = 0.4536, variance is constant.


















