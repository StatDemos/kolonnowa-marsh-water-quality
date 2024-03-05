# Normality Assumption Check QQ Plot


# Loading libraries
library(readxl)
library(tidyverse)
library(broom)
library(car)
library(lmtest)
library(patchwork)

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
# ANOVA

############################## with original variables ################################

# Alkalin
data.Alkalin <- mean_data %>% filter(Parameter == "Av.Alkalin")
model.Alkalin <- lm(Mean.Value~`Vegetation type`, data = data.Alkalin)
summary(model.Alkalin)
# p-value: 0.004985, significant

# res analysis
model.PO1_fitresid <- augment(model.Alkalin)

# shapiro wilk
shapiro.test(model.PO1_fitresid$.std.resid) 
# normality assumption not rejected
# QQ plot  ## OKAY
p1 <- ggplot(model.PO1_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Alkalin", x = "Expected", 
       y = "Residuals")

# residual autocorrelation

durbinWatsonTest(model.Alkalin)
# residuals are correlated

# constant variance
res <- bptest(model.Alkalin)
res
# variance is constant

#####################################################################################

# BOD
data.BOD <- mean_data %>% filter(Parameter == "Av.BOD")
model.BOD <- lm(Mean.Value~`Vegetation type`, data = data.BOD)
summary(model.BOD)
# p-value: 2.948e-05, significant

# res analysis
model.PO2_fitresid <- augment(model.BOD)

# shapiro wilk
shapiro.test(model.PO2_fitresid$.std.resid) 
# normality assumption is rejected
# QQ plot  ## OKAY
p2 <- ggplot(model.PO2_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "BOD", x = "Expected", 
       y = "Residuals")

# residual autocorrelation

durbinWatsonTest(model.BOD)
# residuals are not correlated

# constant variance
res <- bptest(model.BOD)
res
# variance is constant

#######################################################################################

# COD
data.COD <- mean_data %>% filter(Parameter == "Av.COD")
model.COD <- lm(Mean.Value~`Vegetation type`, data = data.COD)
summary(model.COD)
# p-value: 0.0001126, significant

# res analysis
model.PO3_fitresid <- augment(model.COD)

# shapiro wilk
shapiro.test(model.PO3_fitresid$.std.resid) 
# normality assumption is rejected
# QQ plot  ## OKAY
p3 <- ggplot(model.PO3_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "COD", x = "Expected", 
       y = "Residuals")

# residual autocorrelation

durbinWatsonTest(model.COD)
# residuals are not correlated

# constant variance
res <- bptest(model.COD)
res
# variance is constant

#######################################################################################

# DO
data.DO <- mean_data %>% filter(Parameter == "Av.DO")
model.DO <- lm(Mean.Value~`Vegetation type`, data = data.DO)
summary(model.DO)
# p-value: 0.2855, not significant

# res analysis
model.PO4_fitresid <- augment(model.DO)

# shapiro wilk
shapiro.test(model.PO4_fitresid$.std.resid) 
# normality assumption is rejected
# QQ plot  ## OKAY
p4 <- ggplot(model.PO4_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "DO", x = "Expected", 
       y = "Residuals")

# residual autocorrelation

durbinWatsonTest(model.DO)
# residuals are not correlated

# constant variance
res <- bptest(model.DO)
res
# variance is constant


#--------------------------------------------------------------------------------------

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
# QQ plot  ## OKAY
p5 <- ggplot(model.EC_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "EC", x = "Expected", 
       y = "Residuals")


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
# QQ plot  ## OKAY
p6 <- ggplot(model.NH4_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "NH4", x = "Expected", 
       y = "Residuals")


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
# QQ plot  ## OKAY
p7 <- ggplot(model.NO3_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "NO3", x = "Expected", 
       y = "Residuals")


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
# QQ plot  ## OKAY
p8 <- ggplot(model.PO4_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "PO4", x = "Expected", 
       y = "Residuals")


# durbin watson
durbinWatsonTest(model.PO4)
# p-value = 0.06, do not reject Ho - data are not correlated

# constant variance
bptest(model.PO4)
# p-value = 0.1373, do not reject Ho - variance constant




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
p9 <- ggplot(model.TDS_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "TDS", x = "Expected", 
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
p10 <- ggplot(model.TSS_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "TSS", x = "Expected", 
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
p11 <- ggplot(model.Temp_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Temp", x = "Expected", 
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
p12 <- ggplot(model.Ph_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Ph", x = "Expected", 
       y = "Residuals")

durbinWatsonTest(model.Ph)
# p-value =  0.094, residuals are not correlated.

# constant variance
bptest(model.Ph)
# p-value = 0.4782, variance is constant.


#-------------------------------------------------------------------------------


# Cd
data.Cd <- mean_data %>% filter(Parameter == "Cd")
model.Cd <- lm(Mean.Value~`Vegetation type`, data = data.Cd)
summary(model.Cd)
# p-value = 1.246e-11, Significant

# Assumption checking
model.Cd_fitresid <- augment(model.Cd)

# shapiro wilk
shapiro.test(model.Cd_fitresid$.std.resid) # not normal
# QQ plot
p13 <- ggplot(model.Cd_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Cd", x = "Expected", 
       y = "Residuals")

# residual autocorrelation
durbinWatsonTest(model.Cd) # okay

# constant variance
bptest(model.Cd) # okay

#-------------------------------------------------------------------------------

# Cr
data.Cr <- mean_data %>% filter(Parameter == "Cr")
model.Cr <- lm(Mean.Value~`Vegetation type`, data = data.Cr)
summary(model.Cr)
# p-value: < 2.2e-16, Significant

# Assumption checking
model.Cr_fitresid <- augment(model.Cr)

# shapiro wilk
shapiro.test(model.Cr_fitresid$.std.resid) # not normal
# QQ plot  ## OKAY
p14 <- ggplot(model.Cr_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Cr", x = "Expected", 
       y = "Residuals")


# residual autocorrelation
durbinWatsonTest(model.Cr) # not independent

# constant variance
bptest(model.Cr) # no constant variance

#-------------------------------------------------------------------------------

# Pb        
data.Pb <- mean_data %>% filter(Parameter == "Pb")
model.Pb <- lm(Mean.Value~`Vegetation type`, data = data.Pb)
summary(model.Pb)
# p-value = 5.145e-08, Significant

# Assumption checking
model.Pb_fitresid <- augment(model.Pb) 

# shapiro wilk 
shapiro.test(model.Pb_fitresid$.std.resid) # not normal
# QQ plot  ## OKAY
p15 <- ggplot(model.Pb_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Pb", x = "Expected", 
       y = "Residuals")

# residual autocorrelation
durbinWatsonTest(model.Pb) # okay

# constant variance
bptest(model.Pb) # okay



#-------------------------------------------------------------------------------

# Plot 

p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+p12+p13+p14+p15 +
  plot_layout(ncol = 3, nrow = 5, axis_titles = 'collect') +
  plot_annotation(title = "Normal Probability Plot")








