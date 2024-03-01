# Loading libraries
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
ggplot(model.PO1_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
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
ggplot(model.PO2_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
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
ggplot(model.PO3_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
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
ggplot(model.PO4_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# residual autocorrelation

durbinWatsonTest(model.DO)
# residuals are not correlated

# constant variance
res <- bptest(model.DO)
res
# variance is constant

########################################## with log transformation #############################

# Alkalin
data.Alkalin <- mean_data %>% filter(Parameter == "Av.Alkalin")
model.log.Alkalin <- lm(log(Mean.Value)~`Vegetation type`, data = data.Alkalin)
summary(model.log.Alkalin)
# p-value: 0.004825, significant

# res analysis
model.POl1_fitresid <- augment(model.log.Alkalin)

# shapiro wilk
shapiro.test(model.POl1_fitresid$.std.resid) 
# normality assumption not rejected
# QQ plot  ## OKAY
ggplot(model.POl1_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# residual autocorrelation

durbinWatsonTest(model.log.Alkalin)
# residuals are correlated

# constant variance
res <- bptest(model.log.Alkalin)
res
# variance is constant

################################################################################################

# BOD
data.BOD <- mean_data %>% filter(Parameter == "Av.BOD")
model.log.BOD <- lm(log(Mean.Value)~`Vegetation type`, data = data.BOD)
summary(model.log.BOD)
# p-value: 0.0003265, significant

# res analysis
model.POl2_fitresid <- augment(model.log.BOD)

# shapiro wilk
shapiro.test(model.POl2_fitresid$.std.resid) 
# normality assumption is rejected
# QQ plot  ## OKAY
ggplot(model.POl2_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# residual autocorrelation

durbinWatsonTest(model.log.BOD)
# residuals are correlated

# constant variance
res <- bptest(model.log.BOD)
res
# variance is constant

##############################################################################################

# COD
data.COD <- mean_data %>% filter(Parameter == "Av.COD")
model.log.COD <- lm(log(Mean.Value)~`Vegetation type`, data = data.COD)
summary(model.log.COD)
# p-value: 0.001103, significant

# res analysis
model.POl3_fitresid <- augment(model.log.COD)

# shapiro wilk
shapiro.test(model.POl3_fitresid$.std.resid) 
# normality assumption is rejected
# QQ plot  ## OKAY
ggplot(model.POl3_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# residual autocorrelation

durbinWatsonTest(model.log.COD)
# residuals are not correlated

# constant variance
res <- bptest(model.log.COD)
res
# variance is constant

##################################################################################################

# DO
data.DO <- mean_data %>% filter(Parameter == "Av.DO")
model.log.DO <- lm(log(Mean.Value)~`Vegetation type`, data = data.DO)
summary(model.log.DO)
# p-value: 0.1676, not significant

# res analysis
model.POl4_fitresid <- augment(model.log.DO)

# shapiro wilk
shapiro.test(model.POl4_fitresid$.std.resid) 
# normality assumption is rejected
# QQ plot  ## OKAY
ggplot(model.POl4_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# residual autocorrelation

durbinWatsonTest(model.log.DO)
# residuals are not correlated

# constant variance
res <- bptest(model.log.DO)
res
# variance is constant

