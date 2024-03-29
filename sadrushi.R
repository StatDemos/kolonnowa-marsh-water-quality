# Loading libraries
library(readxl)
library(tidyverse)
library(broom)
library(car)
library(lmtest)
library(MASS)

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


# Anova

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
ggplot(model.Cd_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals - Cd", x = "Expected", 
       y = "Residuals")

# residual autocorrelation
durbinWatsonTest(model.Cd) # okay

# constant variance
bptest(model.Cd) # okay

# Box Cox
bc <- boxcox(Mean.Value~`Vegetation type`, data = data.Cd)
(lambda <- bc$x[which.max(bc$y)])
lambda <- -1

#fit new linear regression model using the Box-Cox transformation
new_model <- lm(((Mean.Value^lambda-1)/lambda) ~ `Vegetation type`, data = data.Cd)
summary(new_model)

# Assumption checking
model.Cd_fitresid1 <- augment(new_model)

# shapiro wilk
shapiro.test(model.Cd_fitresid1$.std.resid) # not normal
# QQ plot
ggplot(model.Cd_fitresid1, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals - Cd", x = "Expected", 
       y = "Residuals")



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
ggplot(model.Cr_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals - Cr", x = "Expected", 
       y = "Residuals")


# residual autocorrelation
durbinWatsonTest(model.Cr) # not independent

# constant variance
bptest(model.Cr) # no constant variance


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
ggplot(model.Pb_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals - Pb", x = "Expected", 
       y = "Residuals")

# residual autocorrelation
durbinWatsonTest(model.Pb) # okay

# constant variance
bptest(model.Pb) # okay


############## LOG TRANSFORMATION ###############

# Cd
data.Cd <- mean_data %>% filter(Parameter == "Cd")
model.Cd <- lm(log(Mean.Value)~`Vegetation type`, data = data.Cd)
summary(model.Cd)
# p-value = 2.386e-06, Significant

# Assumption checking
model.Cd_fitresid <- augment(model.Cd)

# shapiro wilk
shapiro.test(model.Cd_fitresid$.std.resid) # not normal
# QQ plot  ## BETTER THAN FIRST
ggplot(model.Cd_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# residual autocorrelation
durbinWatsonTest(model.Cd) # not okay

# constant variance
bptest(model.Cd) # not okay



# Cr
data.Cr <- mean_data %>% filter(Parameter == "Cr")
model.Cr <- lm(log(Mean.Value)~`Vegetation type`, data = data.Cr)
summary(model.Cr)
# p-value: 3.015e-16, Significant

# Assumption checking
model.Cr_fitresid <- augment(model.Cr)

# shapiro wilk
shapiro.test(model.Cr_fitresid$.std.resid) # normal
# QQ plot  ## OKAY
ggplot(model.Cr_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# residual autocorrelation
durbinWatsonTest(model.Cr) # not independent

# constant variance
bptest(model.Cr) # okay


# Pb        
data.Pb <- mean_data %>% filter(Parameter == "Pb")
model.Pb <- lm(log(Mean.Value)~`Vegetation type`, data = data.Pb)
summary(model.Pb)
# p-value = 8.162e-08, Significant

# Assumption checking
model.Pb_fitresid <- augment(model.Pb) 

# shapiro wilk
shapiro.test(model.Pb_fitresid$.std.resid) # normal
# QQ plot  ## OKAY
ggplot(model.Pb_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# residual autocorrelation
durbinWatsonTest(model.Pb) # okay

# constant variance
bptest(model.Pb) # okay

