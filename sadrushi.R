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

# residual autocorrelation
durbinWatsonTest(model.Cd) # okay

# constant variance
bptest(model.Cd) # okay



# Cr
data.Cr <- mean_data %>% filter(Parameter == "Cr")
model.Cr <- lm(Mean.Value~`Vegetation type`, data = data.Cr)
summary(model.Cr)
# p-value: < 2.2e-16, Significant

# Assumption checking
model.Cr_fitresid <- augment(model.Cr)

# shapiro wilk
shapiro.test(model.Cr_fitresid$.std.resid) # not normal

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

# residual autocorrelation
durbinWatsonTest(model.Pb) # okay

# constant variance
bptest(model.Pb) # okay

