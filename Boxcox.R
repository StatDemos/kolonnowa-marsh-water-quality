# Loading libraries
library(readxl)
library(tidyverse)
library(broom)
library(car)
library(lmtest)
library(MASS)
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



# _______________________________________________________________________

## Cd
data.Cd <- mean_data %>% filter(Parameter == "Cd")

# Box Cox
bc <- boxcox(Mean.Value~`Vegetation type`, data = data.Cd)
(lambda <- bc$x[which.max(bc$y)])
lambda <- -1 # after rounding

#fit new model using the Box-Cox transformation
new_cd_model <- lm((Mean.Value^-1) ~ `Vegetation type`, 
                   data = data.Cd)
summary(new_cd_model)

# Assumption checking
model.Cd_fitresid_new <- augment(new_cd_model)

# shapiro wilk
shapiro.test(model.Cd_fitresid_new$.std.resid) # normal
# QQ plot
plot1 <- ggplot(model.Cd_fitresid_new, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Cd", x = "Expected", 
       y = "Residuals")

# constant variance
bptest(new_cd_model) 


# _______________________________________________________________________


## PO4

data.PO4 <- mean_data %>% filter(Parameter == "Av.PO4")

# Box Cox Transformation
bc1 <- boxcox(Mean.Value~`Vegetation type`, data = data.PO4)
(lambda <- bc1$x[which.max(bc1$y)])

lambda <- -1 # after rounding

#fit new model using the Box-Cox transformation
new_PO4_model <- lm((Mean.Value^-1) ~ `Vegetation type`, 
                 data = data.PO4)
summary(new_PO4_model)

# Assumption checking
model.po4_fitresid_new <- augment(new_PO4_model)

# shapiro wilk
shapiro.test(model.po4_fitresid_new$.std.resid) # normal
# QQ plot
plot2 <- ggplot(model.po4_fitresid_new, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "PO4", x = "Expected", 
       y = "Residuals")

# constant variance
bptest(new_PO4_model) 



# _______________________________________________________________________

# Plot 

plot1 + plot2 +
  plot_layout(ncol = 2, nrow = 1, axis_titles = 'collect') +
  plot_annotation(title = "Normal Probability Plot of Residuals")
