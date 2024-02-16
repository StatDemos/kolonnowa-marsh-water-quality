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

data.Alkalin <- mean_data %>% filter(Parameter == "Av.Alkalin")
model.Alkalin <- lm(Mean.Value~`Vegetation type`, data = data.Alkalin)
summary(model.Alkalin)
