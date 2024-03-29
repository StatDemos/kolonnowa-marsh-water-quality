data <- readxl::read_xlsx("water quality monthly variation.xlsx")

# Load libraries
library(tidyverse)
library(ggplot2)
library(fpp3)


# Time Series Plots
data %>%
  filter(`Vegetation type` == "WP") %>%
  ggplot(aes(x=`Month/yr`, y=Av.NO3, col = as.factor(Location))) +
  geom_line()
  

# Vegetation wise graphs

data2 <- data %>% pivot_longer(cols = 4:18, names_to = "para_name", 
                               values_to = "para_val")

data2 %>% 
  filter(`Vegetation type` == "WP") %>%
  ggplot(aes(x=`Month/yr`, y=para_val, col = as.factor(Location))) +
  geom_line() + facet_wrap(vars(para_name), scales = "free_y")


data2 %>% 
  filter(`Vegetation type` == "WWI") %>%
  ggplot(aes(x=`Month/yr`, y=para_val, col = as.factor(Location))) +
  geom_line() + facet_wrap(vars(para_name), scales = "free_y")

data2 %>% 
  filter(`Vegetation type` == "CA") %>%
  ggplot(aes(x=`Month/yr`, y=para_val, col = as.factor(Location))) +
  geom_line() + facet_wrap(vars(para_name), scales = "free_y")

data2 %>% 
  filter(`Vegetation type` == "SG") %>%
  ggplot(aes(x=`Month/yr`, y=para_val, col = as.factor(Location))) +
  geom_line() + facet_wrap(vars(para_name), scales = "free_y")

data2 %>% 
  filter(`Vegetation type` == "MV") %>%
  ggplot(aes(x=`Month/yr`, y=para_val, col = as.factor(Location))) +
  geom_line() + facet_wrap(vars(para_name), scales = "free_y")

data2 %>% 
  filter(`Vegetation type` == "SV") %>%
  ggplot(aes(x=`Month/yr`, y=para_val, col = as.factor(Location))) +
  geom_line() + facet_wrap(vars(para_name), scales = "free_y")

##############################################



data$`Month/yr` <- as.Date(data$`Month/yr`)

data$`Month/yr` <-  yearmonth(data$`Month/yr`)

# temp
data <- data %>% select(Location, "Vegetation type", "Month/yr", Av.Temp) %>% 
  as_tibble() %>% 
  as_tsibble(index = `Month/yr`, key = Location)

autoplot(data, Av.Temp) 


# ph
data <- data %>% select(Location, "Vegetation type", "Month/yr", Av.pH) %>% 
  as_tibble() %>% 
  as_tsibble(index = `Month/yr`, key = Location)

autoplot(data, Av.pH) 

  
  
  