data <- readxl::read_xlsx("water quality monthly variation.xlsx")

# Load libraries
library(tidyverse)
library(ggplot2)


# Time Series Plots
data %>%
  filter(`Vegetation type` == "WP") %>%
  ggplot(aes(x=`Month/yr`, y=Av.NO3, col = as.factor(Location))) +
  geom_line()
  