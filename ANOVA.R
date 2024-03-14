# Loading libraries
library(readxl)
library(tidyverse)
library(writexl)
 
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

mean_data <- mean_data %>% rename(Vegetation.type = "Vegetation type")

write_xlsx(mean_data, "water.quality.data.xlsx")
  
# ANOVA

# Alkalin
data.Alkalin <- mean_data %>% filter(Parameter == "Av.Alkalin")
model.Alkalin <- lm(Mean.Value~Vegetation.type, data = data.Alkalin)
summary(model.Alkalin)
# p-value: 0.004985, significant

model.Alkalin.new <- aov(model.Alkalin)
tukey.alkalin <- TukeyHSD(model.Alkalin.new, conf.level=.95) 
plot(tukey.alkalin, las = 2, title =  )


# BOD
data.BOD <- mean_data %>% filter(Parameter == "Av.BOD")
model.BOD <- lm(Mean.Value~Vegetation.type, data = data.BOD)
summary(model.BOD)
# p-value: 2.948e-05, significant

model.BOD.new <- aov(model.BOD)
tukey.BOD <- TukeyHSD(model.BOD.new, conf.level=.95) 
plot(tukey.BOD, las = 2 )


# COD
data.COD <- mean_data %>% filter(Parameter == "Av.COD")
model.COD <- lm(Mean.Value~Vegetation.type, data = data.COD)
summary(model.COD)
# p-value: 0.0001126, significant

model.COD.new <- aov(model.COD)
tukey.COD <- TukeyHSD(model.COD.new, conf.level=.95) 
plot(tukey.COD, las = 2 )


# DO
data.DO <- mean_data %>% filter(Parameter == "Av.DO")
model.DO <- lm(Mean.Value~`Vegetation type`, data = data.DO)
summary(model.DO)
# p-value: 0.2855, not significant

# EC
data.EC <- mean_data %>% filter(Parameter == "Av.EC")
model.EC <- lm(Mean.Value~`Vegetation type`, data = data.EC)
summary(model.EC)
# p-value: 0.9794, not significant

# NH4
data.NH4 <- mean_data %>% filter(Parameter == "Av.NH4")
model.NH4 <- lm(Mean.Value~Vegetation.type, data = data.NH4)
summary(model.NH4)
# p-value: 0.000164, significant

model.NH4.new <- aov(model.NH4)
tukey.NH4 <- TukeyHSD(model.NH4.new, conf.level=.95) 
plot(tukey.NH4, las = 2 )


# NO3
data.NO3 <- mean_data %>% filter(Parameter == "Av.NO3")
model.NO3 <- lm(Mean.Value~Vegetation.type, data = data.NO3)
summary(model.NO3)
# p-value: 0.00129, significant

model.NO3.new <- aov(model.NO3)
tukey.NO3 <- TukeyHSD(model.NO3.new, conf.level=.95) 
plot(tukey.NO3, las = 2 )


# PO4
data.PO4 <- mean_data %>% filter(Parameter == "Av.PO4")
model.PO4 <- lm(Mean.Value~Vegetation.type, data = data.PO4)
summary(model.PO4)
# p-value: 2.991e-05, significant

model.PO4.new <- aov(model.PO4)
tukey.PO4 <- TukeyHSD(model.PO4.new, conf.level=.95) 
plot(tukey.PO4, las = 2 )


# TDS
data.TDS <- mean_data %>% filter(Parameter == "Av.TDS")
model.TDS <- lm(Mean.Value~`Vegetation type`, data = data.TDS)
summary(model.TDS)
# p-value = 0.1306, Not significant

# TSS
data.TSS <- mean_data %>% filter(Parameter == "Av.TSS")
model.TSS <- lm(Mean.Value~Vegetation.type, data = data.TSS)
summary(model.TSS)
# p-value = 0.008069, Significant

model.TSS.new <- aov(model.TSS)
tukey.TSS <- TukeyHSD(model.TSS.new, conf.level=.95) 
plot(tukey.TSS, las = 2 )


# Temp
data.Temp <- mean_data %>% filter(Parameter == "Av.Temp")
model.Temp <- lm(Mean.Value~`Vegetation type`, data = data.Temp)
summary(model.Temp)
# p-value = 0.1505, Not significant

# Ph
data.Ph <- mean_data %>% filter(Parameter == "Av.pH")
model.Ph <- lm(Mean.Value~Vegetation.type, data = data.Ph)
summary(model.Ph)
# p-value = 7.17e-06, Significant

model.Ph.new <- aov(model.Ph)
tukey.Ph <- TukeyHSD(model.Ph.new, conf.level=.95) 
plot(tukey.Ph, las = 2)


# Cd
data.Cd <- mean_data %>% filter(Parameter == "Cd")
model.Cd <- lm(Mean.Value~Vegetation.type, data = data.Cd)
summary(model.Cd)
# p-value = 1.246e-11, Significant

model.Cd.new <- aov(model.Cd)
tukey.Cd <- TukeyHSD(model.Cd.new, conf.level=.95) 
plot(tukey.Cd, las = 2)

# Cr
data.Cr <- mean_data %>% filter(Parameter == "Cr")
model.Cr <- lm(Mean.Value~Vegetation.type, data = data.Cr)
summary(model.Cr)
# p-value: < 2.2e-16, Significant

model.Cr.new <- aov(model.Cr)
tukey.Cr <- TukeyHSD(model.Cr.new, conf.level=.95) 
plot(tukey.Cr, las = 2)

# Pb        
data.Pb <- mean_data %>% filter(Parameter == "Pb")
model.Pb <- lm(Mean.Value~Vegetation.type, data = data.Pb)
summary(model.Pb)
# p-value = 5.145e-08, Significant

model.Pb.new <- aov(model.Pb)
tukey.Pb <- TukeyHSD(model.Pb.new, conf.level=.95) 
plot(tukey.Pb, las = 2)

################################################################################
# Nonparametric test - kruskal wallis, non normal error

kruskal.test(Mean.Value ~ `Vegetation type`, data = data.Alkalin) 
# p-value = 0.01014, significant
kruskal.test(Mean.Value ~ `Vegetation type`, data = data.BOD) 
#p-value = 0.02025, significant
kruskal.test(Mean.Value ~ `Vegetation type`, data = data.COD)
# p-value = 0.1424, not significant
kruskal.test(Mean.Value ~ `Vegetation type`, data = data.DO)
# p-value = 0.1466, not significant
kruskal.test(Mean.Value ~ `Vegetation type`, data = data.EC)
# p-value = 0.7539, not significant
kruskal.test(Mean.Value ~ `Vegetation type`, data = data.NH4)
# p-value = 0.02195, significant
kruskal.test(Mean.Value ~ `Vegetation type`, data = data.NO3)
# p-value = 0.1925, not significant
kruskal.test(Mean.Value ~ `Vegetation type`, data = data.PO4)
# p-value = 0.008197, significant
kruskal.test(Mean.Value ~ `Vegetation type`, data = data.TDS)
# p-value = 0.4074, not significant
kruskal.test(Mean.Value ~ `Vegetation type`, data = data.TSS) 
# p-value = 0.1677, not significant
kruskal.test(Mean.Value ~ `Vegetation type`, data = data.Temp) 
# p-value = 0.2341, not significant
kruskal.test(Mean.Value ~ `Vegetation type`, data = data.Ph)
# p-value = 0.0001961, significant
kruskal.test(Mean.Value ~ `Vegetation type`, data = data.Cd) 
# p-value = 0.0002007, significant
kruskal.test(Mean.Value ~ `Vegetation type`, data = data.Cr) 
# p-value = 6.817e-07, significant
kruskal.test(Mean.Value ~ `Vegetation type`, data = data.Pb) 
# p-value = 1.762e-05, significant


########################################################################
# MANOVA

manova_data <- mean_data %>% pivot_wider(names_from = Parameter, 
                                         values_from = Mean.Value)


#fit the MANOVA model - parametric
manova_model_para <- manova(cbind(Av.Alkalin, Av.BOD, Av.COD, Av.NH4, Av.NO3, 
                                  Av.PO4, Av.TSS,
                      Av.pH, Cd, Cr, Pb) ~ `Vegetation type`, 
                      data = manova_data)
summary(manova_model_para)
# Look to see which differ
summary.aov(manova_model_para)


#fit the MANOVA model - nonparametric
manova_model_nonpara <- manova(cbind(Av.Alkalin, Av.BOD, Av.NH4, Av.PO4, 
                                  Av.pH, Cd, Cr, Pb) ~ `Vegetation type`, 
                               data = manova_data)
summary(manova_model_nonpara)
# Look to see which differ
summary.aov(manova_model_nonpara)


# Considering all variables
manova_model_all <- manova(cbind(Av.Alkalin, Av.BOD, Av.COD, Av.NH4, Av.NO3, 
                                 Av.PO4, 
                                  Av.pH, Cd, Cr, Pb, Av.Temp, Av.TSS, Av.TDS, 
                                 Av.EC, Av.DO) ~ `Vegetation type`, 
                           data = manova_data)
summary(manova_model_all)
# Look to see which differ
summary.aov(manova_model_all)

# Multivariate Normality
library(mvnormtest)
manova_data$Av.pH[17] <- mean(manova_data$Av.pH)
C <- t(manova_data[1:47, 3:17])
mshapiro.test(C)

#fit the MANOVA model - parametric with normal variables
manova_model_para <- manova(cbind(Av.Alkalin, Av.BOD, Av.COD, Av.NO3, 
                                  Av.pH, Cr, Pb) ~ `Vegetation type`, 
                            data = manova_data)
summary(manova_model_para)
# Look to see which differ
summary.aov(manova_model_para)

# multivariate normality

normaldata <- c("Av.Alkalin", "Av.BOD", "Av.COD", "Av.NO3", 
                "Av.pH", "Cr", "Pb")

# method 1
library(MVN)
manova_data$Av.pH[17] <- mean(manova_data$Av.pH, na.rm = TRUE)
mvn(manova_data[, normaldata], multivariatePlot = "qq", multivariateOutlierMethod = "quan")

# method 2
library(mvnormtest)
C <- t(manova_data[1:47, normaldata])
mshapiro.test(C)



##################### correlation among parameters #########################

data <- manova_data %>% select(Av.Alkalin, Av.BOD, Av.COD, Av.NH4, Av.NO3, 
                               Av.PO4, Av.TSS,
                               Av.pH, Cd, Cr, Pb)

library(Hmisc)

#create matrix of correlation coefficients and p-values
rcorr(as.matrix(data))

library(corrplot)

#visualize correlation matrix
corrplot(cor(data))
