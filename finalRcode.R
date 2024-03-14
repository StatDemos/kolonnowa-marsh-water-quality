# Loading libraries
library(readxl)
library(tidyverse)
library(MASS)
library(broom)
library(car)
library(lmtest)
library(mvnormtest)
library(patchwork)

#############################################################################
#   WATER QUALITY ANALYSIS
#############################################################################

# Loading Dataset
mean_data <- read_excel("water.quality.data.xlsx")

################################ ANOVA MODELS ################################

# Alkalin
data.Alkalin <- mean_data %>% filter(Parameter == "Av.Alkalin")
model.Alkalin <- lm(Mean.Value~Vegetation.type, data = data.Alkalin)
summary(model.Alkalin)
# p-value: 0.004985, significant


# BOD
data.BOD <- mean_data %>% filter(Parameter == "Av.BOD")
model.BOD <- lm(Mean.Value~Vegetation.type, data = data.BOD)
summary(model.BOD)
# p-value: 2.948e-05, significant


# COD
data.COD <- mean_data %>% filter(Parameter == "Av.COD")
model.COD <- lm(Mean.Value~Vegetation.type, data = data.COD)
summary(model.COD)
# p-value: 0.0001126, significant


# DO
data.DO <- mean_data %>% filter(Parameter == "Av.DO")
model.DO <- lm(Mean.Value~Vegetation.type, data = data.DO)
summary(model.DO)
# p-value: 0.2855, not significant


# EC
data.EC <- mean_data %>% filter(Parameter == "Av.EC")
model.EC <- lm(Mean.Value~Vegetation.type, data = data.EC)
summary(model.EC)
# p-value: 0.9794, not significant


# NH4
data.NH4 <- mean_data %>% filter(Parameter == "Av.NH4")
model.NH4 <- lm(Mean.Value~Vegetation.type, data = data.NH4)
summary(model.NH4)
# p-value: 0.000164, significant


# NO3
data.NO3 <- mean_data %>% filter(Parameter == "Av.NO3")
model.NO3 <- lm(Mean.Value~Vegetation.type, data = data.NO3)
summary(model.NO3)
# p-value: 0.00129, significant


# PO4
data.PO4 <- mean_data %>% filter(Parameter == "Av.PO4")
model.PO4 <- lm(Mean.Value~Vegetation.type, data = data.PO4)
summary(model.PO4)
# p-value: 2.991e-05, significant


# TDS
data.TDS <- mean_data %>% filter(Parameter == "Av.TDS")
model.TDS <- lm(Mean.Value~Vegetation.type, data = data.TDS)
summary(model.TDS)
# p-value = 0.1306, Not significant


# TSS
data.TSS <- mean_data %>% filter(Parameter == "Av.TSS")
model.TSS <- lm(Mean.Value~Vegetation.type, data = data.TSS)
summary(model.TSS)
# p-value = 0.008069, Significant


# Temp
data.Temp <- mean_data %>% filter(Parameter == "Av.Temp")
model.Temp <- lm(Mean.Value~Vegetation.type, data = data.Temp)
summary(model.Temp)
# p-value = 0.1505, Not significant


# Ph
data.Ph <- mean_data %>% filter(Parameter == "Av.pH")
model.Ph <- lm(Mean.Value~Vegetation.type, data = data.Ph)
summary(model.Ph)
# p-value = 7.17e-06, Significant


# Cd
data.Cd <- mean_data %>% filter(Parameter == "Cd")
model.Cd <- lm(Mean.Value~Vegetation.type, data = data.Cd)
summary(model.Cd)
# p-value = 1.246e-11, Significant


# Cr
data.Cr <- mean_data %>% filter(Parameter == "Cr")
model.Cr <- lm(Mean.Value~Vegetation.type, data = data.Cr)
summary(model.Cr)
# p-value: < 2.2e-16, Significant


# Pb        
data.Pb <- mean_data %>% filter(Parameter == "Pb")
model.Pb <- lm(Mean.Value~Vegetation.type, data = data.Pb)
summary(model.Pb)
# p-value = 5.145e-08, Significant


################ Normality Test - Normal Probability Plot  ###################

# EC
model.EC_fitresid <- augment(model.EC)
ggplot(model.EC_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# NH4
model.NH4_fitresid <- augment(model.NH4)
ggplot(model.NH4_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# NO3
model.NO3_fitresid <- augment(model.NO3)
ggplot(model.NO3_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# PO4
model.PO4_fitresid <- augment(model.PO4)
ggplot(model.PO4_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# Alkalin
model.Alkalin_fitresid <- augment(model.Alkalin)
ggplot(model.Alkalin_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# BOD
model.BOD_fitresid <- augment(model.BOD)
ggplot(model.BOD_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# COD
model.COD_fitresid <- augment(model.COD)
ggplot(model.COD_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# DO
model.DO_fitresid <- augment(model.DO)
ggplot(model.PO4_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# TDS
model.TDS_fitresid <- augment(model.TDS)
ggplot(model.TDS_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# TSS
model.TSS_fitresid <- augment(model.TSS)
ggplot(model.TSS_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# Temp
model.Temp_fitresid <- augment(model.Temp)
ggplot(model.Temp_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# Ph
model.Ph_fitresid <- augment(model.Ph)
ggplot(model.Ph_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# Cd
model.Cd_fitresid <- augment(model.Cd)
ggplot(model.Cd_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals - Cd", x = "Expected", 
       y = "Residuals")

# Cr
model.Cr_fitresid <- augment(model.Cr)
ggplot(model.Cr_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals - Cr", x = "Expected", 
       y = "Residuals")

# Pb 
model.Pb_fitresid <- augment(model.Pb)
ggplot(model.Pb_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals - Pb", x = "Expected", 
       y = "Residuals")

################ Constant Variance Assumption Checking   ###################


# EC
# constant variance
bptest(model.EC)

# NH4
# constant variance
bptest(model.NH4)

# NO3
# constant variance
bptest(model.NO3)

# Alkalin
# constant variance
bptest(model.Alkalin)

# PO4
bptest(model.PO4)

# BOD
# constant variance
bptest(model.BOD)

# COD
# constant variance
bptest(model.COD)

# DO
# constant variance
bptest(model.DO)

# TDS
# constant variance
bptest(model.TDS)

# TSS
# constant variance
bptest(model.TSS)

# Temp
# constant variance
bptest(model.Temp)

# Ph
# constant variance
bptest(model.Ph)

# Cd
# constant variance
bptest(model.Cd) 

# Cr
# constant variance
bptest(model.Cr) 

# Pb        
# constant variance
bptest(model.Pb) 


####################### Box-Cox Transformation   #############################

# Cd
# Box Cox Transformation
bc <- boxcox(Mean.Value~Vegetation.type, data = data.Cd)
(lambda <- bc$x[which.max(bc$y)])
lambda <- -1

#fit new linear regression model using the Box-Cox transformation
new_model <- lm(((Mean.Value^lambda-1)/lambda) ~ Vegetation.type, data = data.Cd)
summary(new_model)

# Assumption checking
model.Cd_fitresid1 <- augment(new_model)

# shapiro wilk
shapiro.test(model.Cd_fitresid1$.std.resid) 

# QQ plot
ggplot(model.Cd_fitresid1, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals - Cd", x = "Expected", 
       y = "Residuals")

# PO4
# Box Cox Transformation
bc1 <- boxcox(Mean.Value~Vegetation.type, data = data.PO4)
(lambda <- bc1$x[which.max(bc1$y)])
lambda <- -1

#fit new linear regression model using the Box-Cox transformation
new_model1 <- lm(((Mean.Value^lambda-1)/lambda) ~ Vegetation.type, 
                 data = data.PO4)
summary(new_model1)

# Assumption checking
model.po4_fitresid <- augment(new_model1)

# shapiro wilk
shapiro.test(model.po4_fitresid$.std.resid) 
# QQ plot
ggplot(model.po4_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals - PO4", x = "Expected", 
       y = "Residuals")


################################ MANOVA MODELS ################################

# creating data for MANOVA model
manova_data <- mean_data %>% pivot_wider(names_from = Parameter, 
                                         values_from = Mean.Value)

#fit the MANOVA model - parametric
manova_model_para <- manova(cbind(Av.Alkalin, Av.BOD, Av.COD, Av.NH4, Av.NO3, 
                                  (1/Av.PO4), Av.TSS,
                                  Av.pH, (1/Cd), Cr, Pb) ~ Vegetation.type, 
                            data = manova_data)
summary(manova_model_para)


################################ TUKEY TEST ################################

# Alkalin
model.Alkalin.new <- aov(model.Alkalin)
tukey.alkalin <- TukeyHSD(model.Alkalin.new, conf.level=.95) 
plot(tukey.alkalin, las = 2)

# BOD
model.BOD.new <- aov(model.BOD)
tukey.BOD <- TukeyHSD(model.BOD.new, conf.level=.95) 
plot(tukey.BOD, las = 2 )

# COD
model.COD.new <- aov(model.COD)
tukey.COD <- TukeyHSD(model.COD.new, conf.level=.95) 
plot(tukey.COD, las = 2 )

# NH4
model.NH4.new <- aov(model.NH4)
tukey.NH4 <- TukeyHSD(model.NH4.new, conf.level=.95) 
plot(tukey.NH4, las = 2 )

# NO3
model.NO3.new <- aov(model.NO3)
tukey.NO3 <- TukeyHSD(model.NO3.new, conf.level=.95) 
plot(tukey.NO3, las = 2 )

# PO4
model.PO4.new <- aov(model.PO4)
tukey.PO4 <- TukeyHSD(model.PO4.new, conf.level=.95) 
plot(tukey.PO4, las = 2 )

# TSS
model.TSS.new <- aov(model.TSS)
tukey.TSS <- TukeyHSD(model.TSS.new, conf.level=.95) 
plot(tukey.TSS, las = 2 )

# Ph
model.Ph.new <- aov(model.Ph)
tukey.Ph <- TukeyHSD(model.Ph.new, conf.level=.95) 
plot(tukey.Ph, las = 2)

# Cd
model.Cd.new <- aov(model.Cd)
tukey.Cd <- TukeyHSD(model.Cd.new, conf.level=.95) 
plot(tukey.Cd, las = 2)

# Cr
model.Cr.new <- aov(model.Cr)
tukey.Cr <- TukeyHSD(model.Cr.new, conf.level=.95) 
plot(tukey.Cr, las = 2)

# Pb        
model.Pb.new <- aov(model.Pb)
tukey.Pb <- TukeyHSD(model.Pb.new, conf.level=.95) 
plot(tukey.Pb, las = 2)



#############################################################################
#   SOIL QUALITY ANALYSIS
#############################################################################

# Data
Soil_data <- read_excel("soil Analysis summery.xlsx")

# MANOVA model
manova_model <- manova(cbind(`Moisture content`, OMC, IMC, Bulk.D) ~ `Vegetation type`, 
                       data = Soil_data)
summary(manova_model)
# p-value = 0.6436, not significant.

# Look to see which differ
summary.aov(manova_model)

# Shapirowilk Test
shapiro.test(sqrt(Soil_data$`Moisture content`))
shapiro.test(sqrt(Soil_data$OMC))
shapiro.test(sqrt(Soil_data$IMC))
shapiro.test(sqrt(Soil_data$Bulk.D))

p1 <- ggplot(Soil_data, aes(sample=Soil_data$`Moisture content`)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Moisture content", x = "Expected", y = "Observed")

p2 <- ggplot(Soil_data, aes(sample=Soil_data$OMC)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "OMC", x = "Expected", y = "Observed")

p3 <- ggplot(Soil_data, aes(sample=Soil_data$IMC)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "IMC", x = "Expected", y = "Observed")

p4 <- ggplot(Soil_data, aes(sample=Soil_data$Bulk.D)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Bulk.D", x = "Expected", y = "Observed")


# Plot
p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2, nrow = 2, axis_titles = 'collect') +
  plot_annotation(title = "Normal Probability Plot")





