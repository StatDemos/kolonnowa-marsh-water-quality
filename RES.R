###################### Residuals Analysis ###########################

library(broom)

model.PO4_fitresid <- augment(model.BOD2)

# residual vs. fitted
ggplot(model.PO4_fitresid, aes(x = .fitted, y = .std.resid)) + geom_point() + 
  geom_hline(yintercept = 0, color = "red", size = 1) + 
  labs(title = "Residual plot against the fitted values", x = "Fitted values", 
       y = "Residuals")

# histogram of residuals
ggplot(model.PO4_fitresid, aes(x = .std.resid)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
  geom_density(alpha = 0.5, fill = "purple") + ggtitle("Histogram of residuals") +
  xlab("Residuals") + ylab("Density")

# QQ plot
ggplot(model.PO4_fitresid, aes(sample = .std.resid)) + stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Normal probability plot of residuals", x = "Expected", 
       y = "Residuals")

# shapiro wilk
shapiro.test(model.PO4_fitresid$.std.resid)


# residual autocorrelation
acf(model.BOD$residuals, type = "correlation")
pacf(model.BOD$residuals)

library(lmtest)
bgtest(model.BOD, data = data.BOD, order = 1)

library(car)
durbinWatsonTest(model.BOD)

# constant variance
res <- bptest(model.BOD2)
res

# nonpara for BOD - kruskal
kruskal.test(Mean.Value ~ `Vegetation type`, data = data.BOD) 



data("iris")
cbind(iris$Sepal.Length, iris$Sepal.Width)


model <- manova(iris[1:2] ~ Species, data = iris)



