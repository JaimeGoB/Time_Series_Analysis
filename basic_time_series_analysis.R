# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)


###################################
# Importing co2 time series dataset
###################################
co2_data <- co2

###################################
# Viewing information regardint the dataset
# Atmospheric concentrations of CO2 are expressed in parts per million (ppm)
# From 59 ~ 97
###################################
help(co2)

###################################
# Plotting TS data
###################################
# Creating a TS Linear Model
par(mfrow=c(1,1))
co2.linear.model = lm(co2_data ~ time(co2_data) )
plot(co2, ylab = expression(CO[2] ~ (ppm)), main = "Mauna Loa Carbon Dioxide")
abline(co2.linear.model, col = "red")
legend("bottomright", legend = "Line of best fit",
       col= "red", lty=1) #lty 1 is full line

###################################
# Residual Analysis
###################################
par(mfrow=c(2,2))
# fitting residuals for data
c02.residuals = resid( co2.linear.model )
# Residuals seem to follow a normal distribution
hist(c02.residuals, main= "Histogram of CO2 Residuals", col = "red") 
# quantile plot - there is systematic deviation from 
# normality in the tails of the qqplot
# residuals are not exactly normaly distributed.
qqnorm(c02.residuals, main= "Normal Probability Plot") 
qqline(c02.residuals, col = "red")
# time plot of residuals - obvious structure in the time plot 
# violating assumptions
plot(c02.residuals ~ time(co2_data), main="Residuals on Time",pch = 20, col = "blue")
plot(c02.residuals ~ time(co2_data), xlim=c(1960, 1962), main="Zoomed in Residuals on Time - Two Years",pch = 20, col = "blue")
par(mfrow=c(1,1))




