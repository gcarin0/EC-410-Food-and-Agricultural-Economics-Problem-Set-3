#Giancarlo Carino
#EC 410
#Problem Set 3

#Install packages
install.packages(pkgs=c("psych", "stargazer", "lmtest", "car", "dplyr","readxl"))
library(psych)
library(stargazer)
library(lmtest)
library(car)
library(dplyr)
library(readxl)

#Import
setwd("/Users/gc/Desktop/GitHub/EC-410-Food-and-Agricultural-Economics/Problem Set 3")
pset3 <- read_excel("hw3_data.xlsx")
attach(pset3)

#Remove missing rows
adjpset3 <- pset3[complete.cases(pset3), ]

#Regression (DV: BMI, INDV: income, employed)
reg <- lm(bmxbmi ~ income + employed, data = adjpset3)
summary(reg)

#Test for Heteroscedasticity
#Create residuals

adjpset3$residuals <- resid(reg)

#Plot Residuals vs INDV: income, employed
plot(adjpset3$income, adjpset3$residuals, ylab = "Residuals", xlab = "Income", main = "Residuals vs Income")
plot(adjpset3$employed, adjpset3$residuals, ylab = "Residuals", xlab = "Employment", main = "Residuals vs Employment")

bptest(reg)

#Test for multicollinearity
vif(reg)

#Create interaction variable
adjpset3$fempov <- adjpset3$female*adjpset3$povertycat1

#Regression w/ interaction
regint <- lm(bmxbmi ~ income + employed + fempov, data = adjpset3)
summary(regint) 
