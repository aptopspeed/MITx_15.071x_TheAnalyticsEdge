##### VIDEO 4 ####

# Read in data
wine = read.csv("wine.csv")
str(wine)
summary(wine)

# Linear Regression (one variable)
model1 = lm(Price ~ AGST, data=wine)
summary(model1)

# Sum of Squared Errors
model1$residuals
SSE = sum(model1$residuals^2)
SSE

# Linear Regression (two variables)
model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)

# Sum of Squared Errors
SSE = sum(model2$residuals^2)
SSE

# Linear Regression (all variables)
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)

# Sum of Squared Errors
SSE = sum(model3$residuals^2)
SSE

####Quick Question####
modelQ1 = lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(modelQ1)

#Multiple R-squared = 0.3177
#Coefficients of HarvestRain is Estimate HarvestRain = -4.971e-03
#Intercept Coefficients is Estimate(Intercept) = 7.865e+00


##### VIDEO 5 ####

# Remove FrancePop
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)
# You can see *** behind it's mean significant on the variable

#### VIDEO 6 ####

# Correlations
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)

# Remove Age and FrancePop
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)

#Typically, a correlation greater than 0.7 or less than -0.7 is cause for concern.

####Quick Question####
cor(wine$HarvestRain, wine$WinterRain)


#### VIDEO 7 ####

# Read in test set
wineTest = read.csv("wine_test.csv")
str(wineTest)

# Make test set predictions
predictTest = predict(model4, newdata=wineTest)
predictTest


# Compute R-squared
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST

#### Qiuck Question ####
#Ans: 2.4


