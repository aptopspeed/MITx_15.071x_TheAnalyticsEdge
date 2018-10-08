###### Homework Assignment Week 2 ######

####Creating Our First Model####

#Problem 1.1 - 1.2
climate <- read.csv("climate_change.csv")
trainset <- subset(climate, Year <= 2006)
testset <- subset(climate, Year > 2006)

climatelm <- lm(Temp ~ MEI + CO2 +CH4 + N2O + CFC.12 + CFC.11 + TSI +Aerosols, data=trainset)

summary(climatelm)

####Understanding The Model####

#Problem 2.1 - 2.2
cor(trainset)

####Simplifying The Model####

#Problem 3
lmReg <- lm(Temp ~ MEI + TSI + Aerosols + N2O, data = trainset)
summary(lmReg)
#the model R is 0.7261 compared to 0.7509 previously

####Automatically Building The Model####

#Problem 4
StepModel <- step(climatelm)
summary(StepModel)

####Testing On Unseen Data####

#Problem 5
tempPredict <- predict(StepModel, newdata = testset)

SSE <- sum((tempPredict - testset$Temp)^2)
SSE
SST <- sum((mean(trainset$Temp) - testset$Temp)^2)
SST
R2 <- 1-SSE/SST
R2
