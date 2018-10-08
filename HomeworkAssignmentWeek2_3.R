###### Detecting Flu Epidemics via Search Engine Query Data ######

FluTrain <- read.csv("FluTrain.csv")

#Problem 1.1
AA <- subset(FluTrain, ILI == max(ILI))
AA
which.max(FluTrain$ILI)
FluTrain$Week[303]

#Problem 1.2
BB <- hist(FluTrain$ILI)

#Problem 1.3
CC <- plot(FluTrain$Queries, log(FluTrain$ILI))

#Problem 2.1 -2.2
FluTrend1 <- lm(log(ILI)~Queries, data=FluTrain)
summary(FluTrend1)

#Problem 2.3
Correlation <- cor(FluTrain$Queries, log(FluTrain$ILI))
Correlation
log(1/Correlation)
exp(-0.5*Correlation)
DD <- Correlation^2
#DD is R-Squared

#Problem 3.1
FluTest <- read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[11]

#Problem 3.2
#From previous problem we know PredTest[11] is 2.187378
#We need to know FluTest actual value testing set on [11]
#and calculate by (PredTest1[11] - FluTest$ILI[11])/FluTest$ILI[11]
FluTest$ILI[11]
(PredTest1[11] - FluTest$ILI[11])/FluTest$ILI[11]
#Not to - on relative error

#Problem 3.3
SSE <- sum((PredTest1-FluTest$ILI)^2)
RMSE <- sqrt(SSE / nrow(FluTest))
sqrt(mean((PredTest1-FluTest$ILI)^2))


#### Training Time Series Model ####

#Problem 4.1
install.packages("zoo")

library(zoo)

ILILag2 <- lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)

FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain$ILILag2)

#Problem 4.2
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

#Problem 4.3
FluTrend2 <- lm(log(ILI)~Queries+log(ILILag2), data=FluTrain)
summary(FluTrend2)

#Problem 4.4
#Moving from FluTrend1 to FluTrend2, in-sample R^2 improved from 0.709 to 0.9063, and the new variable is highly significant. As a result, there is no sign of overfitting, and FluTrend2 is superior to FluTrend1 on the training set

#Problem 5.1
ILILag2 <- lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 <- coredata(ILILag2)
summary(FluTest$ILILag2)

#Problem 5.2
#The time two weeks before the first week of 2012 is the second-to-last week of 2011. This corresponds to the second to last observation in FluTrain.
#The time two weeks before the second week of 2012 is the last week of 2011. This corresponds to the last observation in FluTrain.

#Problem 5.3
FluTest$ILILag2[1] <- FluTrain$ILI[416]
FluTest$ILILag2[2] <- FluTrain$ILI[417]
FluTest$ILILag2[1]
FluTest$ILILag2[2]

#Problem 5.4
PredTest2 <- exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((PredTest2-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))
RMSE
#Alternative, can use
sqrt(mean((PredTest2-FluTest$ILI)^2))

#Problem 5.5
#Compare Model by RMSE