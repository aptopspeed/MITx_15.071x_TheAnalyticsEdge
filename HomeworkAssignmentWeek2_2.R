###### Reading Test Scores ######

#Problem 1.1
pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")
str(pisaTrain)

#Problem 1.2
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

#Problem 1.3
summary(pisaTrain)

#Problem 1.4
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTrain)
str(pisaTest)

#Problem 2.1 - 2.3
#by discuss variables on model

#Problem 3.1
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

LinReg = lm(readingScore ~ ., data = pisaTrain)
summary(LinReg)

#Problem 3.2
SSE <- sum(LinReg$residuals^2)
RMSE <- sqrt(SSE / nrow(pisaTrain))
RMSE
lmScore <- LinReg
#Problem 3.3
#The coefficient 29.54 on grade is the difference in reading score between two students who are identical other than having a difference in grade of 1. Because A and B have a difference in grade of 2, the model predicts that student A has a reading score that is 2*29.54 larger = 59.09

#Problem 3.4 - 3.5
summary(LinReg)

#Problem 4.1
predTest <- predict(lmScore, newdata=pisaTest)
summary(predTest)
#637.7-353.3 = 284.4

#Problem 4.2
sum((predTest-pisaTest$readingScore)^2)
RMSE <- sqrt(mean((predTest-pisaTest$readingScore)^2))
RMSE

#Problem 4.3
baseline <- mean(pisaTrain$readingScore)
baseline

SST <- sum((baseline-pisaTest$readingScore)^2)
SST

#Problem 4.4
R2 <- 1-(SSE/SST)
R2
