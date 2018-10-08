#### VIDEO 2 ####

# Read in data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

#we need to win more 95 games -> W > 95
#W = Estimate intercept + EstimateRD*(RD)
#95 = 80.8814 + 0.1058(RD)
#RD = 133.4 or more
#### Quick Question ####
Qrd <- 713-614
Qrd
nWin <- 80.881374+(0.105766*(Qrd))
nWin


#### VIDEO 3 ####

str(moneyball)

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

#### Quick Question ####
-804.63+(2737.77*0.311)+(1584.91*0.405)
-837.38+(2913.60*0.297)+(1514.29*0.37)


#### Quick Question ####
teamRank <- c(1,2,3,3,4,4,4,4,5,5)
wins2012 <- c(94,88,95,88,93,94,98,97,93,94)
wins2013 <- c(97,97,92,93,92,96,94,96,92,90)

cor(teamRank,wins2012)
cor(teamRank,wins2013)