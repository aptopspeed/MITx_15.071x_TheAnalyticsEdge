mvtWeek1 <- read.csv("mvtWeek1.csv")

#####Loading Data#####
#Problem 1.1 - 1.2
str(mvtWeek1)

#Problem 1.3
mvtWeek1[which.max(mvtWeek1$ID)]

#Problem 1.4
mvtWeek1$Beat[which.min(mvtWeek1$Beat)]

#Problem 1.5
TrueArrest <- as.numeric(mvtWeek1$Arrest)
table(TrueArrest)

#Problem 1.6
tapply(mvtWeek1$LocationDescription)


#####Understanding Dates in R#####
#Problem 2.1
mvt <- mvtWeek1
DateConvert <- as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))


mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)

mvt$Date = DateConvert

#Problem 2.2
summary(DateConvert)

#Problem 2.3
table(mvt$M) #Look for smallest number on a month

#Problem 2.4
table(mvt$Weekday) #Look for largest number on weekday

#Problem 2.5
table(mvt$Arrest, mvt$Month)

#####Visualizing Crime Trends#####
#Problem 3.1
hist(mvt$Date, breaks=100)

jpeg(filename="name.jpg") # save a plot file as jpg

#Problem 3.2
boxplot(mvt$Date ~ mvt$Arrest) # First half

#Problem 3.3
table(mvt$Arrest , mvt$Year) #proportion of motor vehicle thefts in 2001 was an arrest made is 2152/(18517+2152)

#Problem 3.4
#proportion of motor vehicle thefts in 2007 was an arrest made is 1212/(13058+1212)

#Problem 3.5
#proportion of motor vehicle thefts in 2007 was an arrest made is 550/(13542+550)

#####Popular Locations#####
#Problem 4.1
sort(table(mvt$LocationDescription)) #Top 5 of theft occured

#Problem 4.2
Top5 <- subset(mvt, LocationDescription == "STREET" |
                   LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" |
                   LocationDescription == "ALLEY" |
                   LocationDescription == "GAS STATION" |
                   LocationDescription == "DRIVEWAY - RESIDENTIAL")



#Problem 4.3
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription, Top5$Arrest)


#Problem 4.4 -4.5
table(Top5$LocationDescription, Top5$Weekday)
