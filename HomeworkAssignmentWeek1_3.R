####Demographics and Employment in the United States####
#Import Data
CPS <- read.csv("CPSData.csv")

#Problem 1.1
str(CPS)

#Problem 1.2
table(CPS$Industry)

CPS %>% count(Industry, sort = TRUE) #dplyr packeage style

#Problem 1.3
sort(table(CPS$State))


#Problem 1.4
table(CPS$Citizenship) #(Native+NonCitizen)/Observations

#Problem 1.5
table(CPS$Race, CPS$Hispanic) #in 1 more than 250 interviewees

####Evaluating Missing Values####
#Problem 2.1
summary(CPS)

#Problem 2.2
is.na(CPS$Married)
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

#Problem 2.3
table(CPS$State,is.na(CPS$MetroAreaCode))

#Problem 2.4
table(CPS$Region, is.na(CPS$MetroAreaCode))

#Problem 2.5
tapply(is.na(CPS$MetroAreaCode), CPS$State, mean) #find a proportion on table

####Integrating Metropolitan Area Data####
MetroAreaMap <- read.csv("MetroAreaCodes.csv")
CountryMap <- read.csv("CountryCodes.csv")

#Problem 3.1
str(MetroAreaMap)
str(CountryMap)

#Problem 3.2
CPS <- merge(CPS, MetroAreaMap, 
             by.x="MetroAreaCode",
             by.y="Code", all.x=TRUE)
summary(CPS)

#Problem 3.3
table(CPS$MetroArea)

CPS %>%
    count(MetroArea, sort = TRUE) #dplyr package style

#Problem 3.4
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

#Problem 3.5
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))

#Problem 3.6
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean))

####Integrating Country of Birth Data####
#Problem 4.1
CPS <- merge(CPS, CountryMap,
             by.x = "CountryOfBirthCode",
             by.y = "Code", all.x =TRUE)
names(CPS)
summary(CPS)

#Problem 4.2
sort(table(CPS$Country))

#Problem 4.3
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA",CPS$Country != "United States")
#proportion TRUE isn't the US metropolitan

#Problem 4.4
sort(tapply(CPS$Country == "India", 
            CPS$MetroArea,
            sum, na.rm =TRUE))
sort(tapply(CPS$Country == "Brazil",
            CPS$MetroArea,
            sum, na.rm =TRUE))
sort(tapply(CPS$Country == "Somalia",
            CPS$MetroArea,
            sum, na.rm =TRUE))




