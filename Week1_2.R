USDA <- read.csv("USDA.csv")
USDA

str(USDA)
summary(USDA)

which.max(USDA$Sodium)
names(USDA) #view variable names

USDA$Description[265]

HighSodium <- subset(USDA, Sodium > 10000)
nrow(HighSodium)
HighSodium$Description

# find CAVIAR in description
match("CAVIAR", USDA$Description)

USDA$Sodium[4154]

#same as command above
USDA$Sodium[match("CAVIAR", USDA$Description)]

#find sd of Sodium
sd(USDA$Sodium)
sd(USDA$Sodium, na.rm = TRUE)

#Plot with R
plot(USDA$Protein, USDA$TotalFat)
plot(USDA$Protein, USDA$TotalFat, 
     xlab = "Protein",
     ylab = "Fat",
     main = "Protein vs Fat",
     col = "red")


#Adding Variables
HighSodium <- as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
HighProtein <- as.numeric(USDA$Protein > mean(USDA$Protein, na.rm = TRUE))
HighFat <- as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE))
HighCarbs <- as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = TRUE))
USDA$HighSodium <- HighSodium
USDA$HighProtein <- HighProtein
USDA$HighFat <- HighTotalFat
USDA$HighCarbs <- HighCarbs
