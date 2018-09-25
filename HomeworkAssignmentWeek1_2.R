#####Import Data#####
IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
Boeing <- read.csv("BoeingStock.csv")

str(IBM)
str(GE)
str(CocaCola)
str(ProcterGamble)
str(Boeing)

#####Change to Date#####
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

####Summary Statistics####
#Problem 1.1 - 1.2
str(IBM)

#Problem 1.3 -1.4
summary(IBM)

#Problem 1.5
summary(GE)

#Problem 1.6
summary(CocaCola)

#Problem 1.7
summary(Boeing)

#Problem 1.8
sd(ProcterGamble$StockPrice)


####Visualizing Stock Dynamics####
#Problem 2.1
plot(CocaCola$Date, CocaCola$StockPrice)

#Problem 2.2 -2.3
plot(CocaCola$Date, CocaCola$StockPrice, 
     type = "l", #type "l" told "lines"
     col = "red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, #line function for add line on primary chart
      col = "blue")

abline(v=as.Date(c("1983-03-01")), lwd=3)
abline(v=as.Date(c("2000-03-01")), lwd=3)#add a verticle line to chart and told date lwd function is thickness of verticle line

####Visualizing Stock Dynamics 1995-2005####
#Problem 3.1 - 3.4
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432],
     type="l", 
     col="red", 
     ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], 
      type="l", 
      col="blue", 
      ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], 
      type="l", 
      col="green", 
      ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], 
      type="l", 
      col="purple", 
      ylim=c(0,210))
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], 
      type="l", 
      col="orange", 
      ylim=c(0,210))
abline(v=as.Date(c("2000-03-01")), lwd=3)
abline(v=as.Date(c("1997-10-01")), lwd=3)
abline(v=as.Date(c("2005-01-01")), lwd=3)


####Monthly Trends####
#Problem 4.1
months(IBM$Date)
mean(IBM$StockPrice)
tapply(IBM$StockPrice, months(IBM$Date), mean)

#Problem 4.2
months(GE$Date)
months(CocaCola$Date)
mean(GE$StockPrice)
mean(CocaCola$StockPrice)
tapply(GE$StockPrice, months(GE$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)

#Problem 4.3
tapply(IBM$StockPrice, months(IBM$Date), mean)
