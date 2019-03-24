Sys.setlocale("LC_ALL", "C")

ibm <- read.csv('IBMStock.csv')
ge <- read.csv('GEStock.csv')
pg <- read.csv('ProcterGambleStock.csv')
cc <- read.csv('CocaColaStock.csv')
boeing <- read.csv('BoeingStock.csv')
str(ibm)

#Date Conversions
ibm$Date <- as.Date(ibm$Date,'%m/%d/%y')
ge$Date <- as.Date(ge$Date,'%m/%d/%y')
pg$Date <- as.Date(pg$Date,'%m/%d/%y')
cc$Date <- as.Date(cc$Date,'%m/%d/%y')
boeing$Date <- as.Date(boeing$Date,'%m/%d/%y')

summary(ibm)
summary(ge)
summary(cc)
summary(boeing)

sd(pg$StockPrice,na.rm=T)


#Visualizing
plot(cc$Date, cc$StockPrice, type='l',col='red')
lines(pg$Date, pg$StockPrice, col='blue')
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1983-01-01")), lwd=1, col='green')
abline(v=as.Date(c("1984-01-01")), lwd=1, col='cyan', lty=2)

#Problem 3.1 - Visualizing Stock Dynamics 1995-2005
plot(cc$Date[301:432], cc$StockPrice[301:432], type='l', col='chocolate', ylim=c(0,210))
lines(ibm$Date[301:432], ibm$StockPrice[301:432], col='indianred2')
lines(ge$Date[301:432], ge$StockPrice[301:432], col='green')
lines(pg$Date[301:432], pg$StockPrice[301:432], col='purple')
lines(boeing$Date[301:432], boeing$StockPrice[301:432], col='black')
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1997-09-01")), lwd=2, col='orange',lty=2)
abline(v=as.Date(c("1997-11-01")), lwd=2, col='yellow4',lty=4)

#monthly trends
tapply(ibm$StockPrice, months(ibm$Date), mean)
mean(ibm$StockPrice)

tapply(ge$StockPrice, months(ge$Date), mean)
tapply(cc$StockPrice, months(cc$Date), mean)
