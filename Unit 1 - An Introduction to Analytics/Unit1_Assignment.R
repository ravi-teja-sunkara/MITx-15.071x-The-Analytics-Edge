Sys.setlocale("LC_ALL", "C")

mvt <- read.csv('mvtWeek1.csv')
str(mvt)
summary(mvt)

max(mvt$ID)
min(mvt$Beat)
table(mvt$Arrest)
summary(mvt$Arrest)

#dates
mvt$Date[1]
dateconvert <- as.Date(strptime(mvt$Date,"%m/%d/%y %H:%M"))
summary(dateconvert)

mvt$month <- months(dateconvert)
mvt$weekday <- weekdays(dateconvert)
mvt$Date <- dateconvert
table(mvt$month)
table(mvt$weekday)
table(mvt$Arrest, mvt$month)


#Visualizing Crime Trends
hist(mvt$Date, breaks=100, xlab='Date', ylab='Frequency', main='Crimes vs Date')
boxplot(mvt$Date~mvt$Arrest)


mvt01 <- subset(mvt, mvt$Date>='2001-01-31' & mvt$Date<='2001-12-31')
table(mvt01$Arrest)

table(mvt$Arrest, mvt$Year)

sort(table(mvt$LocationDescription))
top5 <- subset(mvt, mvt$LocationDescription=='STREET' | mvt$LocationDescription=='PARKING LOT/GARAGE(NON.RESID.)'
               | mvt$LocationDescription=='ALLEY' | mvt$LocationDescription=='GAS STATION' |
                 mvt$LocationDescription=='DRIVEWAY - RESIDENTIAL')

table(top5$LocationDescription)
top5$LocationDescription=factor(top5$LocationDescription)
str(top5$LocationDescription)
table(top5$Arrest,top5$LocationDescription)
table(top5$weekday,top5$LocationDescription=='GAS STATION')
table(top5$weekday,top5$LocationDescription)
