Sys.setlocale("LC_ALL", "C")

who <- read.csv("WHO.csv")

str(who)
mean(who$Over60)
which.min(who$Over60)
who$Country[183]
which.max(who$LiteracyRate)
who$Country[44]
hist(who$CellularSubscribers)
boxplot(who$LifeExpectancy ~ who$Region)
boxplot(who$LifeExpectancy ~ who$Region, xlab='', ylab='LifeExpectancy', main='LifeExpectancy of Countries by Region')
table(who$Region)

tapply(who$Over60, who$Region, mean)
tapply(who$LiteracyRate, who$Region, min)
tapply(who$LiteracyRate, who$Region, min, na.rm=T)

tapply(who$ChildMortality, who$Region, mean)
