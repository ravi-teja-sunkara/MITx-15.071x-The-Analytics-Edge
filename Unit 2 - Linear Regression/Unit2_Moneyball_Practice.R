Sys.setlocale("LC_ALL", "C")

# VIDEO 2

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

#video 3
str(moneyball)
runsreg <- lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(runsreg)

runsreg1 <- lm(RS ~ OBP + SLG, data=moneyball)
summary(runsreg1)

runsallowed <- lm(RA ~ OOBP + OSLG, data=moneyball)
summary(runsallowed)

oakland <- subset(moneyball, Team=='OAK')

#Video 5 - Quiz
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 <- c(94,88,95,88,93,94,98,97,93,94)
wins2013 <- c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank,wins2012)
cor(teamRank,wins2013)
