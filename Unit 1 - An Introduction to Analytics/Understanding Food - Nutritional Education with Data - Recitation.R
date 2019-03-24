Sys.setlocale("LC_ALL", "C")

getwd()

usda <- read.csv("USDA.csv")

str(usda)
summary(usda)

#Data Analysis
which.max(usda$Sodium) #index of the food with highest sodium level
names(usda)
usda$Description[265]
usda[265,]

Highsodium <- subset(usda, usda$Sodium > 10000)
nrow(Highsodium)
Highsodium$Description

#sodium content in CAVIAR
match('CAVIAR',usda$Description)
usda$Sodium[4154]
summary(usda$Sodium)
sd(usda$Sodium)
sd(usda$Sodium, na.rm=T)

#plots
plot(usda$Protein, usda$TotalFat)
plot(usda$Protein, usda$TotalFat, xlab='Protein in mg', ylab='TotalFat in mg', main='Protein vs Fat', col='red')
hist(usda$VitaminC, xlab='Vitamin C (mg)', main='Histogram of Vitamin C levels', xlim=c(0,100))
hist(usda$VitaminC, xlab='Vitamin C (mg)', main='Histogram of Vitamin C levels', xlim=c(0,100), breaks = 100)
hist(usda$VitaminC, xlab='Vitamin C (mg)', main='Histogram of Vitamin C levels', xlim=c(0,100), breaks = 2000)
boxplot(usda$Sugar, main='Boxplot of Sugar levels', ylab='Sugar(g)')

#adding variables
usda$Sodium[1] > mean(usda$Sodium, na.rm=T)
highsodium <- usda$Sodium > mean(usda$Sodium, na.rm=T)
str(highsodium)
highsodium <- as.numeric(usda$Sodium > mean(usda$Sodium, na.rm=T))
usda$highsodium <- highsodium
str(usda)

usda$highprotein <- as.numeric(usda$Protein > mean(usda$Protein,na.rm=T))
usda$highfat <- as.numeric(usda$TotalFat > mean(usda$TotalFat,na.rm=T))
usda$highcarbs <- as.numeric(usda$Carbohydrate > mean(usda$Carbohydrate,na.rm=T))
str(usda)

#summary tables
table(usda$highsodium)
table(usda$highsodium, usda$highfat)

tapply(usda$Iron, usda$highprotein, mean, na.rm=T)
tapply(usda$VitaminC, usda$highcarbs, max, na.rm=T)
tapply(usda$VitaminC, usda$highcarbs, summary, na.rm=T)
