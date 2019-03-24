Sys.setlocale("LC_ALL", "C")

cps <- read.csv('CPSData.csv')
str(cps)

#Summarizing Data
sort(table(cps$Industry))
sort(table(cps$State))

country <- read.csv('CountryCodes.csv')

table(cps$Citizenship)

table(cps$Hispanic, cps$Race)
table(cps$PeopleInHousehold)
summary(cps)

#Problem 2.2 - Evaluating Missing Values
is.na(cps$Married)
table(cps$Region, is.na(cps$Married))
table(cps$Sex, is.na(cps$Married))
table(cps$Age, is.na(cps$Married))
table(cps$Citizenship, is.na(cps$Married))

table(is.na(cps$MetroAreaCode), cps$State)
table(cps$Region,is.na(cps$MetroAreaCode))

sort(tapply(is.na(cps$MetroAreaCode), cps$State, mean))


#Problem 3.1 - Integrating Metropolitan Area Data
metroareamap <- read.csv('MetroAreaCodes.csv')
str(metroareamap)
str(country)

cps <- merge(cps, metroareamap, by.x='MetroAreaCode', by.y='Code', all.x=T)
str(cps)
table(is.na(cps$MetroArea))

sort(table(cps$MetroArea))
sort(tapply(cps$Hispanic, cps$MetroArea, mean))

sort(tapply(cps$Race=='Asian', cps$MetroArea, mean))

sort(tapply(cps$Education == "No high school diploma", cps$MetroArea, mean, na.rm=T))

#Problem 4.1 - Integrating Country of Birth Data
str(country)
str(cps)
cps <- merge(cps,country, by.x='CountryOfBirthCode', by.y='Code', all.x=T)
str(cps)
summary(cps)

sort(table(cps$Country))

table(cps$Country=='United States',cps$MetroArea=='New York-Northern New Jersey-Long Island, NY-NJ-PA')

table(cps$Country=='India', cps$MetroArea)
tapply(cps$Country=='Brazil', cps$MetroArea, summary, na.rm=T)
sort(tapply(cps$Country=='Somalia', cps$MetroArea, sum, na.rm=TRUE))
