Sys.setlocale("LC_ALL", "C")

#################################### Video 3: A Line Plot ####################################
mvt <- read.csv('mvt.csv', stringsAsFactors=F)
str(mvt)
mvt$Date <- strptime(mvt$Date, format="%m/%d/%y %H:%M")
mvt$Weekday <- weekdays(mvt$Date)
mvt$Hour <- mvt$Date$hour
str(mvt)
weekdaycounts <- as.data.frame(table(mvt$Weekday))
str(weekdaycounts)

library(ggplot2)
ggplot(weekdaycounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))

#changing to chronological order
weekdaycounts$Var1 <- factor(weekdaycounts$Var1, ordered=T, levels=c('Sunday', 'Monday', 'Tuesday',
                                                                     'Wednesday', 'Thursday', 'Friday',
                                                                     'Saturday'))
ggplot(weekdaycounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) + xlab('Day of the Week') + ylab('Total Motar Vehicle Thefts')
ggplot(weekdaycounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), linetype=2) + xlab('Day of the Week') + ylab('Total Motar Vehicle Thefts')
ggplot(weekdaycounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), alpha=0.3) + xlab('Day of the Week') + ylab('Total Motar Vehicle Thefts')

######################################## Video 4: A Heatmap #########################################
table(mvt$Weekday, mvt$Hour)
dayhourcounts <- as.data.frame(table(mvt$Weekday, mvt$Hour))
str(dayhourcounts)
dayhourcounts$Hour <- as.numeric(as.character(dayhourcounts$Var2)) #converting a FACTOR variable to a numeric variable
ggplot(dayhourcounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1))
ggplot(dayhourcounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1), size=2)

#heatmap
dayhourcounts$Var1 <- factor(dayhourcounts$Var1, ordered=T, levels=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',
                                                                     'Saturday', 'Sunday'))
ggplot(dayhourcounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq))                             
#adding labels and colors
ggplot(dayhourcounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name='Total MV Thefts') +theme(axis.title.y=element_blank())
ggplot(dayhourcounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name='Total MV Thefts', low='white', high='red') + theme(axis.title.y=element_blank())

##################################### Video 5: A Geographical Hot Spot Map ##############################
install.packages('maps')
install.packages('ggmap')
library(maps)
library(ggmap)
chicago <- get_map(location= 'chicago', zoom=11)
ggmap(chicago)
ggmap(chicago) + geom_point(data=mvt[1:100,], aes(x=Longitude, y=Latitude))
LatLonCounts <- as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude, 2)))
str(LatLonCounts)
LatLonCounts$Long <- as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat <- as.numeric(as.character(LatLonCounts$Var2))
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long, y=Lat, color=Freq, size=Freq))
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long, y=Lat, color=Freq, size=Freq)) + scale_color_gradient(low='yellow', high='red')
ggmap(chicago) + geom_tile(data=LatLonCounts, aes(x=Long, y=Lat, alpha=Freq), fill='red')
str(mvt)

#quiz
LatLonCounts2 <- subset(LatLonCounts, Freq>0)
str(LatLonCounts2)
ggmap(chicago) + geom_tile(data=LatLonCounts2, aes(x=Long, y=Lat, alpha=Freq), fill='red')
1638-686

##################################### Video 6: A Heatmap on the United States ########################
murders <- read.csv('murders.csv')
str(murders)
head(murders)

statesMap <- map_data('state')
str(statesMap)
ggplot(statesMap, aes(x=long, y=lat, group=group)) + geom_polygon(fill='white', color='black')
murders$region <- tolower(murders$State)

#merging
murdermap <- merge(statesMap, murders, by='region')
str(murdermap)

#plot
ggplot(murdermap, aes(x=long, y=lat, group=group, fill=Murders)) + geom_polygon(color='black') + scale_fill_gradient(low='black', high='red', guide='legend')
 
ggplot(murdermap, aes(x=long, y=lat, group=group, fill=Population)) + geom_polygon(color='black') + scale_fill_gradient(low='black', high='red', guide='legend')

#more Murders when Population is more. So both population and Murders maps are similar. so we need to create MurderRate
murdermap$murderrate <- murdermap$Murders/murdermap$Population*100000
ggplot(murdermap, aes(x=long, y=lat, group=group, fill=murderrate)) + geom_polygon(color='black') + scale_fill_gradient(low='black', high='red', guide='legend')
#remove washington Dc as it is an outlier with >10 murderrate
ggplot(murdermap, aes(x=long, y=lat, group=group, fill=murderrate)) + geom_polygon(color='black') + scale_fill_gradient(low='black', high='red', guide='legend', limits=c(0,10))

ggplot(murdermap, aes(x=long, y=lat, group=group, fill=GunOwnership)) + geom_polygon(color='black') + scale_fill_gradient(low='black', high='red', guide='legend')
