Sys.setlocale("LC_ALL", "C")

######################################## Video 3: Bar Charts in R #######################
intl <- read.csv('intl.csv')
library(ggplot2)
str(intl)

ggplot(intl, aes(x=Region, y=PercentOfIntl)) + geom_bar(stat='identity') + geom_text(aes(label=PercentOfIntl))

#ordering Region
intl <- transform(intl, Region=reorder(Region, -PercentOfIntl))
str(intl)
intl$PercentOfIntl <- intl$PercentOfIntl * 100
ggplot(intl, aes(x=Region, y=PercentOfIntl)) + geom_bar(stat='identity', fill='dark blue') + geom_text(aes(label=PercentOfIntl), vjust=-0.4) + ylab('Percent of Intl Students') + theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=45, hjust=1))

#################################### Video 5: World Maps in R ############################
library(ggmap)
intlall <- read.csv('intlall.csv', stringsAsFactors=F)
head(intlall)
intlall[is.na(intlall)] <- 0

world_map <- map_data('world')
str(world_map)

world_map <- merge(world_map, intlall, by.x='region', by.y='Citizenship')
str(world_map)

ggplot(world_map, aes(x=long, y=lat, group=group)) +geom_polygon(fill='white', color='black') + coord_map('mercator', xlim=c(-180, 180), ylim=c(-60, 90))
world_map <- world_map[order(world_map$group, world_map$order),]
ggplot(world_map, aes(x=long, y=lat, group=group)) +geom_polygon(aes(fill=Total), color='black') + coord_map('mercator', xlim=c(-180, 180), ylim=c(-60, 90))
table(intlall$Citizenship)

#changing china's name
intlall$Citizenship[intlall$Citizenship=="China (People's Republic Of)"] <- 'China'
world_map <- merge(map_data('world'), intlall, by.x='region', by.y='Citizenship')
world_map <- world_map[order(world_map$group, world_map$order),]
ggplot(world_map, aes(x=long, y=lat, group=group)) +geom_polygon(aes(fill=Total), color='black') + coord_map('mercator', xlim=c(-180, 180), ylim=c(-60, 90))

#ortho
ggplot(world_map, aes(x=long, y=lat, group=group)) +geom_polygon(aes(fill=Total), color='black') + coord_map('ortho', orientation=c(-37,175,0), xlim=c(-180, 180), ylim=c(-60, 90))

######################################## Video 7: Using Line Charts Instead ############################
install.packages('reshape2')
library(reshape2)
library(ggplot2)
households <- read.csv('households.csv')
str(households)
households[,1:2]
head(melt(households, id='Year'))
melt(households, id='Year')[1:10, ]
ggplot(melt(households, id='Year'), aes(x=Year, y=value, color=variable)) + geom_line(size=2) + geom_point(size=5) + ylab('Percentage of Households')
