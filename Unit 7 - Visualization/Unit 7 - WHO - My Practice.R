Sys.setlocale("LC_ALL", "C")
######################### Video 4: Basic Scatterplots Using ggplot ################
who <- read.csv('WHO.csv')
str(who)

plot(who$GNI, who$FertilityRate) #BASE package

install.packages('ggplot2')
library(ggplot2)

scatterplot <- ggplot(who, aes(x=GNI, y=FertilityRate))
scatterplot + geom_point()
scatterplot + geom_line() #line doesn't make sense
scatterplot + geom_point(color='blue', size=3, shape=17) #adding color, size and shapes
scatterplot + geom_point(color='darkred', size=3, shape=8)
scatterplot + geom_point(color='darkred', size=3, shape=8) + ggtitle('Fertility Rate vs GNI')
#saving the plot to a variable
fertilityGNIplot <- scatterplot + geom_point(color='darkred', size=3, shape=8) + ggtitle('Fertility Rate vs GNI')
#saving to a file
pdf('Myplot.pdf')
print(fertilityGNIplot)
dev.off()

########################### Video 5: Advanced Scatterplots Using ggplot ######################
ggplot(who, aes(x=GNI, y=FertilityRate, color=Region)) + geom_point() #adding color according to REGION, here REGION is added in the AES part because it is treated as a variable and color in geom_point is for shape of points
ggplot(who, aes(x=GNI, y=FertilityRate, color=LifeExpectancy)) + geom_point() #color by LifeExp which is continous numeric variable so a gradient of colors

ggplot(who, aes(x=FertilityRate, y=Under15)) + geom_point()
ggplot(who, aes(x=log(FertilityRate), y=Under15)) + geom_point() #log as from above graph the relationship didn't seem linear
model <- lm(Under15 ~ log(FertilityRate), data=who)
summary(model)
ggplot(who, aes(x=log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method='lm') #adding REGRESSION line
#by default ggplot draws a 95% confidence interval shade across the line
ggplot(who, aes(x=log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method='lm', level=.99)#99% interval
#completely removing shade/confidence interval
ggplot(who, aes(x=log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method='lm', se=FALSE, color='orange')

ggplot(who, aes(x=FertilityRate, y=Under15, color=Region)) + geom_point()
