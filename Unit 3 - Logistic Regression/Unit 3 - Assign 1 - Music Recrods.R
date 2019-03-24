Sys.setlocale("LC_ALL", "C")

songs <- read.csv('songs.csv')
str(songs)
songs2010 <- subset(songs, year==2010)
table(songs$artistname)
mj <- subset(songs, artistname=='Michael Jackson' & songs$Top10 == 1)
table(songs$timesignature)

#Out of all of the songs in our dataset, the song with the highest tempo is one of the following songs. Which one is it?
max(songs$tempo)
songs$songtitle[songs$tempo==244.307]

which.max(songs$tempo)
songs$songtitle[6206]

# Creating Our Prediction Model
#splitting
train <- subset(songs, year<=2009)
test <- subset(songs, year==2010)

#we want to exclude some of the variables in our dataset from being used as independent variables ("year", "songtitle", "artistname", "songID", and "artistID").
nonvars <- c("year", "songtitle", "artistname", "songID", "artistID")
train <- train[,!(names(train)%in%nonvars)]
test <- test[,!(names(test)%in%nonvars)]

mod1 <- glm(Top10 ~ ., data=train, family=binomial)
summary(mod1)
cor(train$loudness, train$energy)

mod2 <- glm(Top10 ~ .-loudness, data=train, family=binomial)
summary(mod2)

mod3 <- glm(Top10 ~ .-energy, data=train, family=binomial)
summary(mod3)

#predicting on test dataset
predtest <- predict(mod3, type='response', newdata = test)
table(test$Top10, predtest>0.45)
###################
#   FALSE   TRUE  #
#   0   309    5  #
#   1    40   19  #
###################
(309+19)/(309+19+5+40) #accuracy
(19)/(40+19) #sensitivity
309/(309+5) #specificity

#baseline model
table(train$Top10) # Not Top10 is more frequent so considering it as baseline
(309+5)/(309+19+5+40)
