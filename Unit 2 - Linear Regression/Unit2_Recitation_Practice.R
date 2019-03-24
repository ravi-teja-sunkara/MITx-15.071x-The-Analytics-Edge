Sys.setlocale("LC_ALL", "C")

#Video 1: The DATA
nba <- read.csv('NBA_train.csv')
str(nba)

#Video 2: Playoffs and Wins
playoff <- lm(Playoffs ~ W,nba)
summary(playoff)

table(nba$W, nba$Playoffs)

nba$ptsdiff <- nba$PTS - nba$oppPTS
plot(nba$ptsdiff, nba$W)
winsreg <- lm(W ~ ptsdiff, data=nba)
summary(winsreg)

# W=41 + (0.0326 * ptsdiff) -- wins equation
# for making it to playoffs, taking cutoff of 42 wins so the ptsdiff for 42 should be
#ptsdiff ~= 31

#Video 3: Points Scored
ptsreg <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data=nba)
summary(ptsreg)

sse <- sum((ptsreg$residuals)^2)
rmse <- sqrt(sse/nrow(nba))
rmse

#removing TOV as it has the highest P-value
ptsreg2 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data=nba)
summary(ptsreg2)
cor(nba$DRB, nba$BLK)

#removing DRB
ptsreg3 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data=nba)
summary(ptsreg3)

ptsreg4 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data=nba)
summary(ptsreg4)

sse4 <- sum((ptsreg4$residuals)^2)
rmse4 <- sqrt(sse4/nrow(nba))

#Video 4: Making Predictions
nba_test <- read.csv('NBA_test.csv')
pointspredict <- predict(ptsreg4, newdata = nba_test)

sse_test <- sum((pointspredict-nba_test$PTS)^2)
sst_test <- sum((nba_test$PTS - mean(nba$PTS))^2)
r2 = 1 - (sse_test/sst_test)
