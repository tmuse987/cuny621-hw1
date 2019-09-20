library(ggplot2)
library(tidyr)

setwd("l:/school/cuny/621/hw1")

mbTrain <- read.csv("moneyball-training-data.csv")

#find counts of na's
sapply(mbTrain, function(x) sum(is.na(x)))
#hbp and cs (hit by pitch and caught stealing) have very large numbers of missing values, probably shouldn't use, 
#and knowing baseball, probably minor impact on wins regardless

#remove those two columns, plus the index colume which we don't need
mbTrain <- mbTrain[, -which(names(mbTrain) %in% c("INDEX", "TEAM_BATTING_HBP", "TEAM_BASERUN_CS"))]

#convert to -1 for now so we can run test on zero's
mbTrain[is.na(mbTrain)] <- -1

#lets also check for zeros as zero's for any of these stats clearly is inaccurate and eqivalent to an NA

sapply(mbTrain, function(x) sum(x == 0))

#not so bad, but lets not replace them until we check for outliers and convert NA's back to -1

#running summary we can see that there are some really big outliers (e.g., 19278 Strikeouts in one year?, that would be 119 per game....)
#at initial glance (can always revisit), looks like Pitching_Hits, Pitching_BB, Pitching_SO and Fielding Errors all have outliers that 
#are not realistic values
summary(mbTrain)

#looking at top values we can see that there maybe a number of years that are unrealistic
head(sort(mbTrain$TEAM_PITCHING_H, decreasing = TRUE))
head(sort(mbTrain$TEAM_PITCHING_BB, decreasing = TRUE))
head(sort(mbTrain$TEAM_PITCHING_SO, decreasing = TRUE))
head(sort(mbTrain$TEAM_FIELDING_E, decreasing = TRUE))

#lets plot to see where outliers are
plot(mbTrain$TEAM_PITCHING_H)
plot(mbTrain$TEAM_PITCHING_BB)
plot(mbTrain$TEAM_PITCHING_SO)
plot(mbTrain$TEAM_FIELDING_E)

#there are a lot of rows with pitching hits greater than 3000 (there was max of 2554 hits by batting team, 
#so a limit of 3000 hits (18.5 avg per game) by a pitching team doesn't seem so unreasonable as max)
#We see 86, these should probably be tossed and replaced with median (mean?)
sum(mbTrain$TEAM_PITCHING_H > 3000)

#using same idea for base on balls, max for batting was 878, so perhaps a 1200 max for pitching (7.5 avg a game) might be reasonable
#10 rows here, as before toss and replace with median
sum(mbTrain$TEAM_PITCHING_BB > 1200)

#for strikeouts, batting max of 1399, so perhaps 1800 for pitching max
#9 rows to replace
sum(mbTrain$TEAM_PITCHING_SO > 1800)

#for fielding errors, no corresponding number to validate against, but if we use a plausible limit of about 10 per game (still really high)
# we can exclude for that are clearly outliers
sum(mbTrain$TEAM_FIELDING_E > 1600)

#let's replace all these values

mbTrain$TEAM_PITCHING_H[mbTrain$TEAM_PITCHING_H >3000] <- 0
mbTrain$TEAM_PITCHING_BB[mbTrain$TEAM_PITCHING_BB >1200] <- 0
mbTrain$TEAM_PITCHING_SO[mbTrain$TEAM_PITCHING_SO >1800] <- 0
mbTrain$TEAM_FIELDING_E[mbTrain$TEAM_FIELDING_E >1600] <- 0




#replace all zeros plus the -1's for the na's

for(i in 2:ncol(mbTrain))
{
    idx <- mbTrain[i]<=0
    mbTrain[i][idx] <- median(mbTrain[[i]])
    
}


#do some correlation, note that team pitching stats don't seem to correlate correctly??
cor(mbTrain$TARGET_WINS, mbTrain[-c(1)])



attach(mbTrain)
plot(TEAM_BATTING_H, TARGET_WINS)

ggplot(mbTrain, aes(y=TEAM_BATTING_H, x= 1)) + geom_boxplot()
boxplot(TARGET_WINS)
#note appears reasonably normally distributed
ggplot() + geom_histogram(aes(x = TARGET_WINS))
ggplot() + geom_histogram(aes(x = TEAM_BATTING_H))


ggplot(gather(mbTrain), aes(value)) +
    geom_histogram(bins = 20) +
    facet_wrap(~key, scales = "free_x")



lmFit <- lm(TARGET_WINS ~., data = mbTrain)
lmFit <- lm(TARGET_WINS ~ TEAM_BATTING_HR, data = mbTrain)

summary(lmFit)


plot(TEAM_BATTING_H, TARGET_WINS)


lmFitH <- lm(TARGET_WINS ~ TEAM_BATTING_H)
ggplot(mbTrain, aes(x= TEAM_BATTING_H, y = TARGET_WINS)) + 
    geom_point() + 
    geom_smooth(method = "lm", se= FALSE)






#vs index
ggplot(lmFitH, aes(x= seq(1:nrow(mbTrain)), y= .resid)) +geom_point()
#vs fitted value
ggplot(lmFitH, aes(x= .fitted, y= .resid)) +geom_point()




