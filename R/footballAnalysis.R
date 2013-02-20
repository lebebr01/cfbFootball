setwd("/home/lobo/Desktop/football/Data/Final")

cfb <-read.csv("cfbMerge-2012-03-09.csv")

##Scoring was different before the year 1912
cfbSub <- subset(cfb, Year >= 1912)

library(plyr)
splitvars <- c("school", "Coach", "Year")
winlosPct <- ddply(cfbSub, .variables = splitvars, .fun = summarise,
                   meanPct = mean(Pct., na.rm=TRUE), avgDelta = mean(Delta, na.rm=TRUE),
                   maxWin = max(Win, na.rm=TRUE), maxLoss = max(Loss, na.rm=TRUE),
                   numBowl = mean(PF.Bowl, na.rm=TRUE))

winlosPct$maxWin <- ifelse(winlosPct$maxWin == "-Inf", NA, winlosPct$maxWin)
winlosPct$maxLoss <- ifelse(winlosPct$maxLoss == "-Inf", NA, winlosPct$maxLoss)

winlosPct1 <- ddply(winlosPct, .variables = c("school", "Coach"), .fun = summarise,
                    meanPct = mean(meanPct, na.rm=TRUE), Year = max(Year,na.rm=TRUE) - min(Year,na.rm=TRUE),
                    lYear = max(Year,na.rm=TRUE), fYear = min(Year, na.rm=TRUE),
                    meanWin = mean(maxWin, na.rm=TRUE), meanLoss = mean(maxLoss, na.rm=TRUE))

bowls <- dply(cfbSub, .variables = c("school", "Coach","Year"))