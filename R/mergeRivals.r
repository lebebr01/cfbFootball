library(data.table)
library(plyr)

## load master file
load("/home/lobo/Dropbox/cfbFootball/Data/Final/cfbDataFinal-Feb2013.Rdata")

## past rosters file
roster <- read.table("/home/lobo/Dropbox/cfbFootball/Data/Final/pastroster.txt",
                     header = TRUE)
## changing team to rivalsName
setnames(roster, "team", "CBSsportsName")

## removing A with carat
roster$class <- as.character(roster$class)
roster$class <- iconv(roster$class, to = "ASCII//TRANSLIT")
roster$class <- ifelse(roster$class == "A ", NA, roster$class)

## converting position to character
roster$pos <- as.character(roster$pos)

## converting roster to data.table
roster <- data.table(roster, key = "CBSsportsName")

## removing hometown
roster <- subset(roster, select = - hometown)

## adding year variable to help with the merging
roster$Year <- 2012

## load teamNames file
teamNames <- read.csv2("/home/lobo/Dropbox/cfbFootball/Data/Final/cfbDataTeam.csv")
setnames(teamNames, "rivalsName", "CBSsportsName")

## join rivalsName and roster
cfbData <- join(cfbData, teamNames, by = "offName")
cfbData <- join(cfbData, roster, by = c("CBSsportsName", "Year"), type = "full")