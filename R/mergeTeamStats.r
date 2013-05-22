library(data.table)
library(plyr)

## read in data files
load("C:/Users/e520062/misc/cfbFootball/Data/Final/cfbDataFinal-Mar20.Rdata")
teamStats <- read.table("C:/Users/e520062/misc/cfbFootball/Data/Final/teamStats.txt",
                        header = TRUE)

## making school name character
teamStats$school <- as.character(teamStats$school)

## Reading in team names to merge with master cfbFootball file
teamNames <- read.csv("C:/Users/e520062/misc/cfbFootball/Data/Final/cfbDataTeam.csv")
## make all variables character
for(i in 1:ncol(teamNames)){
  teamNames[,i] <- as.character(teamNames[,i])
}

## joining teamNames to master cfbFootball file
cfbData$offName <- as.character(cfbData$offName)
cfbData <- join(cfbData, teamNames, by = "offName")

##joining teamStats to cfbData
# changing names of teamStats to match
setnames(teamStats, c("school", "year"), c("teamStatsName", "Year"))
# join
cfbData <- join(cfbData, teamStats, by = c("Year", "teamStatsName"))


## joining awards data and ESPN recruiting
## espn recruiting first
espn <- read.csv("C:/Users/e520062/misc/cfbFootball/Data/ESPN-Recruiting.csv")
## making names lower case
setnames(espn, tolower(names(espn)))

## removing first variable 'X'
espn <- espn[, -1]

## making names character and matching variables to join on
espn$ncaa.school <- as.character(espn$ncaa.school)
setnames(espn, c("year", "ncaa.school"), c("Year", "espnName"))

## joining
cfbData <- join(cfbData, espn, by = c("Year", "espnName"))

## reading data files
aa <- read.csv("C:/Users/e520062/misc/cfbFootball/Data/allAmericans.csv")
ca <- read.csv("C:/Users/e520062/misc/cfbFootball/Data/coachAwards.csv")
pa <- read.csv("C:/Users/e520062/misc/cfbFootball/Data/playerAwards.csv")

## making team character vectors
aa$team <- as.character(aa$team)
ca$school <- as.character(ca$school)
pa$school <- as.character(pa$school)

## matching names to join
setnames(aa, c("year", "team"), c("Year", "awardName"))
setnames(ca, "school", "awardName")
setnames(pa, "school", "awardName")

## joining
cfbData <- join(cfbData, aa, by = c("Year", "awardName"))
cfbData <- join(cfbData, ca, by = c("Year", "awardName"))
cfbData <- join(cfbData, pa, by = c("Year", "awardName"))

## fixing names of variables
colnames(cfbData)[141] <- "playaward.pos"
colnames(cfbData)[138] <- "playaward.player"
colnames(cfbData)[140] <- "playaward.class"
colnames(cfbData)[119] <- "espn.conference"
setnames(cfbData, c("Total", "X5.Star", "X4.Star", "X3.Star", "AvgStar", "Points", "no", "player", "pos", "ht", "wt",
                    "class", "games", "division", "rk", "signed", "fivestar", "fourstar", "threestar", "off", "def",
                    "specteams", "athlete", "name", "position", "poy", "team1", "team2", "team3", "coach", "award",
                    "trophy", "awardName"), 
         c("rivals.numRecruit", "rivals.5star", "rivals.4star", "rivals.3star", "rivals.avgstar", "rivals.points",
           "player.no", "player.name", "player.pos", "player.ht", "player.wt", "player.class", "numgames", 
           "espn.division", "espn.rk", "espn.signed", "espn.5star", "espn.4star", "espn.3star", "espn.off", "espn.def",
           "espn.specteams", "espn.athlete", "allamer.name", "allamer.pos", "allamer.poyflag", "allamer.team1", 
           "allamer.team2", "allamer.team3", "coachName", "coachAwardName", "playaward.trophy", "awardTeamName"))

save(cfbData, file = "C:/Users/e520062/misc/cfbFootball/Data/Final/cfbDataFinal-Apr11.Rdata")