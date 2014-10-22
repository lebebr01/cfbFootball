# read in files (Assumes using project in Rstudio and in root of folder)
teams <- read.csv(paste(getwd(), "/Data/Teams.csv", sep = ''))
yby <- read.csv(paste(getwd(), "/Data/YearByYearD1.csv", sep = ''))
polls <- read.csv(paste(getwd(), "/Data/pollsD1.csv", sep = ''))
rankings <- read.csv(paste(getwd(), "/Data/rankings.csv", sep = ''))
bowls <- read.csv(paste(getwd(), "/Data/bowlGames.csv", sep = ''))
coaches <- read.csv(paste(getwd(), "/Data/coaches.csv", sep = ''))
conference <- read.csv(paste(getwd(), '/Data/conference.csv', sep = ''))
teamNames <- read.csv(paste(getwd(), '/Data/teamNamesMerge.csv', sep = ''))
sos <- read.csv(paste(getwd(), '/Data/sosOvr.csv', sep = ''))
allAmer <- read.csv(paste(getwd(), '/Data/allAmericans.csv', sep = ''))
rivals <- read.csv(paste(getwd(), '/Data/rivals.csv', sep = ''))
espn <- read.csv(paste(getwd(), '/Data/ESPN-Recruiting.csv', sep = ''))

# Change a few team names
library(data.table)
setnames(sos, "Team", "sosTeam")
setnames(allAmer, "team", "allAmerTeam")
setnames(allAmer, 'year', 'Year')
setnames(rivals, 'School', 'rivalsTeam')
setnames(espn, tolower(names(espn)))
setnames(espn, 'ncaa.school', 'espnTeam')
setnames(espn, 'year', 'Year')

# Computing variables/Aggregating files to only include variables needed
coaches <- data.table(coaches)
coaches$fixed <- 1
coaches[, ovrWinTeam := cumsum(Win), by = c('Team', 'coach')]
coaches[, overWin := cumsum(Win), by = c('coach')]
coaches[, tenureLength := cumsum(fixed), by = c('Team', 'coach')]

allAmer <- data.table(allAmer)
allAmer$fixed <- 1
allAmer[, numAA := sum(fixed), by = c('Year', 'allAmerTeam')]
allAmer <- allAmer[, list(allAmerTeam, Year, numAA)]

rankings <- data.table(rankings)
rankings <- subset(rankings, Period %in% c('All-Time', 'last10', 'last25', 'last50'))
rankingsovr <- dcast.data.table(rankings, Team ~ Period, value.var = "TotalPoints")
rankingsWP <- dcast.data.table(rankings, Team ~ Period, value.var = 'WinPctPoints')
rankingsSP <- dcast.data.table(rankings, Team ~ Period, value.var = "SchedulePoints")
setnames(rankingsovr, c("All-Time"), "alltime")
setnames(rankingsWP, c("All-Time", "last10", "last25", "last50"), 
         c("alltimewp", "last10wp", "last25wp", "last50wp"))
setnames(rankingsSP, c("All-Time", "last10", "last25", "last50"), 
         c("alltimesp", "last10sp", "last25sp", "last50sp"))


polls <- data.table(polls)
polls <- polls[, list(Year, APRank, CoachRank, Team)]

sos <- data.table(sos)
sos <- sos[, list(Year, sosTeam, SchScore, SchRank)]

#################
# creating file for year by year with coaches
#################

library(dplyr)

#ybyCoach <- left_join(yby, coaches, by = c("Year", "Team"))
ybyCoach <- left_join(coaches, conference, by = c('Year', 'Team'))
ybyCoach <- left_join(ybyCoach, polls, by = c('Year', 'Team'))
ybyCoach <- left_join(ybyCoach, teamNames, by = c('Team'))
ybyCoach <- left_join(ybyCoach, sos, by = c('Year', 'sosTeam'))
ybyCoach <- left_join(ybyCoach, allAmer, by = c('Year', 'allAmerTeam'))
ybyCoach <- left_join(ybyCoach, rivals, by = c('Year', 'rivalsTeam'))
#ybyCoach <- left_join(ybyCoach, espn, by = c('Year', 'espnTeam'))
ybyCoach <- left_join(ybyCoach, rankingsovr, by = c('Team'))
ybyCoach <- left_join(ybyCoach, rankingsWP, by = c('Team'))
ybyCoach <- left_join(ybyCoach, rankingsSP, by = c('Team'))

# creating 1/0 variable if the team won the game
#ybyCoach$gbgWin <- ifelse(ybyCoach$WL == 'W', 1, 0)
