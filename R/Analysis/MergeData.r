# read in files (Assumes using project in Rstudio and in root of folder)
setwd("C:/Users/bleb/Dropbox/cfbFootball")
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

# creating new aggregate variables and removing unneeded columns
library(dplyr)
coaches$fixed <- 1
coaches <- coaches %>% 
  group_by(Team, coach) %>%
  mutate(ovrWinTeam = cumsum(Win),
  	tenureLength = cumsum(fixed))
coaches <- coaches %>% 
  group_by(coach) %>%
  mutate(ovrWin = cumsum(Win))

allAmer$fixed <- 1
allAmer <- allAmer %>%
  group_by(Year, allAmerTeam) %>%
  mutate(numAA = sum(fixed)) %>%
  select(allAmerTeam, Year, numAA)

polls <- polls %>%
  select(Year, APRank, CoachRank, Team)
sos <- sos %>%
  select(Year, sosTeam, SchScore, SchRank)

# Merge files together
ybyCoach <- left_join(coaches, conference, by = c('Year', 'Team'))
ybyCoach <- left_join(ybyCoach, polls, by = c('Year', 'Team'))
ybyCoach <- left_join(ybyCoach, teamNames, by = c('Team'))
ybyCoach <- left_join(ybyCoach, sos, by = c('Year', 'sosTeam'))
ybyCoach <- left_join(ybyCoach, allAmer, by = c('Year', 'allAmerTeam'))
ybyCoach <- left_join(ybyCoach, rivals, by = c('Year', 'rivalsTeam'))

# write to file
write.csv(ybyCoach, file = paste0(getwd(), "/Data/College-Football-2014-11-04.csv"), row.names = FALSE)


