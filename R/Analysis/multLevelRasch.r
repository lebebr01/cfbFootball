#library(ltm)
library(reshape2)
library(data.table)
#library(lavaan)
#library(semPlot)

#setwd("/Users/andrewz/Documents/GitHub/cfbFootball")
setwd('C:/Users/bleb/Dropbox/cfbFootball')
# loading relevant file - must be in root of cfbFootball directory
source(paste(getwd(), '/R/mergeCoach.R', sep = ''))


# removing duplicates due to merging
ybyCoach <- data.table(ybyCoach)

# subset to include only 2013
#ybyCoach <- ybyCoach[i = Year %in% 1980:2013]
ybyCoach <- unique(ybyCoach, by = c('Year', 'Team', 'coach'))

# removing coaches with less than 2 games for team
ybyCoach[, numGames := Win + Loss + Tie, by = list(Year, Team, coach)]
ybyCoach <- ybyCoach[i = numGames > 2]

# removing cases with NA for conference
ybyCoach <- ybyCoach[complete.cases(ybyCoach$Conference),]

# dichotomizing conference variable into dummy variables
ybyCoach$aaConf <- ifelse(ybyCoach$Conference == "American Athletic Conference", 1, 0)
ybyCoach$acConf <- ifelse(ybyCoach$Conference == "Atlantic Coast Conference", 1, 0)
ybyCoach$b12Conf <- ifelse(ybyCoach$Conference == "Big 12 Conference", 1, 0)
ybyCoach$b10Conf <- ifelse(ybyCoach$Conference == "Big Ten Conference", 1, 0)
ybyCoach$cusaConf <- ifelse(ybyCoach$Conference == "Conference USA", 1, 0)
ybyCoach$maConf <- ifelse(ybyCoach$Conference == "Mid-American Conference", 1, 0)
ybyCoach$mwConf <- ifelse(ybyCoach$Conference == "Mountain West Conference", 1, 0)
ybyCoach$p12Conf <- ifelse(ybyCoach$Conference == "Pacific-12 Conference", 1, 0)
ybyCoach$secConf <- ifelse(ybyCoach$Conference == "Southeastern Conference", 1, 0)
ybyCoach$sbConf <- ifelse(ybyCoach$Conference == "Sun Belt Conference", 1, 0)

# power 5 conference indicator
ybyCoach$power5conf <- ifelse(ybyCoach$Conference %in% 
                                c('Atlantic Coast Conference', 
                                  'Big 12 Conference', 'Big Ten Conference',
                                  'Pacific-12 Conference', 'Southeastern Conference'),
                              1, 0)

# number of all americans - if missing put 0
ybyCoach$numAA <- ifelse(is.na(ybyCoach$numAA), 0, ybyCoach$numAA)

# create bowl eligible variable
ybyCoach$bowlElig <- ifelse(ybyCoach$Win >= 6, 1, 0)

# merge in yby data
ybyCoach <- left_join(ybyCoach, data.table(yby), by = c("Year", "Team"))

# convert W/L/T into 1/0 variable
ybyCoach$wingbg <- ifelse(ybyCoach$WL == 'W', 1, 0)

# create ID variable within year, team, and coach
#ybyCoach$ID <- id(ybyCoach[c("Year", "Team", "coach")])
#ybyCoach <- data.table(ybyCoach)
#ybyCoach$wID <- ybyCoach[, wID := 1:length(wingbg), by = list(ID)]

#####################################
# create data for multilevel rasch model
library(data.table)
ybyCoach2 <- data.table(ybyCoach, key = c('Year', 'Team', 'coach'))
ybyCoach2[, ID := (.GRP), by = c('Year', 'Team', 'coach')]
ybyCoach2[, teamID := (.GRP), by = c('Team')]
ybyCoach2[, coachID := (.GRP), by = c('coach')]

library(dplyr)
ybyCoach <- data.frame(ybyCoach2)
ybyCoach <- ybyCoach %>% 
  filter(Year > 1950)

library(lme4)
fm1 <- glmer(wingbg ~ 1 + (1|coachID) + (1|teamID), data = ybyCoach,
	family = binomial)
