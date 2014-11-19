##########################
# Combining years - each cell will represent number of wins against each other coach
##############################################
# read in data
setwd("C:/Users/bleb/Dropbox/cfbFootball")
ybyCoach <- read.csv(paste0(getwd(), "/Data/College-Football-2014-11-04.csv"))
yby <- read.csv(paste(getwd(), "/Data/YearByYearD1.csv", sep = ''))

library(dplyr)

# create distinct records in ybyCoach by year, Team, coach
fb <- ybyCoach %>% 
  group_by(Year, Team, coach) %>% 
  distinct(Year, Team, coach) %>% 
  mutate(numGames = Win + Loss + Tie) %>% 
  filter(numGames > 2)

# removing cases with NA for conference
fb <- fb[complete.cases(fb$Conference),]

# dichotomizing conference variable into dummy variables
fb$aaConf <- ifelse(fb$Conference == "American Athletic Conference", 1, 0)
fb$acConf <- ifelse(fb$Conference == "Atlantic Coast Conference", 1, 0)
fb$b12Conf <- ifelse(fb$Conference == "Big 12 Conference", 1, 0)
fb$b10Conf <- ifelse(fb$Conference == "Big Ten Conference", 1, 0)
fb$cusaConf <- ifelse(fb$Conference == "Conference USA", 1, 0)
fb$maConf <- ifelse(fb$Conference == "Mid-American Conference", 1, 0)
fb$mwConf <- ifelse(fb$Conference == "Mountain West Conference", 1, 0)
fb$p12Conf <- ifelse(fb$Conference == "Pacific-12 Conference", 1, 0)
fb$secConf <- ifelse(fb$Conference == "Southeastern Conference", 1, 0)
fb$sbConf <- ifelse(fb$Conference == "Sun Belt Conference", 1, 0)

# power 5 conference indicator
fb$power5conf <- ifelse(fb$Conference %in% 
                          c('Atlantic Coast Conference', 
                            'Big 12 Conference', 'Big Ten Conference',
                            'Pacific-12 Conference', 'Southeastern Conference'),
                        1, 0)

# number of all americans - if missing put 0
fb$numAA <- ifelse(is.na(fb$numAA), 0, fb$numAA)

# create bowl eligible variable
fb$bowlElig <- ifelse(fb$Win >= 6, 1, 0)

# merge in yby data
fb <- left_join(fb, yby, by = c("Year", "Team"))

# convert W/L/T into 1/0 variable
fb$wingbg <- ifelse(fb$WL == 'W', 1, 0)

coach <- fb %>%
  select(Year, Team, coach, Opponent, wingbg)

opp.coach <- fb %>%
  distinct(Year, Team, coach) %>%
  group_by(Team, coach) %>% 
  select(Year, Opponent = Team, opp.coach = coach)

# Join two together
coach <- left_join(coach, opp.coach, by = c('Year', 'Opponent'))

library(tidyr)

aggCoach <- coach %>%
  filter(Year > 1950) %>%
  group_by(coach, opp.coach) %>%
  filter(is.na(opp.coach) == FALSE) %>%
  mutate(numWins = sum(wingbg)) %>%
  distinct(coach, opp.coach) %>%
  select(coach, opp.coach, numWins) %>%
  ungroup() %>%
  spread(opp.coach, numWins)

library(ltm)
library(mirt)

# remove gaps in categories. 


# attempting graded response model
grade.mod <- mirt(aggCoach[, -1], 1, itemtype = "graded") # error due to gaps in scores 
grm.mod <- grm(aggCoach[, -1], constrained = TRUE)
