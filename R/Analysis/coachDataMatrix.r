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

# Create coach matrix by year - 1930 onward
years <- 1930:2013
coachMatrix <- vector("list", length(years))
for(i in 1:length(years)) {
	# create a coach matrix
  fb13 <- fb %>% 
    filter(Year == years[i]) %>%
    select(coach, Opponent, WL)

  fbOpp <- fb %>% 
    distinct(Year, Team, coach) %>%
    filter(Year == years[i]) %>%
    group_by(Team, coach) %>% 
    select(Opponent = Team, opp.coach = coach)

  # merge
  fb13 <- left_join(fb13, fbOpp, by = "Opponent")
  fb13$win <- ifelse(fb13$WL == "W", 1, 0)

  # remove unused levels
  fb13$coach <- factor(as.character(fb13$coach))
  fb13$opp.coach <- factor(as.character(fb13$opp.coach))

  library(tidyr)

  # filter to final
  coachMatrix[[i]] <- fb13 %>% 
    group_by(Team, coach, opp.coach) %>% 
    distinct(Team, coach, opp.coach) %>%
    group_by(coach) %>%
    mutate(numGames = length(win)) %>%
    filter(is.na(opp.coach) == FALSE) %>%
    filter(numGames > 3) %>%
    select(coach, opp.coach, win) %>%
    ungroup() %>% 
    spread(opp.coach, win)
}

####################
# Rasch models for all years

library(ltm)
# library(mirt)

# mirt(coachMatrix[[84]][, -1], 1, itemtype = "Rasch")
library(foreach)

coachAbility <- foreach(ii = 1:length(coachMatrix), .combine = "rbind", .packages = "ltm",
   .errorhandling = "remove") %do% {
  data.frame(diff = summary(rasch(coachMatrix[[ii]][, -1], 
    constraint = cbind(ncol(coachMatrix[[ii]]), 1)))$coefficients[, 1], year = years[ii])
}

# remove discrimination parameter from dataframe
coachAbility <- coachAbility[grepl("Dscrmn*", rownames(coachAbility)) == FALSE ,]

# truncate large and small ability estimates - currently larger than 10 in absolute value
coachAbility$diff <- with(coachAbility, ifelse(diff > 10, 10, ifelse(diff < -10, -10, diff)))

# extract coach information from rownames
coachAbility$coach <- gsub('^Dffclt.', '', rownames(coachAbility))
# remove number from end of coach names
coachAbility$coach <- gsub('[0-9]+$', '', coachAbility$coach)

# arrange by coach and year
coachAbility <- coachAbility %>%
  arrange(coach, year)

# adjusting year to start at 0
coaches <- unique(coachAbility$coach)
year2 <- vector("list", length(coaches))
for(i in 1:length(coaches)) {
  year2[[i]] <- subset(coachAbility, coach == coaches[i])$year - min(subset(coachAbility, coach == coaches[i])$year)
}

coachAbility$Year2 <- do.call("c", year2)

# plot 
library(ggplot2)
g <- ggplot(coachAbility, aes(x = Year2, y = diff)) + theme_bw()
g + geom_line(aes(group = coach), alpha = .1) + geom_smooth()

##########################
# Combining years - each cell will represent number of wins against each other coach

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
  spread(coach, numWins)

library(ltm)
library(mirt)

# attempting graded response model
grade.mod <- mirt(aggCoach[, -1], 1, itemtype = "gpcm") # error due to gaps in scores 
grm.mod <- grm(aggCoach[, -1], constrained = TRUE)


