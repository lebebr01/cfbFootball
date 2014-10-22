library(ltm)
library(reshape2)
library(data.table)
library(lavaan)
library(semPlot)

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

# mean center variables
ybyCoach$Pctmc <- with(ybyCoach, Pct - mean(Pct, na.rm = TRUE))
ybyCoach$PFmc <- with(ybyCoach, PF - mean(PF, na.rm = TRUE))
ybyCoach$overWinmc <- with(ybyCoach, overWin - mean(overWin, na.rm = TRUE))
ybyCoach$PAmc <- with(ybyCoach, PA - mean(PA, na.rm = TRUE))
ybyCoach$deltamc <- with(ybyCoach, Delta - mean(Delta, na.rm = TRUE))
ybyCoach$tenureLengthmc <- with(ybyCoach, tenureLength - mean(tenureLength, na.rm = TRUE))
ybyCoach$RivalsRankmc <- with(ybyCoach, RivalsRank - mean(RivalsRank, na.rm = TRUE))
ybyCoach$AvgStarmc <- with(ybyCoach, AvgStar - mean(AvgStar, na.rm = TRUE))
ybyCoach$numAAmc <- with(ybyCoach, numAA - mean(numAA, na.rm = TRUE))
ybyCoach$alltimemc <- with(ybyCoach, alltime - mean(alltime, na.rm = TRUE))
ybyCoach$last10mc <- with(ybyCoach, last10 - mean(last10, na.rm = TRUE))
ybyCoach$last25mc <- with(ybyCoach, last25 - mean(last25, na.rm = TRUE))
ybyCoach$last50mc <- with(ybyCoach, last50 - mean(last50, na.rm = TRUE))
ybyCoach$alltimewpmc <- with(ybyCoach, alltimewp - mean(alltimewp, na.rm = TRUE))
ybyCoach$last10wpmc <- with(ybyCoach, last10wp - mean(last10wp, na.rm = TRUE))
ybyCoach$last25wpmc <- with(ybyCoach, last25wp - mean(last25wp, na.rm = TRUE))
ybyCoach$last50wpmc <- with(ybyCoach, last50wp - mean(last50wp, na.rm = TRUE))
ybyCoach$alltimespmc <- with(ybyCoach, alltimesp - mean(alltimesp, na.rm = TRUE))
ybyCoach$last10spmc <- with(ybyCoach, last10sp - mean(last10sp, na.rm = TRUE))
ybyCoach$last25spmc <- with(ybyCoach, last25sp - mean(last25sp, na.rm = TRUE))
ybyCoach$last50spmc <- with(ybyCoach, last50sp - mean(last50sp, na.rm = TRUE))
ybyCoach$SchScoremc <- with(ybyCoach, SchScore - mean(SchScore, na.rm = TRUE))
ybyCoach$SchRankmc <- with(ybyCoach, SchRank - mean(SchRank, na.rm = TRUE))


# subset to have variables of interest
ybyCoach <- subset(ybyCoach, select = c(Year, Team, Pctmc, PFmc, PAmc, Delta, 
                                        coach, ovrWinTeam, overWinmc, tenureLength,
                                        Conference, SchScoremc, numAAmc, alltimemc,
                                        last10mc, last25mc, last50mc, bowlElig,
                                        power5conf))

# data structure for 2PL model
ybyCoach13 <- data.frame(reshape(subset(ybyCoach, select = c(Year, coach, bowlElig)), timevar = "coach", 
                      idvar = "Year", direction = "wide"))

# removing coaches with more than 30 NA values
ybyCoach13 <- subset(ybyCoach13, select = -Year)
temp <- vector(mode = "numeric", length = ncol(ybyCoach13))
for(i in 1:ncol(ybyCoach13)) {
  temp[i] <- ifelse(length(which(is.na(ybyCoach13[, i]))) > 30, i, 0)
}

# remove columns of ybyCoach13 based on 1's in temp computed above
ybyCoach13 <- subset(ybyCoach13, select = -temp[temp != 0])

# 2 parameter model predicting coach ability and discrimination
twoPL <- ltm(ybyCoach13 ~ z1, control = list(GHk = 40))
twoPL
onePL <- rasch(ybyCoach13, control = list(GHk = 40))
onePL

# due to unstable solution - remove all individuals with all 0's and 1's
temp <- vector(mode = "numeric", length = ncol(ybyCoach13))
for(i in 1:ncol(ybyCoach13)) {
  temp[i] <- ifelse(mean(ybyCoach13[, i], na.rm = TRUE) %in% c(0, 1), i, 0)
}
ybyCoach13 <- subset(ybyCoach13, select = -temp[temp != 0])

# 2 parameter model predicting coach ability and discrimination
twoPL <- ltm(ybyCoach13 ~ z1, control = list(GHk = 40))
twoPL
onePL <- rasch(ybyCoach13, control = list(GHk = 40))
onePL

##################################

# longitudinal logistic regression
library(lme4)
# turn Year into 0 through n
ybyCoach$Time <- ybyCoach$Year - min(ybyCoach$Year)
glmer.mod <- glmer(bowlElig ~ Time + Delta + overWinmc + SchScoremc +
                     power5conf + Time:power5conf + (1| coach),
                   data = ybyCoach, family = binomial)
#############################

ybyCoach <- ybyCoach[i = Year %in% 2008:2013]

# reshape into wide format for growth model
ybyCoachW <- reshape(ybyCoach, timevar = "Year", idvar = "coach", direction = "wide",
                     v.names = c("Pctmc", 'PFmc', 'PAmc', 'Delta', 'ovrWinTeam',
                                 'overWinmc', 'tenureLength', 'SchScoremc', 'numAAmc',
                                 'bowlElig', 'Time'))

grmod <- '
  # intercept and slope with fixed coefficients
    i =~ 1*Pctmc.2008 + 1*Pctmc.2009 + 1*Pctmc.2010 + 1*Pctmc.2011 + 1*Pctmc.2012 + 1*Pctmc.2012
    s =~ 0*Pctmc.2008 + 1*Pctmc.2009 + 2*Pctmc.2010 + 3*Pctmc.2011 + 4*Pctmc.2012 + 5*Pctmc.2012
  # regressions
    i ~ power5conf + alltimemc
    s ~ power5conf + alltimemc
'
fit <- growth(grmod, data = ybyCoachW)
summary(fit)

