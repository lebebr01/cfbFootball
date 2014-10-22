#library(ltm)
library(reshape2)
library(data.table)
library(lavaan)
library(semPlot)

# loading relevant file - must be in root of cfbFootball directory
source(paste(getwd(), '/R/mergeCoach.R', sep = ''))

# removing duplicates due to merging
ybyCoach <- data.table(ybyCoach)

# subset to include only 2013
ybyCoach <- ybyCoach[i = Year == 2013]
ybyCoach <- unique(ybyCoach, by = c('Year', 'Team', 'coach'))

# removing coaches with less than 2 games for team
ybyCoach[, numGames := Win + Loss + Tie, ]
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

# number of all americans - if missing put 0
ybyCoach$numAA <- ifelse(is.na(ybyCoach$numAA), 0, ybyCoach$numAA)

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

# log of rankings
ybyCoach$alltimelog <- log(ybyCoach$alltime)
ybyCoach$last10log <- log(ybyCoach$last10)
ybyCoach$last25log <- log(ybyCoach$last25)
ybyCoach$last50log <- log(ybyCoach$last50)

ybyCoach$alltimewplog <- log(ybyCoach$alltimewp)
ybyCoach$last10wplog <- log(ybyCoach$last10wp)
ybyCoach$last25wplog <- log(ybyCoach$last25wp)
ybyCoach$last50wplog <- log(ybyCoach$last50wp)

ybyCoach$alltimesplog <- log(ybyCoach$alltimesp)
ybyCoach$last10splog <- log(ybyCoach$last10sp)
ybyCoach$last25splog <- log(ybyCoach$last25sp)
ybyCoach$last50splog <- log(ybyCoach$last50sp)

# log of overWin
# first removing any 0's
ybyCoach$overWin <- ifelse(ybyCoach$overWin == 0, ybyCoach$overWin + .000001, ybyCoach$overWin)
ybyCoach$overWinlog <- log(ybyCoach$overWin)

# log of Delta
ybyCoach$Delta2 <- ybyCoach$Delta + 343
ybyCoach$deltasqrt <- sqrt(ybyCoach$Delta2)

# create variable if ranked in AP poll or not
ybyCoach$aprankDummy <- ifelse(is.na(ybyCoach$APRank), 0, 1)


######################
# sem - lavaan package
########################
ability.mod <- '
 # latent variables
 ca =~ Pctmc + overWinlog + tenureLengthmc + deltasqrt
 ra =~ AvgStarmc + numAAmc
 sos =~ SchScoremc 
 pr =~ last10log 
 # regressions
 ca ~ ra + sos + pr + aaConf + acConf + b12Conf + b10Conf + cusaConf + maConf + mwConf + p12Conf + secConf + sbConf
 ra ~ sos + pr + aaConf + acConf + b12Conf + b10Conf + cusaConf + maConf + mwConf + p12Conf + secConf + sbConf + aprankDummy
 # residual covariances
 Pctmc ~~ overWinlog
'

ability.fit <- sem(ability.mod, data = ybyCoach)
summary(ability.fit)

# Plot model
semPaths(ability.fit, what = "path", whatLabels = "est", title = FALSE, layout = "spring")

######################
# sem - sem package
######################
# model.ability <- specifyEquations(covs = "sos, pr")
# Pct = 1*ca
# PF = lam1*ca
# overWin = lam2*ca
# PA = lam3*ca
# tenureLength = lam4*ca
# RivalsRank = 1*ra
# AvgStar = lam5*ra
# numAA = lam6*ra
# alltime = 1*pr
# last10 = lam7*pr
# last25 = lam8*pr
# last50 = lam9*pr
# SchScore = 1*sos
# SchRank = lam10*sos
# ca = beta1*ra + gam1*sos + gam3*pr
# ra = gam4*pr + gam5*sos
# V(Pct) = the1
# V(PF) = the2
# V(overWin) = the3
# V(PA) = the4
# V(tenureLength) = the5
# V(RivalsRank) = the6
# V(AvgStar) = the7
# V(numAA) = the8
# V(alltime) = the9
# V(last10) = the10
# V(last25) = the11
# V(last50) = the12
# V(SchScore) = the13
# V(SchRank) = the14


# ybyCoachSub <- data.frame(subset(ybyCoach, select = c(Pct, PF, overWin, PA, tenureLength, RivalsRank,
#                                                       AvgStar, numAA, alltime, last10, last25, last50,
#                                                       SchScore, SchRank)))
# 
# ybyCoach.cov <- cov(ybyCoachSub, use = "complete.obs")
# 
# 
# ability.sem <- sem(model.ability, S = ybyCoach.cov, N = 131)




# plot results

#semPaths(ability.sem, what = "path", whatLabels = "est", title = FALSE, layout = "spring")

