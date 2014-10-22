#library(ltm)
library(reshape2)
library(data.table)
library(sem)

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

# Creating an id variable by year, team, and coach
#ybyCoachMelt <- data.table(ybyCoachMelt, key = c('Year', 'Team', 'coach'))
#ybyCoachMelt[, id := (.GRP), by = c('Year', 'Team', 'coach')]

#ybyCoachMelt[, idW := 1:.N, by = 'id']

# final reshape, id for games, coach, year
#ybyCoachCast98 <- dcast.data.table(ybyCoachMelt, idW ~ coach, subset = (Year == 1998))[, idW := NULL]


# fitting 2 parameter model with the 'ltm' package
#rmod <- rasch(na.omit(ybyCoachCast98))
#twoPL <- ltm(ybyCoachCast98 ~ z1, IRT.param = TRUE, na.action = na.exclude)
#threePL <- tpm(na.omit(ybyCoachCast98))


######################
# CFA
######################
model.ability <- specifyEquations(covs = "sos, pr")
Pct = 1*ca
PF = lam1*ca
overWin = lam2*ca
PA = lam3*ca
tenureLength = lam4*ca
RivalsRank = 1*ra
AvgStar = lam5*ra
numAA = lam6*ra
alltime = 1*pr
last10 = lam7*pr
last25 = lam8*pr
last50 = lam9*pr
SchScore = 1*sos
SchRank = lam10*sos
ca = beta1*ra + gam1*sos + gam3*pr
ra = gam4*pr + gam5*sos
V(Pct) = the1
V(PF) = the2
V(overWin) = the3
V(PA) = the4
V(tenureLength) = the5
V(RivalsRank) = the6
V(AvgStar) = the7
V(numAA) = the8
V(alltime) = the9
V(last10) = the10
V(last25) = the11
V(last50) = the12
V(SchScore) = the13
V(SchRank) = the14

setnames(ybyCoach, "All-Time", "alltime")
ybyCoachSub <- data.frame(subset(ybyCoach, select = c(Pct, PF, overWin, PA, tenureLength, RivalsRank,
                                                      AvgStar, numAA, alltime, last10, last25, last50,
                                                      SchScore, SchRank)))

ybyCoach.cov <- cov(ybyCoachSub, use = "complete.obs")


ability.sem <- sem(model.ability, S = ybyCoach.cov, N = 131)

# plot results
library(semPlot)
semPaths(ability.sem, what = "path", whatLabels = "est", title = FALSE, layout = "spring")

