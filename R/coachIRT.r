library(ltm)
library(reshape2)
library(data.table)
library(sem)

# loading relevant file - must be in root of cfbFootball directory
source(paste(getwd(), '/R/mergeCoach.R', sep = ''))

# subsetting bcs years
ybyCoach <- subset(ybyCoach, Year > 1997)

# removing duplicates due to merging
ybyCoach <- data.table(ybyCoach)
ybyCoach <- unique(ybyCoach, by = c('Year', 'Team', 'coach', 'Date'))

# reshaping to have coaches as the columns and number of games by year as rows
ybyCoachMelt <- melt(ybyCoach, id.vars = c('Year', 'Team', 'coach'), measure.vars = 'gbgWin')

# Creating an id variable by year, team, and coach
ybyCoachMelt <- data.table(ybyCoachMelt, key = c('Year', 'Team', 'coach'))
ybyCoachMelt[, id := (.GRP), by = c('Year', 'Team', 'coach')]

ybyCoachMelt[, idW := 1:.N, by = 'id']

# final reshape, id for games, coach, year
ybyCoachCast98 <- dcast.data.table(ybyCoachMelt, idW ~ coach, subset = (Year == 1998))[, idW := NULL]


# fitting 2 parameter model with the 'ltm' package
rmod <- rasch(na.omit(ybyCoachCast98))
twoPL <- ltm(ybyCoachCast98 ~ z1, IRT.param = TRUE, na.action = na.exclude)
threePL <- tpm(na.omit(ybyCoachCast98))


######################
# CFA
######################
ybyCoach.cov <- cov(subset(ybyCoach, Year == 2012, select = c(Pct, Delta, APRank, CoachRank)),
                    use = "complete.obs")
model.ability <- specifyModel()
a1 -> Pct, lam1
a1 -> Delta, lam2
a1 -> APRank, lam3
a1 -> CoachRank, lam4
a1 <-> a1, NA, 1


ability.cfa <- sem(model.ability, ybyCoach.cov, nrow(ybyCoach))
