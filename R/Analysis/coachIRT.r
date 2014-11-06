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

# merge in yby data
ybyCoach <- left_join(ybyCoach, data.table(yby), by = c("Year", "Team"))

# convert W/L/T into 1/0 variable
ybyCoach$wingbg <- ifelse(ybyCoach$WL == 'W', 1, 0)

# create ID variable within year, team, and coach
ybyCoach$ID <- id(ybyCoach[c("Year", "Team", "coach")])
ybyCoach <- data.table(ybyCoach)
ybyCoach$wID <- ybyCoach[, wID := 1:length(wingbg), by = list(ID)]

##########################################
# Creating a coach matrix
temp <- subset(ybyCoach, Year == 2013)
temp2 <- subset(temp, select = c(coach, Opponent, WL))
tempMerge <- subset(temp, select = c(Team, coach))
library(data.table)
setnames(tempMerge, c('Opponent', 'opp.coach'))
library(dplyr)
tempMerge <- tempMerge[!duplicated(tempMerge), ]
temp2 <- left_join(temp2, tempMerge, by = 'Opponent')
temp2$win <- ifelse(temp2$WL == 'W', 1, 0)
temp2 <- subset(temp2, opp.coach != '<NA>')
library(reshape2)
coachMatrix <- acast(data.frame(temp2), coach ~ opp.coach, value.var = "win")



# loop through years creating and fit rasch model within each year to coaches
year <- subset(unique(ybyCoach$Year), unique(ybyCoach$Year) > 1930)
raschVal <- vector("list", length(year))
for(i in 1:length(unique(ybyCoach$Year))) {
  x <- subset(ybyCoach, Year == year[i])
  # data structure for rasch model
  xw <- data.frame(reshape(subset(x, select = c(wID, coach, wingbg)), timevar = "coach", 
                                   idvar = "wID", direction = "wide"))
  raschVal[[i]] <- data.frame(rasch(subset(xw, select = -wID),
                                    constraint = cbind(ncol(xw) + 1, 1), control = list(GHk = 15))$coefficients,
                              year = year[i],
                              Team = unique(x, by = c('Team', 'coach'))$Team)
}

raschValC <- do.call('rbind', raschVal)
raschValC$coach2 <- gsub('\\.+', ' ', gsub('wingbg\\.', '', rownames(raschValC)))
raschValC$coach2 <- gsub("\\d+$", "", raschValC$coach2)
setnames(raschValC, 'year', 'Year')

# removing punctuation from ybyCoach coach name
ybyCoach$coach2 <- gsub('[[:punct:]]', '', ybyCoach$coach)

# one row per coach, year, and team
ybyCoach2 <- unique(ybyCoach, by = c('Year', 'Team', 'coach'))

# merge raschValC with ybyCoach.
library(dplyr)
raschValC2 <- left_join(raschValC, ybyCoach2, by = c('Team', 'Year', 'coach2'))
raschValC2 <- arrange(raschValC2, coach2, Year)

# adjusting year to start at 0
coaches <- unique(raschValC2$coach2)
year2 <- vector("list", length(coaches))
for(i in 1:length(coaches)) {
  year2[[i]] <- subset(raschValC2, coach2 == coaches[i])$Year - min(subset(raschValC2, coach2 == coaches[i])$Year)
}

raschValC2$Year2 <- do.call("c", year2)

save(raschValC2, file = "Data/Analysis/raschData.rda")

##############################################################

load(file = "Data/Analysis/raschData.rda")

# plotting coach ability level
library(ggplot2)
l <- ggplot(raschValC2, aes(x = Year2, y = beta.i, group = coach2)) + theme_bw()
l + geom_line()

l <- ggplot(subset(raschValC2, power5conf == 1), aes(x = Year2, y = beta.i)) + theme_bw()
l + geom_line(aes(group = coach2)) + coord_cartesian(ylim = c(-4, 4)) + geom_smooth()

# modeling coach ability level
library(lme4)
simp.mod <- lmer(beta.i ~ 1 + Year2 + (1 + Year2|coach2), data = raschValC2)
quad.mod <- lmer(beta.i ~ 1 + Year2 + I(Year2^2) + (1 + Year2|coach2), data = raschValC2)

# scaling variables
raschValC2$overWinmc <- scale(raschValC2$overWin, center = TRUE, scale = FALSE)
raschValC2$alltimemc <- scale(raschValC2$alltime, center = TRUE, scale = TRUE)
raschValC2$numGamesmc <- scale(raschValC2$numGames, center = TRUE, scale = FALSE)

# final model
mod.pred <- lmer(beta.i ~ 1 + Year2 + overWinmc + Year2:overWinmc + 
                        alltimemc + Year2:alltimemc + numAA + Year2:numAA + 
                        numGamesmc + numGamesmc:Year2 + power5conf + Year2:power5conf + 
                        (1 + Year2|coach2), data = raschValC2)

# printing coefficients for latex file
library(xtable)
coef.xt <- xtable(summary(mod.pred)$coefficients, caption = "Model estimates", digits = 3)

# Coach names to extract to plot growth curves.
HistsucCoach <- c("Bobby Bowden", "Paul W Bear Bryant", "Tom Osborne", 
                     "Lou Holtz", "Mack Brown", "Hayden Fry", "Glenn Bo Schembechler")
cursucCoach <- c("Frank Beamer", "Steve Spurrier", "Urban Meyer", "Bob Stoops",
                 "Nick L Saban", "Brian Kelly", "Gary Patterson")
Upcoming <- c("David Shaw", "Jimbo Fisher", "David Cutcliffe", "Pat Fitzgerald",
              "Jerry Kill", "Todd Graham", "Kevin Sumlin", "Al Golden")

#################
# Extract random effects
mod.coef <- summary(mod.pred)$coefficients
re <- data.frame(ranef(mod.pred)[[1]], coach = rownames(ranef(mod.pred)[[1]]))
library(data.table)
setnames(re, c("Intercept", "Year2", "coach"))

re$indInt <- mod.coef[1,1] + re$Intercept
re$indSlope <- mod.coef[2,1] + re$Year2

# simp.coef <- summary(simp.mod)$coefficients
# re <- data.frame(ranef(simp.mod)[[1]], coach = rownames(ranef(simp.mod)[[1]]))
# setnames(re, c("Intercept", "Year2", "coach"))
# 
# re$indInt <- simp.coef[1,1] + re$Intercept
# re$indSlope <- simp.coef[2,1] + re$Year2

hsc <- subset(re, coach %in% HistsucCoach)
csc <- subset(re, coach %in% cursucCoach)
uc <- subset(re, coach %in% Upcoming)

# growth curves ignoring covariates for now
library(ggplot2)
library(RColorBrewer)
library(tikzDevice)
g <- ggplot(hsc, aes(x = Intercept, y = Year2)) + theme_bw()
tikz(file = "paper/pastSCoach.tex")
g + geom_blank() + geom_abline(intercept = mod.coef[1,1], slope = mod.coef[2,1], size = 1) + 
  scale_x_continuous("Years Coaching", limits = c(0, 30)) + 
  scale_y_continuous("Ability", limits = c(-2.5, 2.5)) + 
  geom_abline(data = hsc, aes(intercept = indInt, slope = indSlope, color = coach),
              size = 1, show_guide = TRUE) +
  scale_color_brewer("Coach", palette = "Dark2")
dev.off()

# current successful coaches
g <- ggplot(hsc, aes(x = Intercept, y = Year2)) + theme_bw()
tikz(file = "paper/curScoach.tex")
g + geom_blank() + geom_abline(intercept = mod.coef[1,1], slope = mod.coef[2,1], size = 1) + 
  scale_x_continuous("Years Coaching", limits = c(0, 30)) + 
  scale_y_continuous("Ability", limits = c(-2.5, 2.5)) + 
  geom_abline(data = csc, aes(intercept = indInt, slope = indSlope, color = coach),
              size = 1, show_guide = TRUE) +
  scale_color_brewer("Coach", palette = "Dark2")
dev.off()

# upcoming coaches
g <- ggplot(hsc, aes(x = Intercept, y = Year2)) + theme_bw()
tikz(file = 'paper/upcomingCoach.tex')
g + geom_blank() + geom_abline(intercept = mod.coef[1,1], slope = mod.coef[2,1], size = 1) + 
  scale_x_continuous("Years Coaching", limits = c(0, 30)) + 
  scale_y_continuous("Ability", limits = c(-2.5, 2.5)) + 
  geom_abline(data = uc, aes(intercept = indInt, slope = indSlope, color = coach),
              size = 1, show_guide = TRUE) +
  scale_color_brewer("Coach", palette = "Dark2")
dev.off()

#################################
# Spaghetti plots of coaches actual ability estimates
library(ggplot2)
library(RColorBrewer)
library(tikzDevice)


HistsucCoach <- c("Bobby Bowden", "Paul W Bear Bryant", "Tom Osborne", 
                  "Lou Holtz", "Mack Brown", "Hayden Fry", "Glenn Bo Schembechler")
cursucCoach <- c("Frank Beamer", "Steve Spurrier", "Urban Meyer", "Bob Stoops",
                 "Nick L Saban", "Brian Kelly", "Gary Patterson")
Upcoming <- c("David Shaw", "Jimbo Fisher", "David Cutcliffe", "Pat Fitzgerald",
              "Jerry Kill", "Todd Graham", "Kevin Sumlin", "Al Golden")


l <- ggplot(subset(raschValC2, coach2 %in% HistsucCoach), 
            aes(x = Year2, y = beta.i)) + theme_bw()
tikz(file = "paper/histSpaghetti.tex")
l + geom_line(aes(group = coach2)) + facet_grid(.~coach2)
dev.off()

l <- ggplot(subset(raschValC2, coach2 %in% cursucCoach), 
            aes(x = Year2, y = beta.i)) + theme_bw()
tikz(file = "paper/cursSpaghetti.tex")
l + geom_line(aes(group = coach2)) + facet_grid(.~coach2)
dev.off()

l <- ggplot(subset(raschValC2, coach2 %in% Upcoming), 
            aes(x = Year2, y = beta.i)) + theme_bw()
tikz(file = "paper/upcSpaghetti.tex")
l + geom_line(aes(group = coach2)) + facet_grid(.~coach2)
dev.off()


