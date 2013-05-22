library(XML)
library(data.table)
library(plyr)

## years of team stats
years <- 2000:2012

## base url
base.url <- "http://www.sports-reference.com/cfb/years/"

## urls for offense, defense, special teams for all years
off.urls <- paste(base.url, years, "-team-offense.html", sep = "")
def.urls <- paste(base.url, years, "-team-defense.html", sep = "")
spec.urls <- paste(base.url, years, "-special-teams.html", sep = "")

## creating list for offense
offense <- vector("list", length(off.urls))

## looping over urls
for(i in 1:length(off.urls)){
  offense[[i]] <- readHTMLTable(off.urls[i])
  offense[[i]] <- na.omit(offense[[i]]$offense)
  offense[[i]] <- offense[[i]][-c(grep("[A-Za-z]", offense[[i]]$G)),]
  offense[[i]]$year <- years[i]
}

## combining years
off <- do.call("rbind", offense)
setnames(off, c("off.rank", "school", "games", "off.points", "off.pass.cmp", "off.pass.att", "off.pass.pct", 
                "off.pass.yds", "off.pass.td", "off.rush.att", "off.rush.yds", "off.rush.avg", "off.rush.td", 
                "off.toff.plays", "off.toff.yds", "off.toff.avg", "off.fd.pass", "off.fd.rush", "off.fd.pen", 
                "off.fd.tot", "off.pen.num", "off.pen.yds", "off.turn.fum", "off.turn.int", "off.turn.tot", "year"))

## fixing off variable structures
off$school <- as.character(off$school)
for(i in c(1,3:26)){
  off[,i] <- as.numeric(as.character(off[,i]))
}

## creating list for defense
defense <- vector("list", length(def.urls))

## looping over urls
for(i in 1:length(def.urls)){
  defense[[i]] <- readHTMLTable(def.urls[i])
  defense[[i]] <- na.omit(defense[[i]]$defense)
  defense[[i]] <- defense[[i]][-c(grep("[A-Za-z]", defense[[i]]$G)),]
  defense[[i]]$year <- years[i]
}

## combining years
def <- do.call("rbind", defense)
setnames(def, c("def.rank", "school", "games", "def.points", "def.pass.cmp", "def.pass.att", "def.pass.pct", 
                "def.pass.yds", "def.pass.td", "def.rush.att", "def.rush.yds", "def.rush.avg", "def.rush.td", 
                "def.toff.plays", "def.toff.yds", "def.toff.avg", "def.fd.pass", "def.fd.rush", "def.fd.pen", 
                "def.fd.tot", "def.pen.num", "def.pen.yds", "def.turn.fum", "def.turn.int", "def.turn.tot", "year"))

## fixing def variable structures
def$school <- as.character(def$school)
for(i in c(1,3:26)){
  def[,i] <- as.numeric(as.character(def[,i]))
}

## creating list for special teams
spec <- vector("list", length(spec.urls))

## looping over urls
for(i in 1:length(spec.urls)){
  spec[[i]] <- readHTMLTable(spec.urls[i])
  spec[[i]] <- na.omit(spec[[i]]$special)
  spec[[i]] <- spec[[i]][-c(grep("[A-Za-z]", spec[[i]]$G)),]
  spec[[i]]$year <- years[i]
}

## combining years
spc <- do.call("rbind", spec)
setnames(spc, c("spec.rank", "school", "games", "kick.xpm", "kick.xpa", "kick.xpp", "kick.fgm", "kick.fga",
                "kick.fgp", "kick.pts", "punt.num", "punt.yds", "punt.avg", "kr.num", "kr.yds", 
                "kr.avg", "kr.td", "pr.num", "pr.yds", "pr.avg", "pr.td", "year"))

## fixing spc variable structures
spc$school <- as.character(spc$school)
for(i in c(1,3:22)){
  spc[,i] <- as.numeric(as.character(spc[,i]))
}

## joining by school, games, and year
teamStats <- join(off, def, by = c("year", "school", "games"))
teamStats <- join(teamStats, spc, by = c("year", "school", "games"))

write.table(teamStats, file = "C:/Users/e520062/Dropbox/cfbFootball/Data/Final/teamStats.txt", 
            row.names = FALSE)