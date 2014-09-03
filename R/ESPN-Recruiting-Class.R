library(XML)
library(dplyr)

##Set years from 2006–2013
year <- 2006:2013

##Set up an empty list
recruits <- vector("list", length(year))

##Scrape the ESPN recruiting scorecard data
for(i in 1:length(year)){
  urlaa <- paste("http://espn.go.com/college-sports/football/recruiting/scorecard/_/class/", year[i], "/type/aa", sep="")
  urla <- paste("http://espn.go.com/college-sports/football/recruiting/scorecard/_/class/", year[i], "/type/a", sep="")
  docaa = htmlParse(urlaa)
  doca = htmlParse(urla)
  aa <- readHTMLTable(docaa, skip.rows = 1:2, header = TRUE)$'NULL'
  a <- readHTMLTable(doca, skip.rows = 1:2, header = TRUE)$'NULL'
  recruits[[i]] <- merge(a, aa, all = TRUE)
  recruits[[i]]$year <- year[i]
}

##Merge each year's data into one data frame
espn <- Reduce(function(...) merge(..., all = TRUE), recruits)

##Make most variables numeric
cols = c(3:13)
espn[ ,cols] = apply(espn[ ,cols], 2, function(x) as.numeric(as.character(x)))

##Dump first column
# espn <- espn[ , -1]

##Rename variables
names(espn) <- c("espnNames", "conference", "signed", "five", "four", "three", 
	"off", "def", "specTeams", "athletes", "year", "espn300", "espn150")

espn<-arrange(espn, espnNames, year)
##Write data to text file
write.table(espn, file = "Data/espnRecruiting.txt", row.names = FALSE)
