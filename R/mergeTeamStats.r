library(data.table)
library(plyr)

## read in data files
load("/home/lobo/Dropbox/cfbFootball/Data/Final/cfbDataFinal-Mar20.Rdata")
teamStats <- read.table("/home/lobo/Dropbox/cfbFootball/Data/Final/teamStats.txt",
                        header = TRUE)

## making school name character
teamStats$school <- as.character(teamStats$school)

## writing files to check names


