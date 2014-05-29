# read in files (Assumes using project in Rstudio and in root of folder)
teams <- read.csv(paste(getwd(), "/Data/Teams.csv", sep = ''))
yby <- read.csv(paste(getwd(), "/Data/YearByYearD1.csv", sep = ''))
polls <- read.csv(paste(getwd(), "/Data/pollsD1.csv", sep = ''))
rankings <- read.csv(paste(getwd(), "/Data/rankings.csv", sep = ''))
bowls <- read.csv(paste(getwd(), "/Data/bowlGames.csv", sep = ''))
coaches <- read.csv(paste(getwd(), "/Data/coaches.csv", sep = ''))

#################
# create team level file
#################


