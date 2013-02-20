##reading in data
setwd("/home/lobo/Desktop/cfbFootball/Data/Final")

ybyd1A <- read.table("YearByYear.txt", header = TRUE)
ybyd1AA <- read.table("YearByYearD1AA.txt", header = TRUE)

pollsd1A <- read.table("pollsD1A.txt", header = TRUE)
pollsd1AA <- read.table("pollsD1AA.txt", header = TRUE)

confd1A <- read.table("confAffilD1A.txt", header = TRUE)
names(confd1A)[1] <- "Year"
confd1A <- subset(confd1A, select = 1:5)
confd1AA <- read.table("confAffilD1AA.txt", header = TRUE)
names(confd1AA)[1:2] <- c("Year", "conference")
confd1AA <- subset(confd1AA, select = 1:5)

bowlsd1A <- read.table("bowlsD1A.txt", header = TRUE)
names(bowlsd1A)[11] <- "school"
bowlsd1AA <- read.table("bowlsD1AA.txt", header = TRUE)
names(bowlsd1AA)[11] <- "school"

bowlsd1A <- subset(bowlsd1A, select = c(6,8:11))
bowlsd1AA <- subset(bowlsd1AA, select = c(6,8:11))

coach <- read.table("CoachTable.txt", header = TRUE)
names(coach)[c(2,7:9)] <- c("school", "PF.Year", "PA.Year", "Delta.Year")

##merging data yby and bowls
library(plyr)
library(data.table)

combd1A <- join(ybyd1A, bowlsd1A, by = c("Month", "Day", "Year", "school"))
combd1AA <- join(ybyd1AA, bowlsd1AA, by = c("Month", "Day", "Year", "school"))

##computing football year overall
names(combd1A)[10] <- "YearGBG"
names(combd1AA)[10] <- "YearGBG"
combd1A$Year <- with(combd1A, ifelse(Month == 1, YearGBG-1, YearGBG))
combd1AA$Year <- with(combd1AA, ifelse(Month == 1, YearGBG-1, YearGBG))

##merging rest of files
combd1A <- join(combd1A, pollsd1A, by = c("Year", "school"))
combd1A <- join(combd1A, confd1A, by = c("Year", "school"))

##Making coach into data table to remove duplicates
coach.dt <- data.table(coach, key = "Year,school,Coach")
coach.dt <- unique(coach.dt)

combd1A <- join(combd1A, coach.dt, by = c("Year", "school"))

combd1AA <- join(combd1AA, pollsd1AA, by = c("Year", "school"))
combd1AA <- join(combd1AA, confd1AA, by = c("Year", "school"))
combd1AA <- join(combd1AA, coach.dt, by = c("Year", "school"))

##Adding flag to separate d1-A from d1-AA files
combd1A$d1aFlag <- 1
combd1AA$d1aFlag <- 0

cfbData <- rbind(combd1A, combd1AA)

#Saving cfbData to .Rdata file
save(cfbData, file = "/home/lobo/Desktop/cfbFootball/Data/Final/cfbDataFinal.Rdata")

##adding Rivals data
load("cfbDataFinal.Rdata")

cfbData$SchoolRival <- cfbData$school
cfbData$SchoolRival <- gsub("\\(.+\\)$", "", cfbData$SchoolRival)
cfbData$SchoolRival <- gsub("^\\s+|\\s+$", "", cfbData$SchoolRival)
cfbData$SchoolRival <- gsub("St.$", "State", cfbData$SchoolRival)

rivals <- read.table("rivals.txt", header = TRUE)
setnames(rivals, "School", "SchoolRival")
rivals$Year <- as.numeric(rivals$Year)
rivals$SchoolRival <- as.character(rivals$SchoolRival)

cfbData <- join(cfbData, rivals, by = c("SchoolRival", "Year"))

save(cfbData, file = "C:/Users/e520062/Desktop/cfbFootball/Data/Final/cfbDataFinal-Feb2013.Rdata")

