library(XML)
library(plyr)

##Pulling coaching tables from cfbdatawarehouse.com
#getting coaching urls
theurl <- "http://www.cfbdatawarehouse.com/data/div_ia_team_index.php"
doc = htmlParse(theurl)
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]//a")

tableNodes1 <- tableNodes[17:(length(tableNodes)-1)]

addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
addresses <- gsub("index.php$","coaching_records.php",addresses)

##looping to get every coaching address for all D1-A teams
total <- length(addresses)
pb <- txtProgressBar(min=0, max=total, style = 3)
coaches <- vector("list", length(addresses))
coachAddr <- vector("list", length(addresses))
for(i in 1:length(addresses)){
  doc = htmlParse(addresses[i])
  table <- readHTMLTable(addresses[i])
  temp <- row.names(subset(ldply(table, "length"), length == 9))
  tmp <- nrow(table[[as.numeric(temp[length(temp)-1])]])
  tableNodes <- getNodeSet(doc, "//table//tr//td//p//a")
  tableNodes1 <- tableNodes[(length(tableNodes)-tmp):(length(tableNodes)-1)]
  coaches[[i]] <- sapply(X=tableNodes1, FUN=xmlValue)
  coachAddr[[i]] <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
  setTxtProgressBar(pb, i)
}

coach <- data.frame(unlist(coaches), unlist(coachAddr), stringsAsFactors = FALSE)
colnames(coach) <- c("Name", "url")
coach$url <- paste("http://www.cfbdatawarehouse.com", substr(coach$url, 15,nchar(coach$url)), sep = "")
coach <- unique(coach)

##looping over coach to extract head coaches table
total <- nrow(coach)
pb <- txtProgressBar(min=0, max=total, style = 3)
coachTab <- vector("list", nrow(coach))
for(i in 1:nrow(coach)){
  tab <- readHTMLTable(coach$url[i])
  coachTab[[i]] <- tab[[15]]
  setTxtProgressBar(pb,i)
}
for(i in 1:nrow(coach)){
  coachTab[[i]]$coach <- coach$Name[i]
}
coachTab2 <- do.call("rbind", coachTab)
colnames(coachTab2) <- c("Year", "Team", "Win", "Loss", "Tie", 
                         "Pct", "PF", "PA", "Delta", "Coach")

##turning factors to numeric
cols <- c(1,3,4,5,6,7,8,9)
for(i in cols){
  coachTab2[,i] <- as.numeric(as.character(coachTab2[,i]))
}
coachTab2[,2] <- as.character(coachTab2[,2])
coachTab2 <- subset(coachTab2, !(Coach %in% c("No Coach", "Unknown")))
write.table(coachTab2, row.names = FALSE,
            file = "/home/lobo/Desktop/cfbFootball/Data/Final/CoachTable.txt")




##############################
#For wikipedia extract
##############################

## Extract the Wikipedia address for the head coaches

addresses <- unlist(sapply(X=tableNodes, FUN=xmlGetAttr, "href"))
addresses <- paste("http://www.wikipedia.org",addresses,sep="")
#addresses <- c(addresses[1:115], "http://www.wikipedia.org/wiki/Sonny_Dykes", addresses[116:119])

## Extract school names
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]")
schools <- sapply(X=tableNodes, FUN=xmlValue)

new <- vector("list",length(addresses))
for(i in 1:length(addresses)){
  setwd("C:/Users/lobo/Desktop/football/csv")
  theURL <- addresses[i]
  source("career.R")
  career$Name <- coaches2[i]
  new[[i]] <- career
  #write.csv(career,file=paste("coach",".csv",sep="i"))  
}

