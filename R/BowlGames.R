library(XML)
library(plyr)

#Division 1A
theurl <- "http://www.cfbdatawarehouse.com/data/div_ia_team_index.php"
doc = htmlParse(theurl)
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]//a")

tableNodes1 <- tableNodes[17:(length(tableNodes)-3)]

schools <- sapply(X = tableNodes1, FUN = xmlValue)

addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
#addresses <- gsub("index.php$","bowl_history.php",addresses)
#addresses <- addresses[-50]

#Extracting official school name
library(doMC)
registerDoMC(cores=2)
schoolsO <- foreach(t=1:length(addresses), .combine="c") %do% {
  
  doc <- htmlParse(addresses[t])
  tableNodes <- getNodeSet(doc, "//table//table//tr//td//p//font//b")
  
  tb <- tableNodes[[1]]
  tb1 <- xmlValue(tb)
  substr(tb1, 10, nchar(tb1))   
}

##Pulling Bowl Web Addresses
addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
addresses <- gsub("index.php$","bowl_history.php",addresses)

#filenames <- paste("/home/lobo/Desktop/cfbFootball/Data/bowlD1A/", schools, ".txt", sep="")

##Extracting schools
library(data.table)
bowls <- foreach(t=1:length(addresses), .combine = "rbind", .errorhandling = "remove") %do% {
  
  doc <- htmlParse(addresses[t])
  tableNodes <- getNodeSet(doc, "//table//table")
  
  # Extract correct tables
  majorBowls <- readHTMLTable(tableNodes[[13]])
  minorBowls <- readHTMLTable(tableNodes[[16]])
  
  # ensuring number of columns if correct
  if(length(majorBowls) != 7){
    majorBowls <- readHTMLTable(tableNodes[[12]])
  }
  if(length(minorBowls) != 7){
    minorBowls <- readHTMLTable(tableNodes[[15]])
  }
  
  # rbind if minorBowls is not NULL.
  if(is.null(minorBowls)){
    bowl <- rbind(majorBowls, minorBowls)
  } else {
    bowl <- majorBowls
  }  
  
  # Set colnames
  setnames(bowl, c("No", "WL", "Date", "PF", "Opponent", "PA", "Bowl"))
  
  bowl$Team <- schools[t]
  bowl$offName <- schoolsO[t]
  bowl
}

write.csv(bowls, file = '/home/brandon/Dropbox/cfbFootball/Data/bowlGames.csv')

#Division I-AA
theurl <- "http://www.cfbdatawarehouse.com/data/div_iaa_team_index.php"
doc = htmlParse(theurl)
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]//a")

tableNodes1 <- tableNodes[18:length(tableNodes)-1]

schools <- sapply(X = tableNodes1, FUN = xmlValue)

##Pulling Bowl Web Addresses
addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
addresses <- gsub("index.php$","bowl_history.php",addresses)

filenames <- paste("/home/lobo/Desktop/cfbFootball/Data/bowlD1AA/", schools, ".txt", sep="")

##Extracting schools
for(t in 1:length(addresses)){
  
  doc <- htmlParse(addresses[t])
  tableNodes <- getNodeSet(doc, "//table//table")
  
  source("/home/lobo/Desktop/football/Bowl.R")
  
  tab$Team <- schools[t]
  tab <- subset(tab, Team == schools[t], select = c(2:length(tab)))
  
  ##Writing table
  write.table(x = tab, file = filenames[t], row.names = FALSE)
}


addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
#addresses <- gsub("index.php$","bowl_history.php",addresses)
#addresses <- addresses[-50]

#Extracting official school name
library(doMC)
registerDoMC(cores=2)
schoolsO <- foreach(t=1:length(filenames), .combine="rbind") %dopar% {
  
  doc <- htmlParse(addresses[t])
  tableNodes <- getNodeSet(doc, "//table//table//tr//td//p//font//b")
  
  tb <- tableNodes[[1]]
  tb1 <- xmlValue(tb)
  substr(tb1, 10, nchar(tb1))   
}

#Reading in bowl games
bowls <- vector("list",length(filenames))
for(i in 1:length(filenames)) {
  bowls[[i]] <- read.table(file = filenames[i], header = TRUE)
}

##Adding official team name to each team
lenBowls <- sapply(bowls, nrow)
for(i in 1:length(filenames)){
  bowls[[i]]$offName <- rep(schoolsO[i], lenBowls[i])
}
#combining all the teams into one file
bowlsD1AA <- do.call("rbind",bowls)
write.table(bowlsD1AA, file = "/home/lobo/Desktop/cfbFootball/Data/Final/bowlsD1AA.txt",
            row.names=FALSE)