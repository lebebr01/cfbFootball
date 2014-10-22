library(XML)
library(plyr)

#Division 1A
theurl <- "http://www.cfbdatawarehouse.com/data/div_ia_team_index.php"
doc = htmlParse(theurl)
tableNodes1 <- getNodeSet(doc, "//table//tr//td[position()=1]//a[contains(@href, 'div_ia/')]")

schools <- sapply(X = tableNodes1, FUN = xmlValue)

addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
#addresses <- gsub("index.php$","bowl_history.php",addresses)
#addresses <- addresses[-50]

#Extracting official school name
library(doMC)
registerDoMC(cores=2)
schoolsO <- foreach(t=1:length(addresses), .combine="c") %dopar% {
  
  doc <- htmlParse(addresses[t])
  tableNodes <- getNodeSet(doc, "//table//table//tr//td//p//font//b")
  
  tb <- tableNodes[[1]]
  tb1 <- xmlValue(tb)
  substr(tb1, 10, nchar(tb1))   
}

# Pulling polls Web Addresses
addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
addresses <- gsub("index.php$","in_the_polls.php",addresses)

library(data.table)
polls <- foreach(i=1:length(addresses), .combine = "rbind", .errorhandling = "remove") %dopar% {
  
  doc <- htmlParse(addresses[i])
  tableNodes <- getNodeSet(doc, "//table//table//table//table")
  
  polltab <- readHTMLTable(tableNodes[[3]], skip.rows = 1:5, stringsAsFactors = FALSE)
  
  if(length(polltab) == 5){
    setnames(polltab, c("No", "Year", "Record", "APRank", "CoachRank"))
    
    # add school names
    polltab$Team <- schools[i]
    polltab$offName <- schoolsO[i]
    polltab
  } else {
    polltab <- NULL
    polltab
  }    
}

write.csv(polls
          , file = "Data/pollsD1.txt"
          , row.names = FALSE)



##Division 1AA
theurl <- "http://www.cfbdatawarehouse.com/data/div_iaa_team_index.php"
doc = htmlParse(theurl)

tableNodes1 <- getNodeSet(doc, "//table//tr//td[position()=1]//a[contains(@href, 'div_iaa/')]")

schools <- sapply(X = tableNodes1, FUN = xmlValue)

##Pulling Bowl Web Addresses
addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
addresses <- gsub("index.php$","in_the_polls.php",addresses)


library(data.table)
polls <- foreach(i=1:length(addresses), .combine = "rbind", .errorhandling = "remove") %dopar% {
  
  doc <- htmlParse(addresses[i])
  tableNodes <- getNodeSet(doc, "//table//table//table//table")
  
  polltab <- readHTMLTable(tableNodes[[3]], skip.rows = 1:5, stringsAsFactors = FALSE)
  
  if(length(polltab) == 5){
    setnames(polltab, c("No", "Year", "Record", "APRank", "CoachRank"))
    
    # add school names
    polltab$Team <- schools[i]
    polltab$offName <- schoolsO[i]
    polltab
  } else {
    polltab <- NULL
    polltab
  }    
}

write.csv(polls
          , file = "Data/Final/pollsD1AA.txt"
          , row.names = FALSE)
