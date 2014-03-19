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

write.csv(polls, file = "/home/brandon/Dropbox/cfbFootball/Data/pollsD1.txt", 
            row.names = FALSE)



##Division 1AA
theurl <- "http://www.cfbdatawarehouse.com/data/div_iaa_team_index.php"
doc = htmlParse(theurl)
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]//a")

tableNodes1 <- tableNodes[18:length(tableNodes)-1]

schools <- sapply(X = tableNodes1, FUN = xmlValue)

##Pulling Bowl Web Addresses
addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
addresses <- gsub("index.php$","in_the_polls.php",addresses)

total <- length(addresses)
pb <- txtProgressBar(min=0, max=total, style = 3)
polls <- vector("list", length(addresses))
for(i in c(1:40,42:84,86:95,97:length(addresses))){
  temp <- readHTMLTable(addresses[i])
  temp2 <- na.omit(temp[[16]][,2:ncol(temp[[16]])])
  if(length(temp2) == 1){
    next 
  } else{
  for(j in 1:ncol(temp2)){
    temp2[,j] <- as.character(temp2[,j])
  }
  names(temp2) <- gsub("\\s", "", temp2[1,])
  temp2 <- temp2[-1,]
  temp2$school <- schools[i]
  polls[[i]] <- temp2
  }
  setTxtProgressBar(pb, i)
}  

polls <- polls[!sapply(polls, is.null)]

for(i in 1:length(polls)){
  if(length(polls[[i]]) == 5){
    polls[[i]]$NCAARank <- NA
    polls[[i]]$SportsNetworkRank <- NA
    polls[[i]]$'USA/ESPNRank' <- NA
    polls[[i]] <- polls[[i]][,c(1:4,6:8,5)]
  } else {
    polls[[i]]$APRank <- NA
    polls[[i]]$CoachRank <- NA
    polls[[i]] <- polls[[i]][,c(1,2,7,8,3:6)]
  }
}

pollsFin <- do.call("rbind", polls)
write.table(pollsFin, file = "/home/lobo/Desktop/cfbFootball/Data/Final/pollsD1AA.txt", 
            row.names = FALSE)