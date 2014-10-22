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

##Pulling Bowl Web Addresses
addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
addresses <- gsub("index.php$","bowl_history.php",addresses)

#filenames <- paste("/home/lobo/Desktop/cfbFootball/Data/bowlD1A/", schools, ".txt", sep="")

##Extracting schools
library(data.table)
bowls <- foreach(t=1:length(addresses), .combine = "rbind") %dopar% {
  
  doc <- htmlParse(addresses[t])
  Nodes <- getNodeSet(doc, "//table//table//table//table")
  
  # extracting tables
  if(length(Nodes) >=3){
    tabs <- lapply(seq(3, length(Nodes), 1), function(x) 
      readHTMLTable(Nodes[[x]], stringsAsFactors = FALSE))
    
    # Combine tables
    bowl <- do.call("rbind", tabs)
    
    # Set colnames
    setnames(bowl, c("No", "WL", "Date", "PF", "Opponent", "PA", "Bowl"))
    
    bowl$Team <- schools[t]
    bowl$offName <- schoolsO[t]
    bowl
  }
}

write.csv(bowls, file = 'Data/bowlGames.csv')



## - Division I-AA - ##
theurl <- "http://www.cfbdatawarehouse.com/data/div_iaa_team_index.php"
doc = htmlParse(theurl)
tableNodes1 <- getNodeSet(doc, "//table//tr//td[position()=1]//a[contains(@href, 'div_iaa/')]")

schools <- sapply(X = tableNodes1, FUN = xmlValue)

##Pulling Bowl Web Addresses
addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
addresses <- gsub("index.php$","bowl_history.php",addresses)

##Extracting schools
for(t in 1:length(addresses)){
  
  doc <- htmlParse(addresses[t])
  (tableNodes <- getNodeSet(doc, "//table//table"))
  
  source("R/Bowl.R")
  
  tab$Team <- schools[t]
  tab <- subset(tab, Team == schools[t], select = c(2:length(tab)))
  
  ##Writing table
  write.table(x = tab
              , file = paste0("Data/bowlD1AA/"
                              , schools[t]
                              , ".txt")
              , row.names = FALSE)
}

filenames <- paste("Data/bowlD1AA/", schools, ".txt", sep="")

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
  bowls[[i]] <- read.table(file = filenames[i]
                           , header = TRUE
                           , stringsAsFactors=FALSE)
}

##Adding official team name to each team
lenBowls <- sapply(bowls, nrow)
for(i in 1:length(filenames)){
  bowls[[i]]$offName <- rep(schoolsO[i], lenBowls[i])
}
#combining all the teams into one file
bowlsD1AA <- do.call("rbind",bowls)

bowlsD1AA <- na.omit(bowlsD1AA)

write.table(bowlsD1AA
            , file = "Data/Final/bowlsD1AA.txt"
            , row.names=FALSE)

