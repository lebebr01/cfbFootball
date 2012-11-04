library(XML)
library(plyr)

#Division 1A
theurl <- "http://www.cfbdatawarehouse.com/data/div_ia_team_index.php"
doc = htmlParse(theurl)
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]//a")

tableNodes1 <- tableNodes[17:(length(tableNodes)-1)]

schools <- sapply(X = tableNodes1, FUN = xmlValue)

##Pulling Bowl Web Addresses
addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
addresses <- gsub("index.php$","bowl_history.php",addresses)

filenames <- paste("/home/lobo/Desktop/cfbFootball/Data/bowlD1A/", schools, ".txt", sep="")


addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
#addresses <- gsub("index.php$","bowl_history.php",addresses)
#addresses <- addresses[-50]

#Extracting official school name
library(doMC)
registerDoMC(cores=2)
schoolsO <- foreach(t=1:length(addresses), .combine="rbind") %dopar% {
  
  doc <- htmlParse(addresses[t])
  tableNodes <- getNodeSet(doc, "//table//table//tr//td//p//font//b")
  
  tb <- tableNodes[[1]]
  tb1 <- xmlValue(tb)
  substr(tb1, 10, nchar(tb1))   
}


##function to extract conference affiliation
getTableC <- function(u) {
  table <- readHTMLTable(u)
  tableC <- table[[length(table)-2]]
  tableC <- as.character(tableC[2:length(tableC[,1]),])
  yearsC <- substr(tableC, 1, 9)
  conference <- substr(tableC, 12, nchar(tableC))
  conf.table <- data.frame(yearsC, conference)
  conf.table$years <- sub("-",":",conf.table$years)
  conf.table$years <- sub("XX", "12", conf.table$years)
  conf.table
}
#Function to extract division affiliation
getTableD <- function(u){
  table <- readHTMLTable(u)
  tableD <- table[[length(table)-3]]
  tableD <- as.character(tableD[2:length(tableD[,1]),])
  yearsD <- substr(tableD,1,9)
  division <- substr(tableD, 12, nchar(tableD))
  div.table <- data.frame(yearsD,division)
  div.table$years <- sub("-",":",div.table$years)
  div.table$years <- sub("XX", "12", div.table$years)
  div.table
}

#library(doMC)
#registerDoMC(cores=2)
divTable <- foreach(k=1:length(filenames)) %dopar% {
  getTableD(addresses[k])
}
divTable[[101]][3,3] <- "1904:1977"
#confTable[[101]][4,1] <- ""

exp.divTable <- foreach(t =1:length(divTable), .combine="rbind") %do% {
  yrs <- as.numeric(unlist(strsplit(divTable[[t]]$years,":")))
  Years <- matrix(yrs, ncol=2, nrow=length(yrs)/2, byrow=TRUE)
  nyrs <- Years[,2]-Years[,1]+1
  
  exp.years <- vector("list", length(nyrs))
  for(k in 1:length(nyrs)) {
    exp.years[[k]] <- Years[k,1]:Years[k,2]
  }
  
  exp.divTable <- data.frame(rep(divTable[[t]][,2], nyrs))
  colnames(exp.divTable) <- "Division"
  exp.divTable <- cbind(Years=unlist(exp.years), exp.divTable)
  exp.divTable$offName <- rep(schoolsO[t], nrow(exp.divTable))
  exp.divTable$school <- rep(schools[t], nrow(exp.divTable))
  exp.divTable
}


confTable <- foreach(k=1:length(filenames)) %dopar% {
  getTableC(addresses[k])
}
#confTable[[101]][3,1] <- "1904:1977"
#confTable[[101]][4,1] <- ""

exp.confTable <- foreach(t =1:length(confTable), .combine="rbind") %dopar% {
  yrs <- as.numeric(unlist(strsplit(confTable[[t]]$years,":")))
  Years <- matrix(yrs, ncol=2, nrow=length(yrs)/2, byrow=TRUE)
  nyrs <- Years[,2]-Years[,1]+1
  
  exp.years <- vector("list", length(nyrs))
  for(k in 1:length(nyrs)) {
    exp.years[[k]] <- Years[k,1]:Years[k,2]
  }
  
  exp.confTable <- data.frame(rep(confTable[[t]][,2], nyrs))
  colnames(exp.confTable) <- "conference"
  exp.confTable <- cbind(Years=unlist(exp.years), exp.confTable)
  exp.confTable$offName <- rep(schoolsO[t], nrow(exp.confTable))
  exp.confTable$school <- rep(schools[t], nrow(exp.confTable))
  exp.confTable
}

exp.confTable <- join(exp.confTable, exp.divTable, by = c("Years", "school"))

write.table(exp.confTable, file = "/home/lobo/Desktop/cfbFootball/Data/Final/confAffilD1A.txt", 
            row.names=FALSE)

###Division 1-AA
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

##Conference Affiliation - D1-AA
divTable <- foreach(k=1:length(filenames)) %do% {
  getTableD(addresses[k])
}
divTable[[34]][c(1,4),] <- NA
divTable[[62]][c(1,5),] <- NA
divTable[[119]][c(1,6),] <- NA

exp.divTable <- foreach(t =1:length(divTable), .combine="rbind") %dopar% {
  divTable[[t]] <- na.omit(divTable[[t]])
  yrs <- as.numeric(unlist(strsplit(divTable[[t]]$years,":")))
  Years <- matrix(yrs, ncol=2, nrow=length(yrs)/2, byrow=TRUE)
  nyrs <- Years[,2]-Years[,1]+1
  
  exp.years <- vector("list", length(nyrs))
  for(k in 1:length(nyrs)) {
    exp.years[[k]] <- Years[k,1]:Years[k,2]
  }
  
  exp.divTable <- data.frame(rep(divTable[[t]][,2], nyrs))
  colnames(exp.divTable) <- "Division"
  exp.divTable <- cbind(Years=unlist(exp.years), exp.divTable)
  exp.divTable$offName <- rep(schoolsO[t], nrow(exp.divTable))
  exp.divTable$school <- rep(schools[t], nrow(exp.divTable))
  exp.divTable
}


confTable <- foreach(k=1:length(filenames)) %do% {
  getTableC(addresses[k])
}
confTable[[65]][7,] <- NA

confTable <- lapply(confTable, na.omit)
exp.confTable <- foreach(t =1:length(confTable), .combine="rbind") %dopar% {
  confTable[[t]] <- na.omit(confTable[[t]])
  yrs <- as.numeric(unlist(strsplit(confTable[[t]]$years,":")))
  Years <- matrix(yrs, ncol=2, nrow=length(yrs)/2, byrow=TRUE)
  nyrs <- Years[,2]-Years[,1]+1
  
  exp.years <- vector("list", length(nyrs))
  for(k in 1:length(nyrs)) {
    exp.years[[k]] <- Years[k,1]:Years[k,2]
  }
  
  exp.confTable <- data.frame(rep(confTable[[t]][,2], nyrs))
  colnames(exp.confTable) <- "Conference"
  exp.confTable <- cbind(Years=unlist(exp.years), exp.confTable)
  exp.confTable$offName <- rep(schoolsO[t], nrow(exp.confTable))
  exp.confTable$school <- rep(schools[t], nrow(exp.confTable))
  exp.confTable
}

exp.confTable <- join(exp.confTable, exp.divTable, by = c("Years", "offName"))

write.table(exp.confTable, file = "/home/lobo/Desktop/cfbFootball/Data/Final/confAffilD1AA.txt",
            row.names=FALSE)