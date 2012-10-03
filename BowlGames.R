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
schoolsO <- foreach(t=1:length(addresses), .combine="rbind") %dopar% {

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
bowlsD1A <- do.call("rbind",bowls)
write.table(bowlsD1A, file = "/home/lobo/Desktop/cfbFootball/Data/Final/bowlsD1A.txt",
            row.names = FALSE)

##function to extract conference affiliation
getTable <- function(u) {
   table <- readHTMLTable(u)
   table <- table[[length(table)-3]]
   table <- as.character(table[2:length(table[,1]),])
   years <- substr(table,1,9)
   conference <- substr(table, 12, nchar(table))
   conf.table <- data.frame(years,conference)
   conf.table$years <- sub("-",":",conf.table$years)
   conf.table$years <- sub("XX", "12", conf.table$years)
   conf.table
}

#library(doMC)
#registerDoMC(cores=2)
confTable <- foreach(k=1:length(filenames)) %dopar% {
    getTable(addresses[k])
}
confTable[[101]][3,1] <- "1904:1977"
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
  colnames(exp.confTable) <- "Conference"
  exp.confTable <- cbind(Years=unlist(exp.years), exp.confTable)
  exp.confTable$offName <- rep(schoolsO[t], nrow(exp.confTable))
  exp.confTable
}

write.table(exp.confTable, file = "/home/lobo/Desktop/cfbFootball/Data/Final/confAffilD1A.txt", 
            row.names=FALSE)


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

##Conference Affiliation - D1-AA
confTable <- foreach(k=1:length(filenames)) %dopar% {
  getTable(addresses[k])
}
confTable[[34]][c(1,4),] <- NA
confTable[[62]][c(1,5),] <- NA
confTable[[119]][1,] <- NA

confTable <- lapply(confTable, na.omit)
exp.confTable <- foreach(t =1:length(confTable), .combine="rbind") %dopar% {
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
  exp.confTable
}

write.table(exp.confTable, file = "/home/lobo/Desktop/cfbFootball/Data/Final/confAffilD1AA.txt",
            row.names=FALSE)