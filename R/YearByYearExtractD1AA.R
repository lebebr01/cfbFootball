library(XML)

##Getting Team URLs
theurl <- "http://www.cfbdatawarehouse.com/data/div_iaa_team_index.php"

doc = htmlParse(theurl)
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]//a")

tableNodes1 <- tableNodes[17:(length(tableNodes)-1)]

schools <- sapply(X = tableNodes1, FUN = xmlValue)

##Pulling Bowl Web Addresses
teamAddr <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
teamAddr <- paste("http://www.cfbdatawarehouse.com/data/", teamAddr, sep = "")

total <- length(schools)
pb <- txtProgressBar(min=0, max=total, style = 3)
#tempFinal <- vector("list", length(teamAddr))
for(j in 1:length(teamAddr)){
  doc <- htmlParse(teamAddr[j])
  tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]//a")
  lenTB <- length(tableNodes)-1
  tableNodes1 <- tableNodes[17:lenTB]
  
  years <- sapply(X = tableNodes1, FUN = xmlValue)
  
  ##Extracting addresses
  yearAddr <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
  yearAddr1 <- gsub("index.php", "", teamAddr[j])
  yearAddr <- paste(yearAddr1, yearAddr, sep="")
  
  #setwd("/home/lobo/Desktop/cfbFootball")
  
  setwd("/home/lobo/Desktop/cfbFootball/Data/ybyD1AA")
  tbFinal <- vector("list", length(yearAddr))
  for(t in 1:length(yearAddr)) {
    doc <- htmlParse(yearAddr[t])
    Nodes <- getNodeSet(doc, "//table//table")
    
    source("/home/lobo/Desktop/cfbFootball/YearByYear.R")
    tbFinal[[t]] <- tbF
  }
  temp <- do.call("rbind",tbFinal)
  temp$school <- schools[j]
  write.table(temp, file = paste(schools[j], "txt", sep = "."), row.names = FALSE)
  setTxtProgressBar(pb, j)
}

tempFinal <- vector("list", length(schools))
for(i in 1:length(schools)){
  tempFinal[[i]] <- read.table(paste(schools[i], "txt", sep = "."), header = TRUE)
}

YearByYear <- do.call("rbind", tempFinal)
write.table(YearByYear, file="/home/lobo/Desktop/cfbFootball/Data/Final/YearByYearD1AA.txt", row.names=FALSE)