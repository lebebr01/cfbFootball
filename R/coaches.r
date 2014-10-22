library(XML)
library(plyr)
library(data.table)

#Division 1A
theurl <- "http://www.cfbdatawarehouse.com/data/div_ia_team_index.php"
doc = htmlParse(theurl)
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]//a")

tableNodes1 <- tableNodes[18:(length(tableNodes)-3)]

schools <- sapply(X = tableNodes1, FUN = xmlValue)

# Pulling polls Web Addresses
addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
addresses <- gsub("index.php$","coaching_records.php",addresses)

#library(doMC)
#registerDoMC(cores=2)
library(doSNOW)
registerDoSNOW(makeCluster(2, type = "SOCK"))
coachAddr <- foreach(t = 1:length(addresses), .packages = c("XML", "plyr", "data.table")) %dopar% {
  doc <- htmlParse(addresses[t])
  tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]//a")
  
  lenTB <- grep(".*alltime_coach_year_by_year.*", unlist(sapply(X = tableNodes, FUN = xmlGetAttr, "href")))
  tableNodes1 <- tableNodes[lenTB]
  
  coaches <- sapply(X = tableNodes1, FUN = xmlValue)
  
  ##Extracting addresses
  cAddr <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
  cAddr1 <- sapply(1:length(cAddr), function(x) gsub("^.*data", "http://www.cfbdatawarehouse.com/data", cAddr[x]))
  
  # combine into data.frame
  cbind(coaches, cAddr1)
}

coaches <- foreach(t = 1:length(coachAddr), .combine = "rbind") %:%
  foreach(i = 1:length(coachAddr[[t]][,2]), .combine = "rbind", .errorhandling = "remove", .packages = c("XML", "plyr", "data.table")) %dopar% {
    
    doc <- htmlParse(coachAddr[[t]][i,2])
    Nodes <- getNodeSet(doc, "//table//table//table//table")
    
    coachtab <- readHTMLTable(Nodes[[3]], stringsAsFactors = FALSE)
    
    setnames(coachtab, c("Year", "Team", "Win", "Loss", "Tie", "Pct", "PF", "PA", "Delta"))
    coachtab$coach <- coachAddr[[t]][i,1]
    coachtab
  }

#write.csv(coaches, file = "/home/brandon/Dropbox/cfbFootball/Data/coaches.csv", 
write.csv(coaches, file = "C:/Users/brandonl/Dropbox/cfbFootball/Data/coaches.csv",
          row.names = FALSE)
