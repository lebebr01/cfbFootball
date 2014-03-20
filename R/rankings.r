library(XML)
library(plyr)
library(data.table)

#Division 1A
theurl <- "http://www.cfbdatawarehouse.com/data/div_ia_team_index.php"
doc = htmlParse(theurl)
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]//a")

tableNodes1 <- tableNodes[17:(length(tableNodes)-3)]

schools <- sapply(X = tableNodes1, FUN = xmlValue)

# Pulling polls Web Addresses
addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
addresses <- gsub("index.php$","rankings.php",addresses)

#Extracting official school name
library(doMC)
registerDoMC(cores = 2)
schoolsO <- foreach(t = 1:length(addresses), .combine="c") %do% {
  
  doc <- htmlParse(addresses[t])
  tableNodes <- getNodeSet(doc, "//table//table//tr//td//p//font//b")
  
  tb <- tableNodes[[1]]
  tb1 <- xmlValue(tb)
  substr(tb1, 10, nchar(tb1))   
}

rankings <- foreach(i = 1:length(addresses), .combine = "rbind") %dopar% {
  
  doc <- htmlParse(addresses[[i]])
  Nodes <- getNodeSet(doc, "//table//table//table//table")
  
  # extracting tables
  tabs <- lapply(c(4,6,8,10,12), function(x) 
    readHTMLTable(Nodes[[x]], stringsAsFactors = FALSE))
  
  # combining 5 tables
  tabsC <- do.call("rbind", tabs)
  
  # Change Names
  setnames(tabsC, c("Period", "Rank", "TotalPoints", "WinPctPoints", "SchedulePoints",
                    "NatChampPoints", "Big4BowlPoints"))
  
  # School names
  tabsC$Team <- schools[i]
  tabsC$offName <- schoolsO[i]
  
  tabsC
}

# writing table
write.csv(rankings, file = "/home/brandon/Dropbox/cfbFootball/Data/rankings.csv", 
          row.names = FALSE)
