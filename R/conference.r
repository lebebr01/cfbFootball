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

xx <- function(x){
  as.numeric(x[1]):as.numeric(x[2])
}

#library(doSNOW)
#registerDoSNOW(makeCluster(2, type = "SOCK"))
library(doMC)
registerDoMC(cores = 2)
conference <- foreach(t = 1:length(addresses), .combine = "rbind",
                      .packages = c("XML", "plyr", "data.table")) %dopar% {
  
  doc <- htmlParse(addresses[t])
  Nodes <- getNodeSet(doc, "//table//table//table")
  
  conf <- readHTMLTable(Nodes[[max(length(Nodes))]], stringsAsFactors = FALSE)
  conf <- gsub("XX", "13", conf[,1])
  
  years <- strsplit(substr(conf, 1, 9), '-')
  years <- lapply(years, xx)
  
  conf1 <- substr(conf, 12, nchar(conf))
  
  conf <- data.frame(unlist(years), rep(conf1, sapply(years, length)), schools[t])  
  setnames(conf, c('Year', 'Conference', 'Team'))
  conf
}

# save file - assumes in root of working directory structure.
write.csv(conference, file = paste(getwd(), '/Data/conference.csv', sep = ''),
          row.names = FALSE)
