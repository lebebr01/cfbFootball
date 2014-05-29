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

library(doSNOW)
registerDoSNOW(makeCluster(2, type = "SOCK"))
conference <- foreach(t = 1:length(addresses), .combine = "rbind",
                      .packages = c("XML", "plyr", "data.table")) %dopar% {
  
  doc <- htmlParse(addresses[t])
  Nodes <- getNodeSet(doc, "//table//table//table")
  
  conf <- readHTMLTable(Nodes[[max(length(Nodes))]], stringsAsFactors = FALSE)
  
  conf
}
