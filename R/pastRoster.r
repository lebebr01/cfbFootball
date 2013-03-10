library(XML)
library(data.table)

theurl <- "http://www.cbssports.com/collegefootball/teams"
doc = htmlParse(theurl)
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]//a")

tableNodes1 <- tableNodes[6:length(tableNodes)]

schools <- sapply(X = tableNodes1, FUN = xmlValue)

##Pulling Bowl Web Addresses
addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cbssports.com", addresses, sep = "")
addresses <- gsub("page","roster",addresses)

roster <- vector("list", length(addresses))
for(i in 1:length(addresses)){
  roster[[i]] <- readHTMLTable(addresses[i], which = 5, skip.rows = 1:2)
  setnames(roster[[i]], c('no', 'player', 'pos', 'ht', 'wt', 'class', 'hometown'))
  roster[[i]]$team <- schools[i]
}

rosterComb <- do.call("rbind", roster)

for(i in 1:ncol(rosterComb)){
  rosterComb[,i] <- as.character(rosterComb[,i])
}

rosterComb <- subset(rosterComb, no != "null")

write.table(rosterComb, file = "/home/lobo/Dropbox/cfbFootball/Data/Final/pastroster.txt", 
            row.names = FALSE)