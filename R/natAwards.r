library(XML)
library(data.table)

theurl <- "http://en.wikipedia.org/wiki/National_College_Football_Awards_Association"
doc <- htmlParse(theurl)
tableNodes <- getNodeSet(doc, "//table//tr//td//a")

tableNodes1 <- tableNodes[1:21]

awards <- sapply(X = tableNodes1, FUN = xmlValue)
## removing disney award and heisman (no data)
awards <- awards[-c(6, 10)]

##Pulling Award Web Addresses
addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.wikipedia.org", addresses, sep = "")
## removing disney award and heisman (no data)
addresses <- addresses[-c(6, 10)]

awardRecip <- vector("list", length(addresses))
whichVect <- c(2, 2, 4, 4, 3, 2, 3, 3, 3, 2, 3, 2, 2, , 3, 3, 2, 2, 2)
for(i in 1:length(addresses)){
  awardRecip[i] <- readHTMLTable(addresses[i], which = whichVect[i])
  sapply(awardRecip, "ncol")
}