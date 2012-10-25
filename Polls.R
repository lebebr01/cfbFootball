library(XML)
library(plyr)
library(stringr)

#Division 1A
theurl <- "http://www.cfbdatawarehouse.com/data/div_ia_team_index.php"
doc = htmlParse(theurl)
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]//a")

tableNodes1 <- tableNodes[17:(length(tableNodes)-1)]

schools <- sapply(X = tableNodes1, FUN = xmlValue)

##Pulling Bowl Web Addresses
addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
addresses <- gsub("index.php$","in_the_polls.php",addresses)


polls <- vector("list", length(addresses))
for(i in 1:length(addresses)){
  temp <- readHTMLTable(addresses[i])
  temp2 <- na.omit(temp[[16]][,2:ncol(temp[[16]])])
  for(j in 1:ncol(temp2)){
    temp2[,j] <- as.character(temp2[,j])
  }
  names(temp2) <- gsub("\\s", "", temp2[1,])
  temp2 <- temp2[-1,]
  temp2$school <- schools[i]
  polls[[i]] <- temp2
}

pollsFin <- do.call("rbind", polls)
write.table(pollsFin, file = "/home/lobo/Desktop/cfbFootball/Data/Final/polls.txt", 
            row.names = FALSE)



##Division 1AA
theurl <- "http://www.cfbdatawarehouse.com/data/div_iaa_team_index.php"
doc = htmlParse(theurl)
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]//a")

tableNodes1 <- tableNodes[18:length(tableNodes)-1]

schools <- sapply(X = tableNodes1, FUN = xmlValue)

##Pulling Bowl Web Addresses
addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
addresses <- gsub("index.php$","in_the_polls.php",addresses)

polls <- vector("list", length(addresses))
for(i in 1:2){
  temp <- readHTMLTable(addresses[i])
  temp2 <- na.omit(temp[[16]][,2:ncol(temp[[16]])])
  if(length(temp2) == 1){
    next 
  } else{
  for(j in 1:ncol(temp2)){
    temp2[,j] <- as.character(temp2[,j])
  }
  names(temp2) <- gsub("\\s", "", temp2[1,])
  temp2 <- temp2[-1,]
  temp2$school <- schools[i]
  polls[[i]] <- temp2
 }
}  

pollsFin <- do.call("rbind", polls)
write.table(pollsFin, file = "/home/lobo/Desktop/cfbFootball/Data/Final/polls.txt", 
            row.names = FALSE)