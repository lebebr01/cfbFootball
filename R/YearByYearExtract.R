library(XML)

##Getting Team URLs
theurl <- "http://www.cfbdatawarehouse.com/data/div_ia_team_index.php"

doc = htmlParse(theurl)
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]//a")

tableNodes1 <- tableNodes[20:length(tableNodes)-3]

schools <- sapply(X = tableNodes1, FUN = xmlValue)

addresses <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.cfbdatawarehouse.com/data/", addresses, sep = "")
#addresses <- gsub("index.php$","bowl_history.php",addresses)
#addresses <- addresses[-50]

#Extracting official school name
library(doMC)
registerDoMC(cores = 2)
schoolsO <- foreach(t=1:length(addresses), .combine="c") %do% {
  
  doc <- htmlParse(addresses[t])
  tableNodes <- getNodeSet(doc, "//table//table//tr//td//p//font//b")
  
  tb <- tableNodes[[1]]
  tb1 <- xmlValue(tb)
  substr(tb1, 10, nchar(tb1))   
}

##Pulling Bowl Web Addresses
teamAddr <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
teamAddr <- paste("http://www.cfbdatawarehouse.com/data/", teamAddr, sep = "")

#tempFinal <- vector("list", length(teamAddr))
yearAddr <- foreach(j=1:length(teamAddr)) %dopar% {
  doc <- htmlParse(teamAddr[j])
  tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]//a")
  
  lenTB <- grep(".*yearly_results.*", unlist(sapply(X = tableNodes, FUN = xmlGetAttr, "href")))
  tableNodes1 <- tableNodes[lenTB]
  
  years <- sapply(X = tableNodes1, FUN = xmlValue)
  
  ##Extracting addresses
  yearAddr <- unlist(sapply(X = tableNodes1, FUN = xmlGetAttr, "href"))
  yearAddr1 <- gsub("index.php", "", teamAddr[j])
  yearAddr <- paste(yearAddr1, yearAddr, sep="")
  yearAddr
}
 
library(data.table)
yearly <- foreach(j = 1:length(yearAddr), .combine = "rbind") %:%
  foreach(t = 1:length(yearAddr[[j]]), .combine = "rbind", .errorhandling = 'remove') %dopar% {
    
    doc <- htmlParse(yearAddr[[j]][t])
    Nodes <- getNodeSet(doc, "//table//table//table//table")
    
    # Extracting years
    years <- lapply(3:length(Nodes), function(x) readHTMLTable(Nodes[[x]], stringsAsFactors = FALSE))
        
    # combine
    yearC <- do.call("rbind", years)
    
    # year of season
    yearC$Year <- do.call("c", lapply(1:length(years), function(x) as.numeric(rep(substr(years[[x]][1,2], 
                                                  nchar(years[[x]][1,2])-3, nchar(years[[x]][1,2])),
                                                                    length(years[[x]][,2])))))
    
    # change names
    setnames(yearC, c("WL", "Date", "PF", "Opponent", "PA", "Location", "Notes", "Year"))
    
    yearC$Team <- schools[j]
    yearC$offName <- schoolsO[j]
    
    yearC
  }


write.csv(yearly, file="/home/brandon/Dropbox/cfbFootball/Data/YearByYearD1.csv", row.names=FALSE)


setwd("/home/lobo/Desktop/football/Data/Final")
yby <- read.table("YearByYear.txt", header=T)
schoolName <- read.table("SchoolNames.txt", header=T)

yby <- merge(yby, schoolName, by = "school", all=TRUE, sort=TRUE)

write.table(yby, row.names=FALSE, file="YearByYear-2012-02-27.csv")

##Merging yby file with big football file
setwd("/home/lobo/Desktop/football/Data/Final")
yby <- read.table("YearByYear-2012-02-27.txt", header=T)
colnames(yby) <- c("school", "WLyby", "DateYBY", "PFyby", "OpponentYBY", "PAyby", "LocationYBY", "Notes",
    "MonthYBY", "DayYBY", "Year", "Team")

cfb <- read.csv("College-Football-2012-03-09.csv")

##Changing names in yby
yby$Team <- gsub("Indiana University Bloomington", "Indiana University",
                         yby$Team)
yby$Team <- gsub("Louisiana State University and Agricultural and Mechanical College",
                         "Louisiana State University", yby$Team)
yby$Team <- gsub("Miami University: Oxford Campus",
                         "Miami University", yby$Team)
yby$Team <- gsub("Ohio State University: Columbus Campus",
                         "Ohio State University", yby$Team)
yby$Team <- gsub("Rutgers, State University of New Jersey: Rutgers College",
                         "Rutgers University", yby$Team)
yby$Team <- gsub("State University of New York at Buffalo", 
                         "University at Buffalo", yby$Team)
yby$Team <- gsub("The University of Alabama", "University of Alabama",
                         yby$Team)
yby$Team <- gsub("The University of Alabama at Birmingham",
                         "University of Alabama at Birmingham", yby$Team)
yby$Team <- gsub("University of Hawaii at Manoa", "University of Hawaii",
                         yby$Team)
yby$Team <- gsub("University of Illinois at Urbana-Champaign",
                         "University of Illinois", yby$Team)
yby$Team <- gsub("University of Maryland: College Park",
                         "University of Maryland", yby$Team)
yby$Team <- gsub("University of Minnesota: Twin Cities",
                         "University of Minnesota", yby$Team)
yby$Team <- gsub("University of Missouri: Columbia",
                         "University of Missouri", yby$Team)
yby$Team <- gsub("University of Nebraska - Lincoln",
                         "University of Nebraska", yby$Team)
yby$Team <- gsub("University of North Carolina at Chapel Hill",
                         "University of North Carolina", yby$Team)
yby$Team <- gsub("University of South Carolina-Columbia",
                         "University of South Carolina", yby$Team)
yby$Team <- gsub("University of Tennessee: Knowville",
                         "University of Tennessee", yby$Team)
yby$Team <- gsub("The University of Texas at Austin",
                         "University of Texas", yby$Team)
yby$Team <- gsub("University of Texas-El Paso",
                         "University of Texas at El Paso", yby$Team)
yby$Team <- gsub("University of Wisconsin-Madison",
                         "University of Wisconsin", yby$Team)
yby$Team <- gsub("Virginia Polytechnic Institute & State University",
                         "Viginia Polytechnic Institute and State University",
                         yby$Team)
yby$Team <- factor(yby$Team)

##Adding Number of games per year to yby
library(plyr)
splitVars <- c("Team", "Year")
tempYBY <- ddply(.data = yby, .variables = splitVars, .fun = "nrow")

NumGames <- vector("list", length(tempYBY[,3]))
for(i in 1:length(tempYBY[,3])){
  NumGames[[i]] <- 1:tempYBY[i,3]
}
NumG <- do.call("c", NumGames)

yby <- yby[order(yby$Team),]
yby$NumGames <- NumG    

cfbyby <- join(yby, cfb, by = c("Team", "Year"), type = "left", match="all")

write.csv(cfbyby, file = "cfbMerge-2012-03-09.csv", row.names=FALSE)