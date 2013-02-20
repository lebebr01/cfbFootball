######################
#Points are only provided for the top 50 teams overall. This is rerun for each year and then merged with the other data.
######################
library(XML)

#years to loop over
year <- 2002:2013

#Loop on year
for(m in 1:12){
  
  theurl <- paste("http://rivals.yahoo.com/footballrecruiting/football/recruiting/teamrank",year[m],"all/all", sep="/")
  htmltable <- readHTMLTable(theurl)[[2]]
  
  #turning all variables to character variables
  for(i in 1:ncol(htmltable)){
    htmltable[,i] <- as.character(htmltable[,i])
  } #end column looping
  
  #turning ranking and number of recruits to numeric
  for(i in 3:ncol(htmltable)){
    htmltable[,i] <- as.numeric(htmltable[,i])
  } #end column looping
  
  #removing pound sign in ranking
  htmltable$Rank <- gsub("#", "", htmltable$Rank)
  
  #adding year
  htmltable$Year <- year[m]
  
  names(htmltable) <- c("RivalsRank", "School", "Total", "5-Star", "4-Star", "3-Star", 
                        "AvgStar", "Points", "Year")
    
  #merging years
  if (m == 1) y <- htmltable else y <- merge(y, htmltable, all = TRUE)
  
}

write.csv(y, file = "C:/Users/e520062/Desktop/cfbFootball/Data/rivals.csv")