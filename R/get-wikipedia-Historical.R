## Load necessary libraries
library(XML)

## Extract the name of each head coach
#theurl <- "http://en.wikipedia.org/wiki/List_of_college_football_coaches_with_200_wins"
#doc = htmlParse(theurl)
#tableNodes <- getNodeSet(doc, "//table//tr//td[position()=2]//span[@class='fn']//a")
#coaches <- sapply(X = tableNodes, FUN = xmlValue)
#coaches2 <- coaches[c(1:2,4:8,10,12,16:17,20,26,29,31,33:34,36,52,61:62)]

## Extract the Wikipedia address for each head coach
#addresses <- unlist(sapply(X = tableNodes, FUN = xmlGetAttr, "href"))
#addresses <- paste("http://www.wikipedia.org", addresses, sep = "")
#addresses <- addresses[c(1:2,4:8,10,12,16:17,20,26,29,31,33:34,36,52,61:62)]


## Extract school names
#tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]")
#schools <- sapply(X = tableNodes, FUN = xmlValue)
coaches2 <- read.table("/home/lobo/Desktop/football/HCoachNames.txt")
coaches2 <- coaches2[,1]
filenames <- paste("/home/lobo/Desktop/football/HWin/", coaches2, ".txt", sep="")

#theURL <- "http://www.wikipedia.org/wiki/Frank_Spaziani"
for (i in 1:21){
	
	
	
	tables <- readHTMLTable(addresses[19])

	
	
	## Get playing career
	#source("C:/Users/lobo/Desktop/football/Get-Wikipedia-Playing-Career.R")
	
	
	## Write to external file
		
	#write.table(x = playing, file = filenames[i], row.names = FALSE)
	
	#}
	
	## Get coaching career
	#source("C:/Users/lobo/Desktop/football/Get-Wikipedia-Coaching-Career.R")
	
	#write.table(x = coaching, file = filenames[i], row.names = FALSE)


      ## Get bottom wikipedia table, win loss records
      source("C:/Users/lobo/Desktop/football/Get-Wikipedia-WinLoss.R")
      
      hCoach$Name <- coaches2[19]
      write.table(x = hCoach, file = filenames[19], row.names=FALSE)
}

	## Combine playing and coaching careers into a single data frame
	career <- rbind(playing, coaching)
	
	
	## Write to external file
		
	write.table(x = career, file = filenames[i], row.names = FALSE)

	}
