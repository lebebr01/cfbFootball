## Load necessary libraries
library(XML)

## Extract the name of each head coach
theurl <- "http://en.wikipedia.org/wiki/List_of_current_NCAA_Division_I_FBS_football_coaches"
doc = htmlParse(theurl)
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=3]//span[@class='fn']//a")
coaches <- sapply(X = tableNodes, FUN = xmlValue)
coaches2 <- c(coaches[1:115], "Sonny Dykes", coaches[116:119])

## Extract the Wikipedia address for each head coach
addresses <- unlist(sapply(X = tableNodes, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.wikipedia.org", addresses, sep = "")
addresses <- c(addresses[1:115], "http://www.wikipedia.org/wiki/Sonny_Dykes", addresses[116:119])

addresses[9] <- "http://www.wikipedia.org/wiki/Tom_O'Brien_(American_football)"
addresses[53] <- "http://www.wikipedia.org/wiki/George_O'Leary"

## Extract school names
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]")
schools <- sapply(X = tableNodes, FUN = xmlValue)
filenames <- paste("/home/lobo/Desktop/football/winloss/", coaches2, ".txt", sep="")

#theURL <- "http://www.wikipedia.org/wiki/Frank_Spaziani"
for (t in 89:120){
	
	tables <- readHTMLTable(addresses[t])
	
	## Get playing career
	#source("C:/Users/lobo/Desktop/football/Get-Wikipedia-Playing-Career.R")	
	
	## Write to external file
		
	#write.table(x = playing, file = filenames[i], row.names = FALSE)
	
	#}
	
	## Get coaching career
	#source("C:/Users/lobo/Desktop/football/Get-Wikipedia-Coaching-Career.R")
	
	#write.table(x = coaching, file = filenames[i], row.names = FALSE)

      ## Get bottom wikipedia table, win loss records
      source("/home/lobo/Desktop/football/Get-Wikipedia-WinLoss.R")

      hCoach$Name <- coaches2[88]
      write.table(x = hCoach, file = filenames[88], row.names=FALSE)
}

	## Combine playing and coaching careers into a single data frame
	career <- rbind(playing, coaching)
	
	
	## Write to external file
		
	write.table(x = career, file = filenames[i], row.names = FALSE)

	}
