# Load necessary libraries
library(XML)

# Extract the name of each head coach
theurl <- "http://en.wikipedia.org/wiki/List_of_current_NCAA_Division_I_FBS_football_coaches"
doc = htmlParse(theurl)
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=3]//span[@class='fn']//a")
coaches <- sapply(X = tableNodes, FUN = xmlValue)
# Remove a coach that does not have a wikipedia page
coaches2 <- coaches[-48]

# Extract the Wikipedia address for each head coach
addresses <- unlist(sapply(X = tableNodes, FUN = xmlGetAttr, "href"))
addresses <- paste("http://www.wikipedia.org", addresses, sep = "")
# Remove a coach that does not have a wikipedia page
addresses <- addresses[-48]

# Extract school names
tableNodes <- getNodeSet(doc, "//table//tr//td[position()=1]")
schools <- sapply(X = tableNodes, FUN = xmlValue)

# Create empty vectors to hold scrapes
coachCareer <- vector("list", length(coaches2))
coachPlay <- vector("list", length(coaches2))
coachWL <- vector("list", length(coaches2))

# Loop to extract coach Career, playing career, and head coach season records
#for (t in 1:length(coaches2)){
for(t in 1:length(coaches2)){
	
	tables <- readHTMLTable(addresses[t])
	
	# Get playing career
	coachPlay[t] <- source("R/Get-Wikipedia-Playing-Career.R")	
  #coachPlay[[t]]$name <- coaches2[t]

	# Get coaching career
	coachCareer[t] <- source("R/Get-Wikipedia-Coaching-Career.R")
	#coachCareer[[t]]$name <- coaches2[t]

  # Get bottom wikipedia table, win loss records
  coachWL[t] <- source("R/Get-Wikipedia-WinLoss.R")
  #coachWL[[t]]$name <- coaches2[t]

}

#--------------------------------------
# collapse coachCareer and coachPlay
coachCareer2 <- do.call("rbind", coachCareer)
coachPlay2 <- do.call("rbind", coachPlay)

# Combine playing and coaching careers into a single data frame
career <- rbind(coachCareer2, coachPlay2)

# fix sonny Dykes
career$years <- ifelse(career$years == "Football", '1994', career$years)
career$years <- ifelse(career$years == "Baseball", '1989-1993', career$years)

# filling in missing first two digits of years
career$years <- ifelse((grepl("\\D[0-9]{2}$", career$years) == TRUE & 
                          substr(career$years, 6, nchar(career$years)) < 10),
                       paste(substr(career$years, 1, 5), '20', substr(career$years, 6, nchar(career$years)), sep = ''),
                       ifelse((grepl("\\D[0-9]{2}$", career$years) == TRUE & 
                                substr(career$years, 6, nchar(career$years)) > 10),
                       paste(substr(career$years, 1, 5), '19', substr(career$years, 6, nchar(career$years)), sep = ''),
                              career$years))

# extract the beginning and end of each year cycle
years <- gsub("\\D", ":", career$years)

years2 <- strsplit(career$years, "\\D")
numyears <- matrix(nrow = length(years2), ncol = 1)
for(tt in 1:length(years2)){
  if(is.na(years2[[tt]][2])){
    numyears[tt] <- 1
  } else {
   numyears[tt] <-  as.numeric(years2[[tt]][2]) - as.numeric(years2[[tt]][1]) + 1
  }
}

# expand career to one row per year
index <- rep(seq_len(nrow(career)), numyears)
career2 <- career[index,]

# create new year variable
yearsVec <- vector("list", length(years2))
for(ii in 1:length(years2)){
  if(length(years2[[ii]]) == 2){
    yearsVec[[ii]] <- seq(from = as.numeric(years2[[ii]][1]), to = as.numeric(years2[[ii]][2]),
                          by = 1)
  } else {
    yearsVec[[ii]] <- as.numeric(years2[[ii]][1])
  }
}
career2$year <- as.numeric(unlist(yearsVec))
	
# Write to external file
write.csv(x = career2, file = "Data/Final/CoachCareer.csv", row.names = FALSE)

#-----------------------
# Win/loss into one table
coachWinLoss <- do.call("rbind", coachWL)

# Write to file
write.csv(x = coachWinLoss, file = "Data/Final/CoachWL.csv", row.names = FALSE)
