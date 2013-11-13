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
	
# Write to external file
write.table(x = career, file = "Data/Final/CoachCareer.txt", row.names = FALSE)

#-----------------------
# Win/loss into one table
coachWinLoss <- do.call("rbind", coachWL)

# Write to file
write.table(x = coachWinLoss, file = "Data/Final/CoachWL.txt", row.names = FALSE)
