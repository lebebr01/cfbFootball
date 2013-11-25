##Setting Working Directory
setwd("/home/lobo/Desktop/football/Data/Final")

##Reading in Datasets
footballData <- read.csv("College-Football-2011-12-30.csv")
bowlsFin <- read.table("bowlsFin.txt", header=TRUE)
bowlsFinAA <- read.table("bowlsFinAA.txt", header=TRUE)

bowlsFinAAm <- na.omit(bowlsFinAA)

##Renaming official Team Names in footballData
footballData$Team <- gsub("California State University-Fresno", "California State Univerity: Fresno",
                          footballData$Team)
footballData$Team <- gsub("Texas A & M University", "Texas A&M University",
                          footballData$Team)
footballData$Team <- gsub("United States Military Academy (Army)",
                          "United States Military Academy", footballData$Team)
footballData$Team <- gsub("University of California-Berkeley", 
                          "University of California: Berkeley", footballData$Team)
footballData$Team <- gsub("University of California-Los Angeles",
                          "University of California: Los Angeles", footballData$Team)
footballData$Team <- gsub("University of Nevada", "University of Nevada: Reno",
                          footballData$Team)
footballData$Team <- gsub("University of Nevada-Las Vegas",
                         "University of Nevada: Las Vegas", footballData$Team)
footballData$Team <- gsub("Bethune-Cookman University", "Bethune-Cookman College",
                          footballData$Team)
footballData$Team <- gsub("Bryant University", "Bryant College", footballData$Team)
footballData$Team <- gsub("California Polytechnic State University-San Luis Obispo",
                          "California Polytechnic State University: San Luis Obispo",
                          footballData$Team)
footballData$Team <- gsub("California State University-Sacramento",
                          "California State University: Sacramento", footballData$Team)
footballData$Team <- gsub("Robert Morris University", "Robert Morris College",
                          footballData$Team)
footballData$Team <- gsub("University of California-Davis",
                          "University of California: Davis", footballData$Team)
footballData$Team <- gsub("University of Tennessee-Martin", 
                          "University of Tennessee: Martin", footballData$Team)
footballData$Team <- factor(footballData$Team)

##Changing official Team Names in bowlsFin
bowlsFin$offName <- gsub("Indiana University Bloomington", "Indiana University",
                         bowlsFin$offName)
bowlsFin$offName <- gsub("Louisiana State University and Agricultural and Mechanical College",
                         "Louisiana State University", bowlsFin$offName)
bowlsFin$offName <- gsub("Miami University: Oxford Campus",
                         "Miami University", bowlsFin$offName)
bowlsFin$offName <- gsub("Ohio State University: Columbus Campus",
                         "Ohio State University", bowlsFin$offName)
bowlsFin$offName <- gsub("Rutgers, State University of New Jersey: Rutgers College",
                         "Rutgers University", bowlsFin$offName)
bowlsFin$offName <- gsub("State University of New York at Buffalo", 
                         "University at Buffalo", bowlsFin$offName)
bowlsFin$offName <- gsub("The University of Alabama", "University of Alabama",
                         bowlsFin$offName)
bowlsFin$offName <- gsub("The University of Alabama at Birmingham",
                         "University of Alabama at Birmingham", bowlsFin$offName)
bowlsFin$offName <- gsub("University of Hawaii at Manoa", "University of Hawaii",
                         bowlsFin$offName)
bowlsFin$offName <- gsub("University of Illinois at Urbana-Champaign",
                         "University of Illinois", bowlsFin$offName)
bowlsFin$offName <- gsub("University of Maryland: College Park",
                         "University of Maryland", bowlsFin$offName)
bowlsFin$offName <- gsub("University of Minnesota: Twin Cities",
                         "University of Minnesota", bowlsFin$offName)
bowlsFin$offName <- gsub("University of Missouri: Columbia",
                         "University of Missouri", bowlsFin$offName)
bowlsFin$offName <- gsub("University of Nebraska - Lincoln",
                         "University of Nebraska", bowlsFin$offName)
bowlsFin$offName <- gsub("University of North Carolina at Chapel Hill",
                         "University of North Carolina", bowlsFin$offName)
bowlsFin$offName <- gsub("University of South Carolina-Columbia",
                         "University of South Carolina", bowlsFin$offName)
bowlsFin$offName <- gsub("University of Tennessee: Knowville",
                         "University of Tennessee", bowlsFin$offName)
bowlsFin$offName <- gsub("The University of Texas at Austin",
                         "University of Texas", bowlsFin$offName)
bowlsFin$offName <- gsub("University of Texas-El Paso",
                         "University of Texas at El Paso", bowlsFin$offName)
bowlsFin$offName <- gsub("University of Wisconsin-Madison",
                         "University of Wisconsin", bowlsFin$offName)
bowlsFin$offName <- gsub("Virginia Polytechnic Institute & State University",
                         "Viginia Polytechnic Institute and State University",
                         bowlsFin$offName)
bowlsFin$offName <- factor(bowlsFin$offName)

##changing names of bowlsFinAA
bowlsFinAA$offName <- gsub("Alabama Agricultural and Mechanical University",
                           "Alabama A & M University", bowlsFinAA$offName)
bowlsFinAA$offName <- gsub("Columbia University: Columbia College",
                           "Columbia University", bowlsFinAA$offName)
bowlsFinAA$offName <- gsub("Montana State University: Bozeman",
                           "Montana State University", bowlsFinAA$offName)
bowlsFinAA$offName <- gsub("North Carolina Agricultural and Technical State University",
                           "North Carolina A & T State University", bowlsFinAA$offName)
bowlsFinAA$offName <- gsub("Prairie View A&M University", "Prairie View A & M University",
                           bowlsFinAA$offName)
bowlsFinAA$offName <- gsub("Southern Illinois University Carbondale",
                           "Southern Illinois University", bowlsFinAA$offName)
bowlsFinAA$offName <- gsub("Southern University and Agricultural and Mechanical College",
                           "Southern University and A & M College", bowlsFinAA$offName)
bowlsFinAA$offName <- gsub("State University of New York at Stony Brook",
                           "Stony Brook University", bowlsFinAA$offName)
bowlsFinAA$offName <- gsub("Stephen F. Austin State University",
                           "Stephen F Austin State University", bowlsFinAA$offName)
bowlsFinAA$offName <- gsub("Texas State University-San Marcos", 
                           "Texas State University", bowlsFinAA$offName)
bowlsFinAA$offName <- gsub("University of Montana-Missoula",
                           "University of Montana", bowlsFinAA$offName)
bowlsFinAA$offName <- gsub("University of Tennessee: Chattanooga",
                           "University of Tennessee at Chattanooga",bowlsFinAA$offName)
bowlsFinAA$offName <- factor(bowlsFinAA$offName)



##Merging two datasets
bowlsMerge <- merge(bowlsFin, bowlsFinAA, by = c(names(bowlsFin)), all=TRUE, sort=TRUE)
bowlsMerge$Year <- ifelse(bowlsMerge$Month==1, bowlsMerge$Year-1, bowlsMerge$Year)
bowlsMerge <- na.omit(bowlsMerge)
colnames(bowlsMerge) <- c(names(bowlsMerge)[-c(11:12)], "School", "offName")

footballMerge <- merge(footballData, bowlsMerge, by.x = c("Team", "Year"), by.y = c("offName", "Year"), all=TRUE, sort=TRUE)

write.csv(footballMerge, file = "College-Football-2012-03-09.csv")

