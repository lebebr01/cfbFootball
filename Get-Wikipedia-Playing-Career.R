source("C:/Users/lobo/Desktop/football/myFunction.R")

pc <- as.numeric(rownames(tables[[1]][tables[[1]][ ,1] == "Playing career", ]))
cc <- as.numeric(rownames(tables[[1]][tables[[1]][ ,1] == "Coaching career (HC unless noted)", ]))

if(length(pc)==0) {
	next;
	} else {
	
	test <- tables[[1]][(pc + 1):(cc-1), ]  #Obtain the table element that contains the playing career
	years <- unlist(strsplit(as.character(test[1, 1]), split = "\n"))  #Create vector of the years
	positions <- as.character(test[2, 2:nrow(test)])

	where.played <- as.character(test[1, 2])  # Create vector of where.coached/position where coached
	comma.positions <- unlist(gregexpr(")[A-Z]", where.played))  #Finds locations to add commas - right ) follwed by capital letter
	comma.positions2 <- unlist(gregexpr("[a-z][A-Z]", where.played))

	if(comma.positions2[1] > 0){
		comma.positions <- c(comma.positions, comma.positions2, nchar(where.played))
    	} else{
    	comma.positions <- c(comma.positions, nchar(where.played))
   		 }

	comma.positions <- sort(comma.positions)
	where.played <- unlist(strsplit(my.function(where.played, comma.positions), ","))  #Create separate elements for each school/position
	
	school <- gsub("*\\(.*\\)$", "", where.played)  #Extracts coaching school
	position <- positions  
	school <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", school, perl = TRUE) #removes trailing white space

	playing <- cbind(years, school, position) 
	playing <- data.frame(playing)
	
	playing$years <- as.character(playing$years)
	playing$years <- gsub("present", "2011", as.character(playing$years))
	
	playing$school <- as.character(playing$school)
	
	my.rows <- grep("[a-zA-Z]", playing$school)
	playing <- playing[my.rows, ]
	playing$name <- names(test)[1]
	playing$career <- "Playing"
	}
