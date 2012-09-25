source("C:/Users/lobo/Desktop/football/myFunction.R")

#Determine table element that has the text 'Coaching career (HC unless noted)'
cc <- as.numeric(rownames(tables[[1]][tables[[1]][ ,1] == "Coaching career (HC unless noted)", ]))
  if(length(cc)>0){
   test <- tables[[1]][cc + 1, ]  #Obtain the table element that contains the coaching career
   years <- unlist(strsplit(as.character(test[1, 1]), split = "\n"))  #Create vector of the years
   coach <- as.character(test[1, 2])  # Create vector of schools/position where coached
    } else {
     cc <- as.numeric(rownames(tables[[2]][tables[[2]][,1]=="Coaching career (HC unless noted)", ]))
      test <- tables[[2]][cc + 1, ]  #Obtain the table element that contains the coaching career
      years <- unlist(strsplit(as.character(test[1, 1]), split = "\n"))  #Create vector of the years
      coach <- as.character(test[1, 2])  # Create vector of schools/position where coached
}

#test <- tables[[1]][cc + 1, ]  #Obtain the table element that contains the coaching career
#years <- unlist(strsplit(as.character(test[1, 1]), split = "\n"))  #Create vector of the years
#coach <- as.character(test[1, 2])  # Create vector of schools/position where coached

comma.positions <- unlist(gregexpr(")[A-Z]", coach))  #Finds locations to add commas - right ) follwed by capital letter
comma.positions2 <- unlist(gregexpr("[a-z][A-Z]", coach))
  if(comma.positions2[1] > 0){
    comma.positions <- c(comma.positions, comma.positions2, nchar(coach))
    } else{
    comma.positions <- c(comma.positions, nchar(coach))
    }
comma.positions <- sort(comma.positions)
if(length(comma.positions)!=length(years)){
   coach <- unlist(strsplit(my.function(coach, comma.positions), ","))
   coach <- unlist(strsplit(coach, split = "\n"))
    } else {
     coach <- unlist(strsplit(my.function(coach, comma.positions), ","))
}

#coach <- unlist(strsplit(my.function(coach, comma.positions), ","))  #Create separate elements for each school/position

school <- gsub("*\\(.*\\)$", "", coach)  #Extracts coaching school
position <- gsub("^.*\\(|\\)$", "", coach)  #Extracts text in the parentheses
school <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", school, perl = TRUE) #removes trailing white space

coaching <- cbind(years, school, position) 
coaching <- data.frame(coaching)
coaching$years <- as.character(coaching$years)
coaching$years <- gsub("present", "2011", as.character(coaching$years))
coaching$name <- names(test)[1]
coaching$career <- "Coaching"
