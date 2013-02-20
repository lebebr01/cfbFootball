  ##Identifying correct tables
    tb <- vector("list", length(Nodes))
      for(i in 1:length(Nodes)){
        tb[[i]] <- readHTMLTable(Nodes[[i]])
      }

  ##Tables that are the correct length
    tabNum <- matrix(nrow=length(Nodes), ncol=2)
    tabNum[,1] <- sapply(tb, length)
    tabNum[,2] <- 1:length(Nodes)

   Num <- subset(tabNum, tabNum[,1] == 7)[,2]

if(length(Num)==0){
 next;
 } else {
  selNum <- Num[c(3,6,9,12,15)]
  tb1 <- tb[[selNum[1]]]
  tb2 <- tb[[selNum[2]]]
  tb3 <- tb[[selNum[3]]]
  tb4 <- tb[[selNum[4]]]
  tb5 <- tb[[selNum[5]]]
  tbF <- rbind(tb1,tb2,tb3,tb4,tb5)
   
 colnames(tbF) <- c("W.L", "Date", "PF", "Opponent", "PA", "Location", "Notes")

 #Dates
  dates <- unlist(strsplit(as.character(tbF$Date), "-"))
 if(length(dates) == 3 * length(tbF[,1])){
 dates <- unlist(strsplit(as.character(tbF$Date), "-"))
 y <- c(0:(length(tbF$Date)-1))
 n <- 3
 mon <- 1 + y * n
 day <- 2 + y * n
 year <- 3 + y * n

 tbF$Month <- as.numeric(dates[mon])
 tbF$Day <- as.numeric(dates[day])
 tbF$Year <- as.numeric(dates[year])
  } else {

  tbF[,2] <- as.character(tbF[,2])
  for(i in 1:length(tbF[,1])){
   if(nchar(tbF[i,2])==4) {
    tbF[i,2] <- paste("00-00-", tbF[i,2], sep="") 
   } else { 
    tbF[i,2] <- tbF[i,2]
   }
  }

  dates <- unlist(strsplit(as.character(tbF$Date), "-"))  
   y <- c(0:(length(tbF$Date)-1))
 n <- 3
 mon <- 1 + y * n
 day <- 2 + y * n
 year <- 3 + y * n

 tbF$Month <- as.numeric(dates[mon])
 tbF$Day <- as.numeric(dates[day])
 tbF$Year <- as.numeric(dates[year])
 }
}

