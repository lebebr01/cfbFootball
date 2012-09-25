  ##Identifying correct tables
    tb <- vector("list", length(tableNodes))
      for(i in 1:length(tableNodes)){
        tb[[i]] <- readHTMLTable(tableNodes[[i]])
      }

  ##Tables that are the correct length
    tabNum <- matrix(nrow=length(tableNodes), ncol=2)
    tabNum[,1] <- sapply(tb, length)
    tabNum[,2] <- 1:length(tableNodes)

   Num <- subset(tabNum, tabNum[,1] == 7)[,2]

  ##Selecting and combining tables
if(length(Num) == 5){
   tb1 <- tb[[Num[3]]]
   tb1$Other <- 0
   tb2 <- tb[[Num[5]]]
   tb2$Other <- 1
   tab <- rbind(tb1, tb2)
 } else { 
  if(length(Num) ==3){
   tab <- tb[[Num[3]]]
   tab$Other <- 1
 } else {
  tab <- matrix(NA, ncol= 8, nrow=1)  
 }
 }
  
colnames(tab) <- c("No.", "W/L", "Date", "PF", "Opponent", "PA", "Bowl", "Other")
  

if(is.na(tab[1,1])){
  tab <- data.frame(tab)
  tab$Month <- NA
  tab$Day <- NA
  tab$Year <- NA
 } else {
  tab$PF <- as.numeric(as.character(tab$PF))
  tab$PA <- as.numeric(as.character(tab$PA))
  tab$Opponent <- as.character(tab$Opponent)

 ##Extracting Date
 dates <- unlist(strsplit(as.character(tab$Date), "-"))
 y <- c(0:(length(tab$Date)-1))
 n <- 3
 mon <- 1 + y * n
 day <- 2 + y * n
 year <- 3 + y * n

 tab$Month <- as.numeric(dates[mon])
 tab$Day <- as.numeric(dates[day])
 tab$Year <- as.numeric(dates[year])
}
