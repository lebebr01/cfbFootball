if(length(tables[[2]]$Overall) != 0) {
  winloss1 <- tables[[2]] 
   } else {
    if(length(tables[[3]]$Overall) != 0) {
     winloss1 <- tables[[3]]
      } else {
      if(length(tables[[4]]$"Regular Season") != 0){
        winloss1 <- tables[[5]]
         } else {
          if(length(tables[[4]]$Overall) != 0) {
           winloss1 <- tables[[4]]
            } else {
             break
}}}}

winloss1$Year1<-as.numeric(as.character(winloss1$Year))
winloss<-subset(winloss1,Year1>0)

Overall1<-as.character(winloss$Overall)
conf<-as.character(winloss$Conference)
stand<-as.character(winloss$Standing)
bowl<-as.character(winloss[,6])

for(i in 1:length(Overall1)){
   if(nchar(Overall1[i]) > 4){
     Overall1[i] <- gsub(".$", "", Overall1[i])
  } else {
     Overall1[i] <- Overall1[i]
  }
}

for(j in 1:length(conf)){
   if(nchar(conf[j]) == 0){
     conf[j] <- "Miss"
  } else {
   if(nchar(conf[j]) > 3){
     conf[j] <- gsub(".$", "", conf[j])
  } else {
     conf[j] <- conf[j]
  }
  }
}

y<-c(0:(length(Overall1)-1))
n<-2
win<-rep(1+y*n,1)
loss<-rep(2+y*n,1)

for(k in 1:length(conf)){
   if(conf[k] == "Miss"){
     conf[k] <- "NA"
   } else {
     conf[k] <- conf[k]
}}

winloss$oW<-as.numeric(unlist(strsplit(gsub("[^0-9]",",",Overall1),","))[win])
winloss$OL<-as.numeric(unlist(strsplit(gsub("[^0-9]",",",Overall1),","))[loss])
winloss$cW<-as.numeric(unlist(strsplit(gsub("[^0-9]",",",conf),","))[win])
winloss$cL<-as.numeric(unlist(strsplit(gsub("[^0-9]",",",conf),","))[loss])
winloss$stand <- as.numeric(unlist(gsub("[^0-9]","",stand)))

winloss$coachRank<-as.numeric(as.character(winloss[,7]))
winloss$apRank<-as.numeric(as.character(winloss[,8]))
winloss$bowl<-bowl


hCoach<-winloss[c(2,9:length(winloss))]
colnames(hCoach)[2] <- "Year"

hCoach
