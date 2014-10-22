


library(XML)
library(stringr)

##Get the URLS for the year-by-year lists of All-Americans
url <- "http://d1sportsnet.com/awardsf.php"
doc = htmlParse(url)

tableNodes <- getNodeSet(doc, "//table//tr//td//a[@class='CLASS09']")
urls <- sapply(tableNodes, xmlGetAttr, "href")
urls <- urls[grep("/aa/", urls)]

##1995-2012: urls[1:18]

year <- seq(2003, 1995, by = -1)
allAmericans <- vector("list", 1)

for(i in c(1:1)){
	allAmer <- readLines(con = urls[1])
	allAmer<- allAmer[grep("\\([A-Zslash]", allAmer)]   #eliminate lines without parens.
	allAmer <- strsplit(allAmer, "[\\(\\)]")  #split on parens.
	#allAmer <- do.call(gsub(allAmer, "<.*>", ""), allAmer)
	allAmer <- lapply(allAmer, str_trim)
	allAmericans[[1]] <- as.data.frame(do.call(rbind, allAmer))
	allAmericans[[1]]$year <- 2013
	}

##Merge each year's data into one data frame
#allAmericans2 <- Reduce(function(...) merge(..., all = TRUE), allAmericans)	

new2 <- as.data.frame(do.call(rbind, allAmericans))
write.csv(new2, file = "~/Desktop/allA.csv", row.names=F)


2012-2007, 2003-1995, 1993-1946


year <- seq(1915, 1889, by = -1)
allAmericans <- vector("list", 27)

for(i in 1:56){
	allAmer <- readLines(con = urls4[i])
	allAmer<- allAmer[grep("[A-Z]-[A-Z]", allAmer)]   #eliminate lines without parens.
	allAmer <- unlist(strsplit(allAmer, ","))  #split on comma.
	aamat <- matrix(allAmer, byrow=T, ncol=2)
	#pos <- unlist(strsplit(aamat[ ,1], "-"))
	aamat <- cbind(matrix(unlist(strsplit(aamat[ ,1], "-")), byrow=TRUE, ncol=2), aamat) #split on hyphen.
	allAmericans[[i]] <- data.frame(str_trim(aamat), stringsAsFactors=FALSE)
	allAmericans[[i]]$year <- rep(year[i], nrow(allAmericans[[i]]))
	}

new2 <- as.data.frame(do.call(rbind, allAmericans))
write.csv(new2, file = "~/Desktop/allA.csv", row.names=F)
