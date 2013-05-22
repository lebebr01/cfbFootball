load("/home/lobo/Dropbox/cfbFootball/Data/Final/cfbDataFinal-Apr11.Rdata")

## creating flag for bcs Conferences
cfbData$bcsConference <- ifelse(cfbData$conference %in% 
                                  c("Atlantic Coast Conference",
                                    "Big East Conference",
                                    "Big Ten Conference",
                                    "Big 12 Conference",
                                    "Conference USA",
                                    "Mid-American Conference",
                                    "Mountain West Conference",
                                    "Pacific-12 Conference",
                                    "Southeastern Conference",
                                    "Sun Belt Conference"), 1, 0)

library(ggplot2)
library(scales)
library(RColorBrewer)

## plotting 

# Recode years for each coach
cfbData <- cfbData[is.na(cfbData$Coach) != 1, ]

library(plyr)
cfbData <- arrange(cfbData, Coach, Year)

x = ddply(cfbData, .(Coach), function (x) x$Year[1])
cfbData <- merge(cfbData, x)
cfbData$time <- cfbData$Year - cfbData$V1

c2 <- cfbData[ , c("Coach", "school", "Bowl", "Year", "Pct",
                   "Delta.Year", "time", "bcsConference", "conference",
                   "Division", 'rivals.5star', 'PF.Year', 'PA.Year')]
c2 <- c2[ !duplicated(c2), ] 

c3 <- subset(c2, Division %in% c('NCAA Division I', 'NCAA Division I-A', 
                                 'NCAA Division II', 'NCAA Division III',
                                 'NCAA University Division (Major College)',
                                 'NCAA College Division (Small College)'))
c3$Division2 <- with(c3, ifelse(Division %in% c('NCAA Division I', 'NCAA Division I-A',
                                                'NCAA University Division (Major College)'),
                                'I-A','Other'))
c3$rivals.5star2 <- with(c3, ifelse(rivals.5star == 0, 0, ifelse(rivals.5star == 1, 1, ifelse(
  rivals.5star == 2, 2, 3))))
c3$DeltaFlag <- ifelse(c3$Delta.Year > 0, "Positive", "Negative")
c3$PFflag <- ifelse(c3$PF.Year < 200, "Less 200", ifelse(c3$PF.Year > 350, "Greater 350", "Between 200 - 350"))
c3$PAflag <- ifelse(c3$PA.Year < 200, 'Less 150', ifelse(c3$PA.Year > 350, 'Greater 350', 'Between 200 - 350'))

## computing number of schools for each coach
library(data.table)
c3 <- data.table(c3)
c3[, numSchools := length(unique(school)), by = Coach] 
c3[, startYear := min(Year), by = Coach]
c3$bowlFlag <- ifelse(is.na(c3$Bowl) == TRUE, 0, 1)
c3[, numBowls := sum(bowlFlag), by = Coach]

library(ggplot2)
#pdf(file = "~/Desktop/Coaching.pdf", width = 8, height = 6)
ggplot(data = c3, aes(x = time, y = Pct)) + 
  geom_line(aes(group = Coach), alpha = 0.1) + 
  geom_smooth(se = F, size = 1) +
  #geom_point(size = 1) +
  #geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Years Coaching") +
  ylab("Winning Percentage") + 
  facet_wrap(~numSchools)
#dev.off()