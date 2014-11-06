# Read in Data
fb = read.csv(file = "/Users/andrewz/Documents/Research/Computing-Projects/Football-Project/Data/College-Football-2012-03-09.csv")

library(dplyr)

# Set up Team/Coach keys by year
coach_team = fb %>%
  group_by(Team, Coach, Year) %>%
  distinct(Team, Coach, Year) %>%
  select(Team, Coach, Year) %>%
  filter(Coach != "NA", Coach != "Unknown")

head(coach_team)

coach_team[coach_team$Coach == "Milton E. Daniel", ]


# Read in game-level data
games = read.csv(file = "/Users/andrewz/Desktop/Football/Games.csv")

games2 = games %>%
	select(team, opponent, date, year)

head(games2)

# I would like to basically merge the team coach and the opponent coach in using the coach_team data
# as a key. Two problems: (1) the team names in the game-level data are short versions (e.g.,
# TCU rather than Texas Christian University), and (2) the year is actual year the game took place...
# The year in the other data set refers to the two-year season (e.g., 2011 = 2011/2012 season).


# Then, in theory, we could use this syntax
# I used this to create the matrices by school rather than coach

###################################################
### Format data for IRT
###################################################

i = unique(fb2$team)[1]
mySchool = fb2 %.%
	 filter(team == i)
myTable = table(mySchool$win, droplevels(mySchool$opponent)) 
test = data.frame(t(myTable[row.names(myTable)=="W", ]))
test$school = fb2$team[i]

myData = test

for(i in unique(fb2$team)){
	mySchool = fb2 %.%
	 filter(team == i)
	myTable = table(mySchool$win, droplevels(mySchool$opponent)) 
	test = data.frame(t(myTable[row.names(myTable)=="W", ]))
	test$team = i
	#mn2$year = i
	myData = merge(myData, test, all = TRUE)
	}
row.names(myData) = myData$team 
myData2 = myData[-1]
myData2[myData2 == 2] = 1


nonNA = as.vector(apply(myData2, 2, function(x) length(which(!is.na(x)))))
raschData = myData2[nonNA > 2]



