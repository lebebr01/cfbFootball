library(XML)
library(plyr)
library(data.table)

#Division 1A
theurl <- data.frame(url = c("http://www.teamrankings.com/college-football/ranking/strength-of-schedule-by-team?rating_date=2014-01-11",
                             "http://www.teamrankings.com/college-football/ranking/strength-of-schedule-by-team?rating_date=2013-01-11",
                             "http://www.teamrankings.com/college-football/ranking/strength-of-schedule-by-team?rating_date=2012-01-11",
                             "http://www.teamrankings.com/college-football/ranking/strength-of-schedule-by-team?rating_date=2011-01-11",
                             "http://www.teamrankings.com/college-football/ranking/strength-of-schedule-by-team?rating_date=2010-01-11",
                             "http://www.teamrankings.com/college-football/ranking/strength-of-schedule-by-team?rating_date=2009-01-11",
                             "http://www.teamrankings.com/college-football/ranking/strength-of-schedule-by-team?rating_date=2008-01-11",
                             "http://www.teamrankings.com/college-football/ranking/strength-of-schedule-by-team?rating_date=2007-01-11",
                             "http://www.teamrankings.com/college-football/ranking/strength-of-schedule-by-team?rating_date=2006-01-11",
                             "http://www.teamrankings.com/college-football/ranking/strength-of-schedule-by-team?rating_date=2005-01-11",
                             "http://www.teamrankings.com/college-football/ranking/strength-of-schedule-by-team?rating_date=2004-01-11"),
                     year = seq(2014, 2004))


