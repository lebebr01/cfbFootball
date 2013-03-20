College Football Scraping Setup
=====

A set of R scripts that scrape college football data from cfbdatawarehouse.com and wikipedia.org.  All division I-A and I-AA teams are included.  The data includes:
* Game by game records
* Bowl game records
* Conference and Division Affiliations
* Year end polls for all time
* Yearly coach records
* Current coaches coaching and playing background
* Rivals recruiting rankings

Potential Questions
=====

* Predict whether a team will be bowl eligible for the next season
* Is recruiting ranking (number of _k_-star recruits) predictive of success?
* Is it primarily the  school or head coach that predicts the quality of recruits?


Final Data - Feb 20, 2013
=====
Current version of the final data can be found within the directory: **Data/Final/cfbData-Feb2013.Rdata**.  This can be loaded with *load("cfbData-Feb2013.Rdata")* from within the directory.  The data is stored in the object **cfbData** or you can use the command *ls()* to see all the objects currently loaded within *R*.

Games nested within Years nested within School/Teams

Games

* Win (1/0)
* Opponent
* Date
* Location
* Score

Years

* Number of wins
* Number of points scored
* Number of points allowed
* Bowl (1/0)
* Head Coach
	* Salary
	* Played college football (1/0)
	* Played professional football (1/0)
	* Coached high school
	* Coached college head
	* Coached college assistant
	* Coached professional
* End of season ranking
* Conference
* Division (e.g., I-A, I-AA)
* Athletic revenue
* Number of assistant coaches
* Rivals.com recruit ranking
* ESPN.com recruit ranking

School/Team

* Geographic data (long/lat)
* Sector (e.g., public, private)


Analysis Plan
=====

TBA

Wish List
=====

Pull the rosters from each years teams. These are available in Media Guides [http://www.collegefootballdatadvds.com/guides/guides.html](http://www.collegefootballdatadvds.com/guides/guides.html) in PDF format

Pull the All-Americans, All-Conference, and award winners for each year. Also the coaching awards.

Add weather data to the game-level data.

