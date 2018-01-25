library("sqldf", lib.loc="~/R/win-library/3.2")
#first i load the packge 
############# question 2.1 #############
#i have make new database 
olympics.db<-datasetsDb()
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname="olympics.db")

dbWriteTable(con, name="Medals", value=medals.df)
dbWriteTable(con, name="Countries",value=countries.df)
dbWriteTable(con, name="Athlets",value=athletes.df)
dbWriteTable(con, name="Games",value=games.df)
#i load the data frame into the data base 
############# question 2.2 #############
dbGetQuery(con,"SELECT DISTINCT SPORT
           FROM MEDALS ORDER BY SPORT ")

############# question 2.3 #############
dbGetQuery(con,"SELECT country, COUNT(COUNTRY)
  AS no_games FROM GAMES GROUP BY COUNTRY ORDER 
      BY no_games DESC")


############# question 2.4 #############
dbGetQuery(con,"SELECT first_name
 || ' '||last_name AS full_name , 
year FROM Medals JOIN Athlets 
  USING(athlete_id) WHERE event='Marathon Women' AND medal='SILVER' ")
############# question 2.5 #############

dbGetQuery(con,"SELECT DISTINCT first_name
|| ' '||last_name AS full_name,sport , year,city ||' '|| ','||
country as loction FROM Medals JOIN Athlets USING(athlete_id) 
JOIN Games USING (year)  WHERE year  BETWEEN 1920 AND 1980 AND
medal='GOLD' AND country_id='MEX'ORDER BY  year DESC  ")

############# question 2.6 #############
dbGetQuery(con,"SELECT first_name
|| ' '||last_name AS full_name, (count(athlete_id)) AS top_no_medals 
           FROM Medals JOIN Athlets USING(athlete_id)
           
           GROUP BY athlete_id ORDER BY top_no_medals DESC LIMIT 1 ")

############# question 2.7 #############
dbGetQuery(con,"SELECT first_name
|| ' '||last_name AS full_name, (count(athlete_id)) AS top_no_medals 
           FROM Medals JOIN Athlets USING(athlete_id)
           where medal='GOLD'
           GROUP BY athlete_id ORDER BY top_no_medals DESC LIMIT 1 ")



############# question 2.8 #############
dbGetQuery(con,"SELECT first_name
|| ' '||last_name AS full_name, year ,(count(athlete_id)) AS top_no_medals 
           FROM Medals JOIN Athlets USING(athlete_id)
           where medal='GOLD'
           GROUP BY year,athlete_id ORDER BY top_no_medals DESC LIMIT 1 ")







