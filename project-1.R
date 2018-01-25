#name:wagde armosh
#id:311136816


library("XML", lib.loc="~/R/win-library/3.2")   #first i load this 2 packges 
library("RCurl", lib.loc="~/R/win-library/3.2")

############# question 1.1 #############
URL <- "http://www.databaseolympics.com" #i take the year links form the main page 
parsed.page <- htmlParse(getURL(URL))
URL.vec <- xpathSApply(parsed.page, "//a[starts-with(@href, '/games/')]",
                    xmlGetAttr, 'href')
vic<-numeric()
vic<-unique(paste("http://www.databaseolympics.com",URL.vec, sep=""))[2:28]#and merge the link wih the main page 
  games.df<- data.frame(year=0,country=0,city=0)
for(i in 1:length(vic)){
parsed.page <- htmlParse(vic[i])
vic1<-xpathSApply(parsed.page, "//b", xmlValue)[1:2]
games.df[i,]<-c(substr(vic1[1],1,4),rev(unlist(strsplit( vic1[2], ","))))
}

  ########### Qustion 1.2###########
  vic2<-numeric()#victor for the links of  sport games 
  vic2.name<-character()#the name of the sports 
  for (i in 1:length(vic)) { 
    parsed.page11 <- htmlParse(getURL(vic[i]))
    URL.vec1 <- xpathSApply(parsed.page11, "//a[starts-with(@href, '/games/')]",xmlGetAttr, 'href')
    vic2.name<-c(vic2.name ,(xpathSApply(parsed.page11, "//a[starts-with(@href, '/games/')]",xmlValue)[-1]))
    vic2<-c(vic2,paste("http://www.databaseolympics.com",URL.vec1[2:length(URL.vec1)],sep ="")) 
    
  }
  medals.df<- data.frame(year=0,sport=0,event=0,athlete_id=0,medal=0)
  place<-0
  for (i in 1:length(vic2)) {#i have pass on all the year sport link and take the data
    place.vic<-numeric()
    parsed.page1 <- htmlParse(vic2[i])
    
    vic3<-xpathSApply(parsed.page1, "//td", xmlValue)[-(1:12)]#this have the data on sport table
    sportoo<-(vic3[c(1,which(vic3=="BRONZE"|vic3=="SILVER"|vic3=="BRONZE")+1)])
    sporto<-sportoo[which(nchar(sportoo)>2)]#a method to taking the data
    
    playerurl<-paste("http://www.databaseolympics.com",(xpathSApply(parsed.page1, "//a[starts-with(@href, '/players/')]",xmlGetAttr, 'href')),sep ="")[-1]
    
    yearo<-rep(xpathSApply(parsed.page1, "//a[starts-with(@href, '/games/')]",xmlValue)[-1],length(playerurl))    
    medalso<-vic3[which(vic3=="BRONZE"|vic3=="GOLD"|vic3=="SILVER")]
    kind.sporto<-rep(vic2.name[i],length(playerurl))
    for (l in 1:length(sporto)) {
      place.vic<-c(place.vic,which(vic3==sporto[l]))
      
    } 
    
    counter_<-character()
    place.vic<-c(place.vic,length(vic3))
    for (k in 1:(length(place.vic)-1)) {
      
      vic3_<-vic3[(place.vic[k]):(place.vic[k+1])]
      counter_<-c(counter_,rep(sporto[k],sum(vic3_=="BRONZE"|vic3_=="SILVER"|vic3_=="GOLD")))
      
    }
    
    for (j in 1:length(playerurl)) {#i have put the data on the data frame here
      place<-place+1
      medals.df[place,4]<-toupper(paste(unlist(strsplit(playerurl[j]  , ""))[-(1:61)],collapse  = ""))
      medals.df[place,1]<-yearo[j]
      medals.df[place,2]<-kind.sporto[j]
      medals.df[place,3]<-counter_[j]
      medals.df[place,5]<-medalso[j]
      
    } 
  }

  ############# question 1.3 #############
  first_last_vic<-character()
  athlete_id<-character()
  country_id<-character()
  for (i in 1:length(vic2)) {#this qusetion depent on question 2 vic2 that used on question 2 
    place.vic<-numeric()
    parsed.page1 <- htmlParse(vic2[i])
    
    vic3<-xpathSApply(parsed.page1, "//td", xmlValue)[-(1:12)]
    mat<-matrix(vic3,5, )
    playerurl<-paste("http://www.databaseolympics.com",(xpathSApply(parsed.page1, "//a[starts-with(@href, '/players/')]",xmlGetAttr, 'href')),sep ="")[-1]
    country_id<-c(country_id,mat[3,])
    athlete_id<-c(athlete_id,substr(playerurl,62,72))
    first_last_vic<-c(first_last_vic,mat[2,])
    
  } 
  first_name<-character()
  last_name<-character() #a method to cut fristname and the last name
  
  for (i in 1:length(first_last_vic)) {
    first_last<-unlist(strsplit(first_last_vic[i]," "))
    first_name[i]<-first_last[1]
    last_name[i]<-first_last[2]
    
  }
  athletes.df<- data.frame(athlete_id,first_name,last_name, country_id)
  #i put the vicors that i make 
  
  
  
  ############# question 1.4 #############
  countries.df<-data.frame(country_id=0,country_name=0 )
 
   country.page <- htmlParse("http://www.databaseolympics.com/country/countrylist.htm")
   URL.country<-xpathSApply(country.page, 
                  "//a[starts-with(@href, '/country/')]",xmlGetAttr, 'href')[-1]
   countryid<-numeric()
   countr<-xpathSApply(country.page, 
                       "//a[starts-with(@href, '/country/')]",xmlValue)[-1]
     for (i in 1:length(URL.country)) {
           
       countries.df[i,"country_id"]<-paste(unlist(strsplit(URL.country[i],"" ))[30:32],collapse = "")
       countries.df[i,"country_name"]<-countr[i]
  
}



  
