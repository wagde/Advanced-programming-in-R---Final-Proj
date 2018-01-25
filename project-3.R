
load("medals.df")
load("athletes.df")
load("countries.df")
load("games.df")

library("reshape2", lib.loc="~/R/win-library/3.2")
library("ggplot2", lib.loc="~/R/win-library/3.2")
#i load this 2 packge first 
########### question 3.1 ###############
list.medals<-function(coun,yearst,yearend){
  vic<-numeric()
  fulltable<-merge(athletes.df,medals.df)#i use the data file 
  fulltable<-fulltable[order(fulltable$year),]
  place<-(which(fulltable[,"year"]>=yearst&fulltable[,"year"]<= yearend))
  newtable1<-fulltable[place,]
  place1<-which(newtable1[,"country_id"]==coun)
  newtable2<-newtable1[place1,]
  
  yearo<-(unique(fulltable$year))[unique(fulltable$year)<= yearend&unique(fulltable$year)>=yearst]
  dataye<-data.frame(year=0,gold=0,silver=0,bronze=0)
  
  for (i in 1:length(yearo)) {
    dataye[i,"year"]<-yearo[i]
    place2<-which(newtable2[,"year"]==yearo[i])
    newtable3<-newtable2[place2,5:8]
    newtable3<-unique(newtable3)
    
    dataye[i,"bronze"]<-sum(newtable3[,"medal"]=="BRONZE")
    dataye[i,"gold"]<-sum(newtable3[,"medal"]=="GOLD")
    dataye[i,"silver"]<-sum(newtable3[,"medal"]=="SILVER")
    
  }
  return(dataye)
}

########### question 3.2 ################

PLOT.MEDAL<-function(coun,yearst,yearend){
  title<-countries.df$country_name[countries.df$country_id==coun]
  fulltable<-merge(athletes.df,medals.df)
  fulltable<-fulltable[order(fulltable$year),]
  place<-(which(fulltable[,"year"]>=yearst&fulltable[,"year"]<= yearend))
  newtable1<-fulltable[place,]
  place1<-which(newtable1[,"country_id"]==coun)
  newtable2<-newtable1[place1,]
  
  yearo<-(unique(fulltable$year))[unique(fulltable$year)<= yearend&unique(fulltable$year)>=yearst]
  dataye<-data.frame(year=0,TOTAL=0,GOLD=0,SILVER=0,BRONZE=0)
  
  for (i in 1:length(yearo)) {
    dataye[i,"year"]<-yearo[i]
    place2<-which(newtable2[,"year"]==yearo[i])
    newtable3<-newtable2[place2,5:8]
    newtable3<-unique(newtable3)
    
    dataye[i,"BRONZE"]<-sum(newtable3[,"medal"]=="BRONZE")
    dataye[i,"GOLD"]<-sum(newtable3[,"medal"]=="GOLD")
    dataye[i,"SILVER"]<-sum(newtable3[,"medal"]=="SILVER")
    dataye[i,"TOTAL"]<-sum(dataye[i,3:5])
  }
  
  reshtable<-melt(dataye,id.vars = "year") # i use the melt function here 
  plotee <- ggplot(reshtable, aes(x = year,y=value,color=))
  fplote<-plotee+geom_line(aes(group=variable, color=variable),size=1.5)+
    geom_point(aes(color=variable),size=2.5)+#the colour that i iuse 
    scale_color_manual(values=c("#151515","#DBA901", "#BDBDBD", "#8A0808" ))+
  ggtitle(paste("Medals by",title,yearst,"-",yearend))+scale_y_continuous("medals won")
  
  return(fplote)
  
}


###########question 3.3 #############
fulltable<-merge(athletes.df,medals.df)
year_count<-unique(data.frame(year=fulltable$year,country=fulltable$country_id))
year_event<-unique(data.frame(year=fulltable$year,country=fulltable$event))
yearo<-unique(fulltable$year)# first i make table to draw the plot 
countr_table<-data.frame(year=0,no.of.countries=0,no_event=0)
for (i in 1:length(yearo)) {
  countr_table[i,"year"]<-yearo[i]
  countr_table[i,"no.of.countries"]<-sum(year_count[,"year"]==yearo[i])
  countr_table[i,"no_event"]<-sum(year_event[,"year"]==yearo[i])
  
  }

p <- ggplot(countr_table, aes(x = year,y=no_event,size=no.of.countries ))
p+geom_point(colour="red")+scale_y_continuous("no. of events")+
ggtitle("No. of events and no. of countries vs. year")+scale_size("no. of countries
")








