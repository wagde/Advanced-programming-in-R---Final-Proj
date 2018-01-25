
library("reshape2", lib.loc="~/R/win-library/3.2")
library("ggplot2", lib.loc="~/R/win-library/3.2")
PLOT.MEDAL.2<-function(coun1,yearst,yearend,medalsta){
  
  if(yearst>yearend){#i condtins
    return(ggplot()+ggtitle("THE STARTING YEAR SHOULD BE LESS THAN THE ENDYEAR"))
  }
  if(length(medalsta)==0){
    return(ggplot()+ggtitle("Please Choose Any Medals"))
  }
  
  #the same function that i use in 3.4 the same counstract 
  
  coun<-countries.df$country_id[countries.df$country_name==coun1]
  fulltable<-merge(athletes.df,medals.df)
  fulltable<-fulltable[order(fulltable$year),]
  place<-(which(fulltable[,"year"]>=yearst&fulltable[,"year"]<= yearend))
  newtable1<-fulltable[place,]
  place1<-which(newtable1[,"country_id"]==coun)
  newtable2<-newtable1[place1,]
  
  yearo<-(unique(fulltable$year))[unique(fulltable$year)<= yearend&unique(fulltable$year)>=yearst]
  dataye<-data.frame(year=0,TOTAL=0,GOLD=0,SILVER=0,BRONZE=0)
  
  without<-medalsta[-1]
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
dataye<-dataye[,c("year",medalsta)]
chose.color<-character()
for (i in 1:length(medalsta)) {
  vic.kindmed<-c("TOTAL","GOLD","SILVER","BRONZE")#a (for) to chse the the colour 
  placeo<-which(vic.kindmed==medalsta[i])
  
  vic.color<-c("#151515","#DBA901", "#BDBDBD", "#8A0808" )[placeo] 
  chose.color<-c(chose.color,vic.color)
}

  reshtable<-melt(dataye,id.vars = "year")
  plotee <- ggplot(reshtable, aes(x = year,y=value,color=))
  reploc<-plotee+geom_line(aes(group=variable, color=variable),size=1.5)+
    geom_point(aes(color=variable),size=3)+
    scale_color_manual(values=chose.color)+
    ggtitle(paste("Medals by",coun1,yearst,"-",yearend))+scale_y_continuous("medals won")
  
#the plot buliding 
  
  return( reploc)
  
}