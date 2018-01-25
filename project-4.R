############# question 4.1 #############


estimate.b<-function(count,k){
  bo<-data.frame(bo=0,b1=0,b2=0,b3=0,b4=0,b5=0,b6=0,b7=0,b8=0,b9=0,b10=0,b10=0,b11=0,
    b12=0,b13=0,b14=0,b15=0,b16=0,b17=0,b18=0,b19=0,b20=0,
    b21=0,b22=0,b23=0,b24=0,b25=0,b26=0,b27=0)#i have make data frame for all the B 
  mat<-matrix(,length(count),(k+1))
  
  for (i in 1:length(count)) {
    medalsum<-list.medals(count[i], 8, 2008)[27:(27-k),] # I used the function form 3.1 
for (j in 1:(k+1)) {
  mat[i,j]<-sum(medalsum[j,2:4])
}
  }
  retbo<-bo[,1:(k+1)]
 b<-lm(mat[,1]~mat[,2:(k+1)])
 btot<-b$coefficients
 for(l in 1:length(b$coefficients)){
   retbo[1,l]<-btot[l]
 }
   
return(retbo)
}

############# question 4.2 #############

count<-c("AUS" ,"CHN" ,"ESP" ,"FRA" ,"GBR" ,"GER" ,"KOR" ,"NED" ,"RUS", "USA")
mat1<-matrix(,length(count),4)

for (i in 1:length(count)) {
  medalsum<-list.medals(count[i], 8, 2008)[27:(27-3),]#i use the function from 3.1
  for (j in 1:(3+1)) {
    mat1[i,j]<-sum(medalsum[j,2:4])
  }
}
actual.2008<-mat1[,1]
actual.2008
#this will take 2 min
nboy<-data.frame(AUS=0 ,CHN=0 ,ESP=0 ,FRA=0 ,GBR=0 ,GER=0 ,KOR=0 ,NED=0 ,RUS=0, USA=0)
for (i in 1:length(count)) {
  newcount<-count[-i]
  btot<-estimate.b(newcount,3)#i use the function from 4.1
  mat1[i,1:3]
  yc<-btot$bo+(btot$b1*mat1[i,2])+(btot$b2*mat1[i,3])+(btot$b3*mat1[i,4])
  nboy[1,i]<-yc
    
  }

nboy

############# question 4.3 #############
plottable<-data.frame(predicted=0,actual=0,country=0)
for (i in 1:10) {#i use new data frame for the plot 
  plottable[i,1]<-nboy[1,i]
plottable[i,2]<-actual.2008[i]
plottable[i,3]<-count[i]
}
p<-ggplot(plottable,aes(x=actual,y=predicted))
p+geom_text(aes(label=country), size = 3.5)+geom_abline(intercept = 1)+
  ggtitle("Actual vs. predicted number of medals won, 2008")


