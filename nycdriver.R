
# First download the dataset [nycdriver(2007-2013).xls] from: 
# https://github.com/yimihua2013/NYCdriver/blob/master/nycdriver(2007-2013).xls

###load dataset###
library(gdata)
#sheet 2 is driver license file from NYS-DMV 
#and sheet 3 is the population statistics in NYC
 nycdriver<-read.xls("nycdriver(2007-2013).xls",sheet=2)
 nycpop<-read.xls("nycdriver(2007-2013).xls",sheet=3)
 
 ###Data Exploring###
 ##(1)changing of the population in NYC
 library(ggplot2)
 labels <- round(nycpop$POPULATION/1000000,3)
 ggplot(nycpop,aes(x=YEAR, y=POPULATION))+geom_line(colour="blue")+
     geom_point(colour="red",size=3)+ylim(5000000,10000000)+
     geom_text(aes(label=paste(format(labels,nsmall=2),"million")),
      vjust=-1,size=4)+ggtitle("Estimated Population of NYC(2007~2013)")
  
 ##(2) Percentage of people who drive
 # data prepare: # of drivers in 2007~2013
 driver_year<-tapply(nycdriver$TOTAL,nycdriver$YEAR,sum)
 DRIVERS<-numeric(7)
 for (i in 1:7){
     DRIVERS[i]<-driver_year[[i]]
 }
# compute the percentage
nycdriver01<-cbind(nycpop[,-3],DRIVERS)
nycdriver01$VALUE<-round(nycdriver01$DRIVERS/nycdriver01$POPULATION,4)
library(scales)
nycdriver01$PERCENT<-percent(nycdriver01$DRIVERS/nycdriver01$POPULATION)
# check the data
nycdriver01
# plot the data
ggplot(nycdriver01,aes(x=YEAR,y=VALUE))+geom_line(colour="darkgreen")+
    geom_point(size=3,colour="red")+ylim(0.3,0.5)+geom_text(aes(label=PERCENT),
    vjust=-0.8)+ggtitle(" % of population who drive in NYC(2007~2013)")
 
 
##(3)summary of drivers based on sex 
# male drivers
 male<-tapply(nycdriver$MALE,nycdriver$YEAR,sum)
 year<-2007:2013
 maleValue<-numeric(7)
 for (j in 1:7){
     maleValue[j]<-male[[j]]
 }
 male_driver<-data.frame(YEAR=year,DRIVERS=maleValue)
 male_driver$SEX<-rep("male",7)
 #female drivers
 female<-tapply(nycdriver$FEMALE,nycdriver$YEAR,sum)
 femaleValue<-numeric(7)
 for (k in 1:7){
     femaleValue[k]<-female[[k]]
 }
 female_driver<-data.frame(YEAR=year,DRIVERS=femaleValue)
 female_driver$SEX<-rep("female",7)
 
 nycdriver2<-rbind(male_driver,female_driver)
#check the data
nycdriver2
# plot the data
 library(plyr)
 #sort data by YEAR and SEX
 nycdriver_sex<-arrange(nycdriver2,YEAR,SEX)
 # get the cumulative sum
 nycdriver_sex<-ddply(nycdriver_sex,"YEAR",transform,label_y=cumsum(DRIVERS)-0.5*DRIVERS)
 
ggplot(nycdriver_sex,aes(x=factor(YEAR),y=DRIVERS,fill=SEX))+geom_bar(stat="identity")+
 geom_text(aes(y=label_y,label=DRIVERS))+ ggtitle("Male vs Female driver in NYC")
 

 
 

 
 