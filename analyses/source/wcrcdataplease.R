## Started April 2019 ##
## by Ailene ##

## Source file for reading in the data and
## formatting datafile for stan

#add week to dataset
d$week<-strftime(strptime(d$date,format= "%Y-%m-%d"), format = "%V")#new weeks start on mondays

# separate fish by species to put the fish caught 
anglers = aggregate(d$Anglers, list(paste(d$year,d$week, sep="")),sum, na.rm=TRUE)
chinook = aggregate(d$ChinCaught, list(paste(d$year,d$week, sep="")),sum, na.rm=TRUE)
coho= aggregate(d$CohoCaught, list(paste(d$year,d$week, sep="")),sum, na.rm=TRUE)
#add other species from d$OtherSpCaught
fishsum<-cbind(anglers,chinook$x,coho$x)
colnames(fishsum)<-c("date","anglers","chin","coho")
fishsum$year<-substr(fishsum$date,1,4)
fishsum$week<-substr(fishsum$date,5,6)
#fishsum<- fishsum [apply(fishsum , 1, function(x) all(!is.na(x))),] # only keep rows of all not na
fishsum<-fishsum[fishsum$date!="NANA",]
