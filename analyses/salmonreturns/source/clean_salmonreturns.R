#Cleaning and formatting salmon return data
#Create columns for month, year, doy
d$doy<-strftime(strptime(d$Event_Date,format= "%m/%d/%y"),format= "%j")
d$year<-strftime(strptime(d$Event_Date,format= "%m/%d/%y"),format= "%Y")
d$month<-strftime(strptime(d$Event_Date,format= "%m/%d/%y"),format= "%m")

unique(d$year)#goes back to 1995
unique(d$ADULT_EVENT_TYPE_Name)#Trap estimates 
#just use trap estimates
d<-d[d$ADULT_EVENT_TYPE_Name=="Trap Estimate",] #start with trap events. could also look at Parent Spawn EVents
unique(d$MarkCode)#should find out what these mean...
#which column should i use- adults_Cnt, Females_Cnt, Males_Cnt
#length(which(is.na(d$Adults_Cnt)))
#length(which(is.na(d$Males_Cnt)))
#length(which(is.na(d$Females_Cnt)))
#definitely ADults_Cnt- ahs way more data!
#When Adults_Cnt, does not have data, does Females_Cnt have data?
#length(which(is.na(d$Females_Cnt[which(is.na(d$Adults_Cnt))])))
#unique(d$Females_Cnt[which(is.na(d$Adults_Cnt))])
#nope- all NAs!

#in most cases, the Brood year column seems to align with the spawning year in the way i want it to, but occasionally there are mistakes in it
  #especially in 1997-1998. fix this
  
d$year[d$BROOD_Yr==1997 & d$year==2000]<-1997#i think the brood year is correct because all other nearby rows have year=1997
d$BROOD_Yr[d$BROOD_Yr==1993]<-d$year[d$BROOD_Yr==1993]#
d$BROOD_Yr[d$BROOD_Yr==2003 & d$year==2002]<-2002
d$BROOD_Yr[d$BROOD_Yr==1997 & d$year==1998 & d$doy>250]<-1998

d$Adults_Cnt[which(d$Adults_Cnt<0)]<-abs(d$Adults_Cnt[which(d$Adults_Cnt<0)])

d$doy<-as.integer(d$doy)
