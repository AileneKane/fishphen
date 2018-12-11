# Orca data prep for movement
# Started December 11, 2018
# by Ailene Ettinger ailene.ettinger@noo.gov

# 2. Clean the data (also saved in output/AppendixII_cleaned,csv)
source("analyses/clean_orca.R")

#Create a new column that combines Pod and Likely Pod columna and removes spaces
d$Pod.cl<-d$Pod

#Always use Likely Pod column, when it is not blank:
d$Pod.cl[d$LikelyPod!="" & d$LikelyPod!=" "]<-d$LikelyPod[d$LikelyPod!="" & d$LikelyPod!=" "]
#perhaps also stick with Pod when LikelyPod has a "?" grep("?",d$LikelyPod,)

#remove non-orca data
d<-d[d$Pod.cl!="HB?"|d$Pod.cl!="Not Orcas",]

#only using fishing areas in Washington's Salish Sea
#d<-d[d$FishArea %in% c("01","02","03","04","05","06","07","09","10","11","12","13","81","82"),]#not sure where 17, 18, 19, 20, 28, 29 are...need to find out. also, where is 42583,42584


#Add week and day of year (day)
d$day<-strftime(strptime(paste(d$Month, d$Day, d$Year, sep="."),format= "%m.%d.%Y"),format= "%j")
d$week<-strftime(strptime(paste(d$Month, d$Day, d$Year, sep="."),format= "%m.%d.%Y"), format = "%V")#new weeks start on mondays

#Create a new column that combines Lat/Long and Actual Lat/Long columns
d$Lat.cl<-d$Lat
d$Long.cl<-d$Long

#Always use actual lat/long columns, when they are not blank:
d$Lat.cl[d$ActLat!="" & d$ActLat!=" " & is.na(d$ActLat)==FALSE]<-d$ActLat[d$ActLat!="" & d$ActLat!=" " & is.na(d$ActLat)==FALSE]
d$Long.cl[d$ActLong!="" & d$ActLong!=" " & is.na(d$ActLong)==FALSE]<-d$ActLong[d$ActLong!="" & d$ActLong!=" " & is.na(d$ActLong)==FALSE]



#Add a column for presences (1/0) for each pod, for Ts, and for SRKWs
d$J<-0
d$J[grep("J",d$Pod.cl)]<- 1
d$K<-0
d$K[grep("K",d$Pod.cl)]<- 1
d$L<-0
d$L[grep("L",d$Pod.cl)]<- 1
d$SRKW<-0
d$SRKW[grep("SR",d$Pod.cl)]<- 1
d$SRKW[d$J==1|d$K==1|d$L==1]<- 1   
d$Orcas<-1

# Restrict analysis to only SRKWs (pods J,K,L)
#d<-d[d$SRKW==1,]#do not do this for now- perhaps this will  reduce the number of "perfect detections"?

#only data after 1978
d<-d[d$Year>1978,]


j<-subset(d,select=c(FishArea,J,Month,day,Year,Lat.cl,Long.cl))
j<-subset(j,J==1)#only when j is present!
j$Lat.cl<-as.numeric(j$Lat.cl)
j$Long.cl<-as.numeric(j$Long.cl)
j<- j[apply(j, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
k<-subset(d,select=c(FishArea,K,Month,day,Year,Lat.cl,Long.cl))
k<-subset(k,K==1)#only when k is present!
k$Lat.cl<-as.numeric(k$Lat.cl)
k$Long.cl<-as.numeric(k$Long.cl)
k<- k[apply(k, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
l<-subset(d,select=c(FishArea,L,Month,day,Year,Lat.cl,Long.cl))
l<-subset(l,L==1)#only when L is present!
l$Lat.cl<-as.numeric(l$Lat.cl)
l$Long.cl<-as.numeric(l$Long.cl)
l<- l[apply(l, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
