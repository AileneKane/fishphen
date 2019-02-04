
#Only use fishing areas that have atleast 4 years with >20 observations:
#Look at some basic stats about orca observations
#Started with orca_dataprep_occmodel.R code
#4 February 2019

#housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/GitHub/fishphen")
#or from laptop:
#setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

# Load libraries
library(dplyr)
# 1. Get the data
d <- read.csv("data/AppendixII.csv")

# 2. Clean the data (also saved in output/AppendixII_cleaned,csv)
source("analyses/clean_orca.R")

#Create a new column that combines Pod and Likely Pod columna and removes spaces
d$Pod.cl<-d$Pod


#Always use Likely Pod column, when it is not blank:
d$Pod.cl[d$LikelyPod!="" & d$LikelyPod!=" "]<-d$LikelyPod[d$LikelyPod!="" & d$LikelyPod!=" "]
#perhaps also stick with Pod when LikelyPod has a "?" grep("?",d$LikelyPod,)

#remove non-orca data
#d<-d[d$Pod.cl=="HB?"|d$Pod.cl=="Not Orcas",]

#only using fishing areas in Washington's Salish Sea 
d<-d[d$FishArea %in% c("01","02","03","04","05","06","07","09","10","11","12","13","81","82"),]#not sure where 17, 18, 19, 20, 28, 29 are...need to find out. also, where is 42583,42584

#Assign region, based on fishing area:
d$region<-"ps"
d$region[d$FishArea=="07"|d$FishArea=="06"|d$FishArea=="05"|d$FishArea=="04"]<-"uss"
d$region[d$FishArea=="01"|d$FishArea=="02"|d$FishArea=="03"]<-"oc"#outer coast

#Add week and day of year (day)
d$day<-strftime(strptime(paste(d$Month, d$Day, d$Year, sep="."),format= "%m.%d.%Y"),format= "%j")
d$week<-strftime(strptime(paste(d$Month, d$Day, d$Year, sep="."),format= "%m.%d.%Y"), format = "%V")#new weeks start on mondays

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

#only data after 1978
d<-d[d$Year>1978,]

# Add a column that combines :
#1) day, year, and region; 
#2) day, year, and fishing area 
#3) week, year, and region; 
#4) week, year, and fishing are; 
#to use to total up observations for later analyses 
d$yrdayregion<-paste(d$Year, d$day,d$region,sep="_")
d$yrwkregion<-paste(d$Year, d$week,d$region,sep="_")
d$yrdayfa<-paste(d$Year, d$day,d$FishArea,sep="_")
d$yrwkfa<-paste(d$Year, d$week,d$FishArea,sep="_")

#Plot a bunch of different things:
#1. Plot the total number of orca observations per year in each region since 1979
  #a. All pods together
  #b .Each pod separately
obs = aggregate(Orcas ~Year, data = d,sum)
js = aggregate(J ~Year, data = d,sum)
ks = aggregate(K~Year, data = d,sum)
ls = aggregate(L~Year, data = d,sum)
srs = aggregate(SRKW~Year, data = d,sum)
orcasum<-cbind(js,ks[,2],ls[,2],srs[,2],obs[2])
colnames(orcasum)[2:6]<-c("Jobs","Kobs","Lobs","AllSRobs","AllOrcas")

quartz()
t<-rownames(orcasum)
ps<-region.sumobs[,1]
uss<-region.sumobs[,2]

plot(t,uss,type="l",xlab="Year",ylab="Number of detections", lwd=2,bty="l")
lines(t,ps, lwd=2,lty=2)
legend("topright",)

#2. Plot the number of "whale days" (days that whales were observed in each region)
  #a. All pods together
  #b .Each pod separately
obs = aggregate(Orcas ~yrdayfa, data = d,sum)
js = aggregate(J ~yrdayfa, data = d,sum)
ks = aggregate(K~yrdayfa, data = d,sum)
ls = aggregate(L~yrdayfa, data = d,sum)
srs = aggregate(SRKW~yrdayfa, data = d,sum)
orcasum<-cbind(js,ks[,2],ls[,2],srs[,2],obs[2])
colnames(orcasum)[2:6]<-c("Jobs","Kobs","Lobs","AllSRobs","AllOrcas")


#3. Plot the nu
