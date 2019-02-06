
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
library(mgcv)
library(scales)
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
d<-d[-which(is.na(d$day)),]
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
d<-d[d$Year>1977,]

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
d$yrregion<-paste(d$Year,d$region,sep="_")
d$yrfa<-paste(d$Year, d$FishArea,sep="_")

#Plot a bunch of different things:
#1. Plot the total number of orca observations per year in each region since 1978
  #a. All pods together
  #b .Each pod separately
obs = aggregate(Orcas ~yrregion, data = d,sum)
js = aggregate(J ~yrregion, data = d,sum)
ks = aggregate(K~yrregion, data = d,sum)
ls = aggregate(L~yrregion, data = d,sum)
srs = aggregate(SRKW~yrregion, data = d,sum)
orcasum<-cbind(js,ks[,2],ls[,2],srs[,2],obs[2])
colnames(orcasum)[2:6]<-c("Jobs","Kobs","Lobs","AllSRobs","AllOrcas")
orcasum$year<-substr(orcasum$yrregion,1,4)
orcasum$region<-substr(orcasum$yrregion,6,nchar(orcasum$yrregion))

quartz()
#par(mfrow)
#start with uss, all SRKWs
plot(orcasum$year[orcasum$region=="uss"],orcasum$AllSRobs[orcasum$region=="uss"],type="l",xlab="Year",ylab="Number of detections", lwd=2,bty="l", main="All SRKW sightings, 1978-2017")
lines(orcasum$year[orcasum$region=="ps"],orcasum$AllSRobs[orcasum$region=="ps"], lwd=2,lty=2)
lines(orcasum$year[orcasum$region=="oc"],orcasum$AllSRobs[orcasum$region=="oc"], lwd=2,lty=3)
lines(orcasum$year[orcasum$region=="uss"],orcasum$Jobs[orcasum$region=="uss"],lwd=2,col="blue")
lines(orcasum$year[orcasum$region=="ps"],orcasum$Jobs[orcasum$region=="ps"], lwd=2,lty=2, col="blue")
lines(orcasum$year[orcasum$region=="oc"],orcasum$Jobs[orcasum$region=="oc"], lwd=2,lty=3, col="blue")
lines(orcasum$year[orcasum$region=="uss"],orcasum$Kobs[orcasum$region=="uss"],lwd=2,col="purple")
lines(orcasum$year[orcasum$region=="ps"],orcasum$Kobs[orcasum$region=="ps"], lwd=2,lty=2, col="purple")
lines(orcasum$year[orcasum$region=="oc"],orcasum$Kobs[orcasum$region=="oc"], lwd=2,lty=3, col="purple")
lines(orcasum$year[orcasum$region=="uss"],orcasum$Lobs[orcasum$region=="uss"],lwd=2,col="darkred")
lines(orcasum$year[orcasum$region=="ps"],orcasum$Lobs[orcasum$region=="ps"], lwd=2,lty=2, col="darkred")
lines(orcasum$year[orcasum$region=="oc"],orcasum$Lobs[orcasum$region=="oc"], lwd=2,lty=3, col="darkred")

legend("topleft",legend=c("Upper Salish Sea","Puget Sound","Outer Coast","Js","Ks","Ls"), lty=c(1,2,3,1,1,1),col=c("black","black","black","blue","purple","darkred"), bty="n")


quartz()
par(mfrow=c(3,1))
#start with uss, all SRKWs
plot(orcasum$year[orcasum$region=="uss"],orcasum$Jobs[orcasum$region=="uss"],type="l",xlab="Year",ylab="Number of detections", lwd=2,bty="l", main="J pod sightings, 1978-2017", col="blue")
lines(orcasum$year[orcasum$region=="ps"],orcasum$Jobs[orcasum$region=="ps"], lwd=2,lty=2, col="blue")
lines(orcasum$year[orcasum$region=="oc"],orcasum$Jobs[orcasum$region=="oc"], lwd=2,lty=3, col="blue")

legend("topleft",legend=c("uss","ps","oc"), col="blue", lty=c(1,2,3), bty="n")
#Kpod
plot(orcasum$year[orcasum$region=="uss"],orcasum$Kobs[orcasum$region=="uss"],type="l",xlab="Year",ylab="Number of detections", lwd=2,bty="l", main="K pod sightings, 1978-2017", col="purple")
lines(orcasum$year[orcasum$region=="ps"],orcasum$Kobs[orcasum$region=="ps"], lwd=2,lty=2, col="purple")
lines(orcasum$year[orcasum$region=="oc"],orcasum$Kobs[orcasum$region=="oc"], lwd=2,lty=3, col="purple")
#Lpod
plot(orcasum$year[orcasum$region=="uss"],orcasum$Lobs[orcasum$region=="uss"],type="l",xlab="Year",ylab="Number of detections", lwd=2,bty="l", main="K pod sightings, 1978-2017", col="darkred")
lines(orcasum$year[orcasum$region=="ps"],orcasum$Lobs[orcasum$region=="ps"], lwd=2,lty=2, col="darkred")
lines(orcasum$year[orcasum$region=="oc"],orcasum$Lobs[orcasum$region=="oc"], lwd=2,lty=3, col="darkred")

#2. Plot the number of "whale days" (days that whales were observed in each region)
  #a. All pods together
  #b .Each pod separately
obs.days = aggregate(Orcas ~yrdayregion, data = d,sum)
js.days = aggregate(J ~yrdayregion, data = d,sum)
ks.days = aggregate(K~yrdayregion, data = d,sum)
ls.days = aggregate(L~yrdayregion, data = d,sum)
srs.days = aggregate(SRKW~yrdayregion, data = d,sum)
orcasum.days<-cbind(js.days,ks.days[,2],ls.days[,2],srs.days[,2],obs.days[2])
colnames(orcasum.days)[2:6]<-c("Jobs","Kobs","Lobs","AllSRobs","AllOrcas")
orcasum.days$year<-substr(orcasum.days$yrdayregion,1,4)
orcasum.days$day<-substr(orcasum.days$yrdayregion,6,8)
orcasum.days$region<-substr(orcasum.days$yrdayregion,10,nchar(orcasum.days$yrdayregion))
orcasum.days$Jpres<-orcasum.days$Jobs
orcasum.days$Jpres[orcasum.days$Jobs>0]<-1
orcasum.days$Kpres<-orcasum.days$Kobs
orcasum.days$Kpres[orcasum.days$Kobs>0]<-1
orcasum.days$Lpres<-orcasum.days$Lobs
orcasum.days$Lpres[orcasum.days$Lobs>0]<-1
orcasum.days$AllSRpres<-orcasum.days$AllSRobs
orcasum.days$AllSRpres[orcasum.days$AllSRobs>0]<-1
#summarize whale days per year by region
wdays<-as.data.frame(tapply(orcasum.days$AllSRpres,list(orcasum.days$year,orcasum.days$region),sum))
quartz()
#par(mfrow)
#start with uss, all SRKWs
plot(rownames(wdays),wdays$uss,type="l",xlab="Year",ylab="Number of whale days", lwd=2,bty="l", main="All SRKW sightings, 1978-2017", ylim=c(0,200))
lines(rownames(wdays),wdays$ps, lwd=2,lty=2)
lines(rownames(wdays),wdays$oc, lwd=2,lty=3)
legend("topleft",legend=c("Upper Salish Sea","Puget Sound","Outer Coast"), lty=c(1,2,3),col="black", bty="n")


wdays.J<-as.data.frame(tapply(orcasum.days$Jpres,list(orcasum.days$year,orcasum.days$region),sum))
wdays.K<-as.data.frame(tapply(orcasum.days$Kpres,list(orcasum.days$year,orcasum.days$region),sum))
wdays.L<-as.data.frame(tapply(orcasum.days$Lpres,list(orcasum.days$year,orcasum.days$region),sum))

quartz()
par(mfrow=c(3,1))#par(mfrow)
#J pod
plot(rownames(wdays.J),wdays.J$uss,type="l",xlab="Year",ylab="Number of whale days", lwd=2,bty="l", main="J sightings, 1978-2017", ylim=c(0,200), col="blue")
lines(rownames(wdays.J),wdays.J$ps, lwd=2,lty=2, col="blue")
lines(rownames(wdays.J),wdays.J$oc, lwd=2,lty=3, col="blue")
legend("topleft",legend=c("Upper Salish Sea","Puget Sound","Outer Coast"), lty=c(1,2,3),col="blue", bty="n")

#K
plot(rownames(wdays.K),wdays.K$uss,type="l",xlab="Year",ylab="Number of whale days", lwd=2,bty="l", main="K sightings, 1978-2017", ylim=c(0,200), col="purple")
lines(rownames(wdays.K),wdays.K$ps, lwd=2,lty=2, col="purple")
lines(rownames(wdays.K),wdays.K$oc, lwd=2,lty=3, col="purple")

#L
plot(rownames(wdays.L),wdays.L$uss,type="l",xlab="Year",ylab="Number of whale days", lwd=2,bty="l", main="L sightings, 1978-2017", ylim=c(0,200), col="darkred")
lines(rownames(wdays.L),wdays.L$ps, lwd=2,lty=2, col="darkred")
lines(rownames(wdays.L),wdays.L$oc, lwd=2,lty=3, col="darkred")







#Below doesn't work well....
#par(mfrow)
#start with uss, all SRKWs
years<-unique(orcasum.days$year)
pal <- colorRampPalette(c("black", "white"))
colors<-pal(40)
quartz()
par(mfrow=c(5,8))
#yrdat<-orcasum.days[orcasum.days$year==years[1],]
#uss<-yrdat[yrdat$region=="uss",]
#plot(yrdat$day[yrdat$region=="uss"],yrdat$AllSRpres[yrdat$region=="uss"],type="p",xlab="Year",ylab="Presence", pch=21,bg=alpha(colors[1], 0.4),bty="l", main="All SRKW, USS sightings, 1978-2017")
#g = gam(AllSRpres~s(as.numeric(day)),family=binomial,data=uss)
#lines(as.numeric(uss$day),g$fitted.values, col=alpha(colors[1], 0.4), lwd=2)
maxprobobs<-rep(NA, times=length(years))
doymaxprobobs<-rep(NA, times=length(years))
for (i in 1:length(years)){
  quartz()
  yrdat<-orcasum.days[orcasum.days$year==years[i],]

uss<-yrdat[yrdat$region=="uss",]
plot(yrdat$day[yrdat$region=="uss"],yrdat$AllSRpres[yrdat$region=="uss"],type="p",xlab="Year",ylab="Presence", pch=21,bg=alpha(colors[i], 0.4),bty="l", main="All SRKW, USS sightings, 1978-2017")

g = gam(AllSRpres~s(as.numeric(day)),family=binomial,data=uss)
lines(as.numeric(uss$day),g$fitted.values,bg=alpha(colors[i], 0.4), lwd=2)
print(years[i]);print(summary(g))
maxprobobs[i]<-max(g$fitted.values)
doymaxprobobs[i]<-as.numeric(uss$day)[which(g$fitted.values==max(g$fitted.values))]
}
quartz()
plot(years,doymaxprobobs,type="p",pch=21,bg="gray", ylab="DOY",xlab="Year")

summary(lm(doymaxprobobs~as.numeric(years)))


maxprobobs<-rep(NA, times=length(years))
doymaxprobobs<-rep(NA, times=length(years))
for (i in 1:length(years)){
  quartz()
  yrdat<-orcasum.days[orcasum.days$year==years[i],]
  ps<-yrdat[yrdat$region=="ps",]
  if(dim(ps)[1]<9){next}
  plot(yrdat$day[yrdat$region=="ps"],yrdat$AllSRpres[yrdat$region=="ps"],type="p",xlab="Year",ylab="Presence", pch=21,bg=alpha(colors[i], 0.4),bty="l", main="All SRKW, PS sightings")
  
  g = gam(AllSRpres~s(as.numeric(day)),family=binomial,data=ps)
  lines(as.numeric(ps$day),g$fitted.values,bg=alpha(colors[i], 0.4), lwd=2)
  print(years[i]);print(summary(g))
  maxprobobs[i]<-max(g$fitted.values)
  doymaxprobobs[i]<-as.numeric(uss$day)[which(g$fitted.values==max(g$fitted.values))]
}
quartz()
plot(years,doymaxprobobs,type="p",pch=21,bg="gray", ylab="DOY",xlab="Year")

summary(lm(doymaxprobobs~as.numeric(years)))
