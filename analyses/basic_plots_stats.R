
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
d<-d[d$FishArea %in% c("01","02","03","04","05","06","07","09","10","11","12","13","81","82","19C","18C","29C","20C"),]#not sure where 17, 18, 19, 20, 28, 29 are...need to find out. also, where is 42583,42584
#remove sites with no fishing area:
d<-d[!d$FishArea %in% c(""),]

#Assign region, based on fishing area:
d$region<-"ps"
d$region[d$FishArea=="07"|d$FishArea=="06"|d$FishArea=="05"|d$FishArea=="04"|d$FishArea=="19C"|d$FishArea== "18C"|d$FishArea=="20C"]<-"uss"
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
d$Trans<-0
d$Trans[grep("T",d$Pod.cl)]<- 1

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
#legend("topleft",legend=c("Upper Salish Sea","Puget Sound","Outer Coast"), lty=c(1,2,3),col=c("black"), bty="n")


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

#2. Plot the number of "whale days" (days on which whales were observed in each region)
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
dim(wdays)#40 years
colMeans(wdays[1:20,], na.rm=TRUE)

quartz()
par(mai=c(1,1,1,1.5))
#start with uss, all SRKWs
plot(rownames(wdays),wdays$uss,type="l",xlab="Year",ylab="Number of whale days", lwd=2,bty="l", main="All SRKW sightings, 1978-2017", ylim=c(0,200))
lines(rownames(wdays),wdays$ps, lwd=2,lty=2)
lines(rownames(wdays),wdays$oc, lwd=2,lty=3)
legend("topleft",legend=c("Upper Salish Sea","Puget Sound","Outer Coast"), lty=c(1,2,3),col="black", bty="n")
#Add dates of some significant events to the plot
#"Dyes inlet event" 1997
#abline(v=1997, col="blue")
#text("Dyes inlet event",x=2000,y=200, col="blue",cex=0.8 )
#Internet sightsing 
abline(v=2001, col="purple")
text("Internet sightings added",x=2008,y=190, col="purple",cex=0.8 )

#Is there a trend in number of days on which whales observed since 1978?
#
m.uss<-lm(wdays$uss~as.numeric(rownames(wdays)))
m.ps<-lm(wdays$ps~as.numeric(rownames(wdays)))
m.oc<-lm(wdays$oc~as.numeric(rownames(wdays)))
abline(m.uss, col="darkred", lwd=2)
abline(m.ps, col="darkred", lwd=2, lty=2)
abline(m.oc, col="darkred", lwd=2, lty=3)
mtext(paste((round(m.uss$coef[2], digits=2)*10),"days/dec"), line=-5.5, adj=1.3,cex=0.9)
mtext(paste((round(m.ps$coef[2], digits=2)*10),"days/dec"), line=-20.5, adj=1.3,cex=0.9)
mtext(paste((round(m.oc$coef[2], digits=2)*10),"days/dec"), line=-23.5, adj=1.3,cex=0.9)

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


#Plots observed or not, by doy and year
#yaxis is year
#xaxis is doy
#do for 3 regions
podcols<-c("Jpres", "Kpres", "Lpres", "AllSRpres")
pods<-c("J","K","L","SRs")
for(p in 1:length(podcols)){
  quartz(width=16,height=6)
  par(omi=c(.5,2,.5,.5), mfrow=c(1,3))
  colnum<-which(colnames(orcasum.days)==podcols[p])
  regions=unique(orcasum.days$region)
    for(r in regions){
    regdat<-orcasum.days[orcasum.days$region==r,]
    years = unique(orcasum.days$year)
    plot(0, type = 'n', las=1, xlim=c(1,366),ylim=c(min(as.numeric(years)),max(as.numeric(years))),ylab="",xlab="Day of year", main=paste(r), cex.axis=1.1, cex.lab=1.3)
      for(y in years){
        yrdat = regdat[regdat$year==y,]
        days = yrdat$day[yrdat[,colnum]==1]
      points(x=days,y=rep(y,length=length(days)), pch=21,bg="gray", cex=1.3)
      
      #lines(x=days,y=rep(y,length=length(days)), lwd=2)
      }  
    if(r=="uss"){mtext(paste(pods[p]), side=3,line=3, adj=0.5)}
    if(r=="ps"){mtext("Year", side=2,line=4, adj=0.5)}
    
    }
}
unique()
#summary of the number of days whales were observed in each region, by year:
 pres<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region, orcasum.days$year),sum)
 #summary of the number of days whales with rows of data (observed or not) in each region, by year:
 totobs<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region, orcasum.days$year),length)
 prob<-pres/totobs
 #summary of the number of days whales were observed in each region, across all years:
 pres.doy<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region, orcasum.days$day),sum)
 #summary of the number of days whales with rows of data (observed or not) in each region, across all tears:
 totobs.doy<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region, orcasum.days$day),length)
 prob.doy<-pres.doy/totobs.doy
 quartz()
 par(mfrow=c(2,1))
 plot(colnames(prob.doy),prob.doy[3,], type="l", ylab="Probability of obs.",xlab="DOY", main="USS")
 plot(colnames(prob.doy),prob.doy[2,], type="l", ylab="Probability of obs.",xlab="DOY", main="PS")
 
#Make box plots to see central tendencies:
podcols<-c("Jpres", "Kpres", "Lpres", "AllSRpres")
pods<-c("J","K","L","SRs")
for(p in 1:length(podcols)){
  quartz(width=15,height=6)
  par(mfrow=c(1,3))
  colnum<-which(colnames(orcasum.days)==podcols[p])
  regions=unique(orcasum.days$region)
  for(r in regions){
    regdat<-orcasum.days[orcasum.days$region==r,]
    if (r=="ps"){
     regdat<-regdat[as.numeric(regdat$day)>273,]#look at data only after Sept 30 for PS
    }
        boxplot(as.numeric(regdat$day)~as.factor(regdat$year),
            horizontal=TRUE,las=1,
            ylab="year",xlab="DOY",main=paste(r))
    
    if(r=="uss"){mtext(paste(pods[p]), side=3,line=2, adj=0.5)}
    
  }
}


#Add days after sept 30:
orcasum.days$daysaftsept30<-NA
orcasum.days$day<-as.numeric(orcasum.days$day)
orcasum.days$year<-as.numeric(orcasum.days$year)

orcasum.days$daysaftsept30[which(orcasum.days$day>273 & orcasum.days$day<367)]<-orcasum.days$day[which(orcasum.days$day>273 & orcasum.days$day<367)]-273
orcasum.days$daysaftsept30[which(orcasum.days$day<274)]<-orcasum.days$day[which(orcasum.days$day<274)]+93#
#add an "orca year" which runs Oct 1-Sept 31
orcasum.days$orcayear<-orcasum.days$year
orcasum.days$orcayear[which(orcasum.days$day>273)]<-orcasum.days$year[which(orcasum.days$day>273)]+1


#See if there is a shift in first, last, mean  by decade
regions=unique(orcasum.days$region)
podcols<-c("Jpres", "Kpres", "Lpres", "AllSRpres")
pods<-c("J","K","L","SRs")
years<-unique(orcasum.days$year)
pods.all<-c()
regions.all<-c()
years.all<-c()
nobs.all<-c()
firstest.all<-c()
lastest.all<-c()
meanest.all<-c()
firstest.sept30.all<-c()
lastest.sept30.all<-c()
meanest.sept30.all<-c()
for(p in 1:length(podcols)){
  colnum<-which(colnames(orcasum.days)==podcols[p])
  for(r in 1:length(regions)){
    regdat<-orcasum.days[orcasum.days$region==regions[r],]
    if (regions[r]=="ps"){
      regdat<-regdat[as.numeric(regdat$day)>273,]#look at data only after Sept 30 for PS
    }
    if (regions[r]=="uss"){
      regdat<-regdat[as.numeric(regdat$day)<273,]#look at data before Sept 30 for USS
      regdat<-regdat[as.numeric(regdat$day)>121,]#look at data after MAy 1 for USS
      
    }
    
    for(y in 1:length(years)){
      yrdat<-regdat[regdat$year==years[y],]
      pods.all<-c(pods.all,pods[p])
      regions.all<-c(regions.all,regions[r])
      years.all<-c(years.all,years[y])
      nobs.all<-c(nobs.all,length(yrdat$day[yrdat[,colnum]==1]))
      firstest.all<-c(firstest.all,min(yrdat$day[yrdat[,colnum]==1], na.rm=TRUE))
      lastest.all<-c(lastest.all,max(yrdat$day[yrdat[,colnum]==1], na.rm=TRUE))
      meanest.all<-c(meanest.all,mean(as.numeric(yrdat$day[yrdat[,colnum]==1]), na.rm=TRUE))
      orcayrdat<-regdat[regdat$orcayear==years[y],]
      firstest.sept30.all<-c(firstest.sept30.all,min(orcayrdat$daysaftsept30[orcayrdat[,colnum]==1], na.rm=TRUE))
      lastest.sept30.all<-c(lastest.sept30.all,max(orcayrdat$daysaftsept30[orcayrdat[,colnum]==1], na.rm=TRUE))
      meanest.sept30.all<-c(meanest.sept30.all,mean(as.numeric(orcayrdat$daysaftsept30[orcayrdat[,colnum]==1]), na.rm=TRUE))
    }
  }
}
df <- as.data.frame(cbind(pods.all,regions.all,years.all,nobs.all,firstest.all,lastest.all,meanest.all,firstest.sept30.all,lastest.sept30.all,meanest.sept30.all))
colnames(df)<-c("pod","region","year","nobs","firstest","lastest","meanest","firstest.sept30","lastest.sept30","meanest.sept30") 

#Now fit some linear models and plots

quartz()
par(mfrow=c(3,4))
pod.df=df[df$pod=="SRs",]
pod.df$firstest[which(pod.df$firstest=="Inf")]<-NA
pod.df$lastest[which(pod.df$lastest=="-Inf")]<-NA
pod.df$firstest<-as.numeric(pod.df$firstest)
pod.df$lastest<-as.numeric(pod.df$lastest)
pod.df$meanest=as.numeric(pod.df$meanest)

pod.df$firstest.sept30[which(pod.df$firstest.sept30=="Inf")]<-NA
pod.df$lastest.sept30[which(pod.df$lastest.sept30=="-Inf")]<-NA
pod.df$duration<-pod.df$lastest-pod.df$firstest
for(i in 1:length(regions)){
  reg.df=pod.df[pod.df$region==regions[i],]
  reg.df$year=as.numeric(reg.df$year)
 
  #if(regions[i]=="ps" & unique(reg.df$pod)=="SRs"){reg.df<-reg.df[1:40,]}
  #if(regions[i]=="uss"& unique(reg.df$pod)=="SRs"){reg.df[81:120,]}
  #first obs
  plot(reg.df$year,reg.df$firstest,xlab="year",ylab="first obs doy", main=paste(unique(reg.df$pod)), bty="l", pch=21, bg="gray")
  mod<-lm(reg.df$firstest~reg.df$year)
  abline(mod)
  mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
  
  #last obs
  plot(reg.df$year,reg.df$lastest,xlab="year",ylab="last obs doy", main=paste(regions[i]), bty="l", pch=21, bg="gray")
  mod<-lm(reg.df$lastest~reg.df$year)
  mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  abline(mod)
  mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
  
  #mean obs
  plot(reg.df$year,reg.df$meanest,xlab="year",ylab="mean obs doy", main="", bty="l", pch=21, bg="gray")
  mod<-lm(reg.df$meanest~reg.df$year)
  mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  abline(mod)
  mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
  
  #duration
  plot(reg.df$year,reg.df$duration,xlab="year",ylab="duration (days)", main="", bty="l", pch=21, bg="gray")
  mod<-lm(reg.df$duration~reg.df$year)
  mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
  
  abline(mod)
  
 #using days since sept 30 does not change much!
  #first obs after sept 30
  #plot(reg.df$year,reg.df$firstest.sept30,xlab="year",ylab="first obs day after sept 30", main="all srs", bty="l", pch=21, bg="gray")
  #mod<-lm(reg.df$firstest.sept30~reg.df$year)
  #abline(mod)
  #mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  #last obs days after sept 30
  #plot(reg.df$year,reg.df$lastest.sept30,xlab="year",ylab="last obs day after sept 30", main=paste(regions[i]), bty="l", pch=21, bg="gray")
  #mod<-lm(reg.df$lastest.sept30~reg.df$year)
  #mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  #abline(mod)
  #mean obs
  #plot(reg.df$year,reg.df$meanest.sept30,xlab="year",ylab="mean obs day after sept 30", main="", bty="l", pch=21, bg="gray")
  #mod<-lm(reg.df$meanest.sept30~reg.df$year)
  #mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  #abline(mod)

}

#Make boxplots similar to the way that i made them for test data
cbind(df$nobs[df$pod=="SRs"],df$year[df$pod=="SRs"])
pod.df=df[df$pod=="SRs",]
pod.df$firstest[which(pod.df$firstest=="Inf")]<-NA
pod.df$lastest[which(pod.df$lastest=="-Inf")]<-NA
pod.df$firstest<-as.numeric(pod.df$firstest)
pod.df$lastest<-as.numeric(pod.df$lastest)
pod.df$meanest=as.numeric(pod.df$meanest)

pod.df$decade<-"1978-1987"
pod.df$decade[pod.df$year>1987]<-"1988-1997"
pod.df$decade[pod.df$year>1997]<-"1998-2007"
pod.df$decade[pod.df$year>2007]<-"2008-2017"
pod.df$period<-"1978-1997"
pod.df$period[pod.df$year>1997]<-"1998-2017"

quartz()
par(mfrow=c(1,2))
#First obs
boxplot(as.numeric(pod.df$firstest[pod.df$region=="ps"])~as.factor(pod.df$decade[pod.df$region=="ps"]), xlab="Decade", ylab="Estimate of first obs (doy) in PS", main="First obs")
#Last obs
boxplot(as.numeric(pod.df$lastest[pod.df$region=="ps"])~as.factor(pod.df$decade[pod.df$region=="ps"]), xlab="Decade", ylab="Estimate of last obs (doy) in PS", main="Last obs")

quartz()
par(mfrow=c(1,2))
#First obs
boxplot(as.numeric(pod.df$firstest[pod.df$region=="ps"])~as.factor(pod.df$period[pod.df$region=="ps"]), xlab="Period", ylab="Estimate of first obs (doy) in PS", main="First obs")
t<-t.test(as.numeric(pod.df$firstest[pod.df$region=="ps"])~pod.df$period[pod.df$region=="ps"], paired = FALSE, var.equal = FALSE,conf.level=0.95)
mtext(paste("Change=",-1*round(t$estimate[1]-t$estimate[2], digits=1),"(",-1*round(t$conf.int[1],digits=1),",",-1*round(t$conf.int[2],digits=1),")", sep=""),side=3,line=-3, adj=1)

#Last obs
boxplot(as.numeric(pod.df$lastest[pod.df$region=="ps"])~as.factor(pod.df$period[pod.df$region=="ps"]), xlab="Period", ylab="Estimate of last obs (doy) in PS", main="Last obs")
t<-t.test(as.numeric(pod.df$lastest[pod.df$region=="ps"])~as.factor(pod.df$period[pod.df$region=="ps"]), conf.level=0.95)
mtext(paste("Change=",-1*round(t$estimate[1]-t$estimate[2], digits=1),"(",-1*round(t$conf.int[1],digits=1),",",-1*round(t$conf.int[2],digits=1),")", sep=""),side=1,line=-3, adj=1)


##Number of consecutive days
doy<- strptime(paste(orcasum.days$day,orcasum.days$year, sep="-"),format = "%j")
orcasum.days$date<-paste(orcasum.days$year,substr(doy,6,10), sep="-")
runchecker <- function(data, days){
  data %>% arrange(date) %>%
    group_by(AllSRpres) %>%
    mutate(diff = c(0, diff(date)),
           periodID = 1 + cumsum(diff > days)) %>%
    group_by(ID, periodID) %>%
    summarise(days = last(date) - first(date))
}
#p=4 for "ALLSRpres"
for(p in 1:length(podcols)){
  quartz(width=15,height=6)
  par(mfrow=c(1,3))
  colnum<-which(colnames(orcasum.days)==podcols[p])
  regions=unique(orcasum.days$region)
  for(r in regions){
    regdat<-orcasum.days[orcasum.days$region==r,]
    orcaobs<-subset(regdat, select=c(AllSRpres,date))]
    orcaobs$date<-as.Date(orcaobs$date)
    runchecker(orcaobs,1)
    runchecker(orcaobs$AllSRpres,)
  }
  }


df <- orcasum.days[order(orcasum.days$region,orcasum.days$date),] 
df <- df[!duplicated(df[,c("region","date")]),]
df$last_Date <- as.Date(c("1979-08-16 ",df[1:nrow(df)-1,]$date))
df$Date <- as.Date(df$date)
df$diff <- df$Date - df$last_Date 
df$last_region <- c(0,df[1:nrow(df)-1,]$region)
df$diff_region <- ifelse(df$region == df$last_region,0,1)
df$flag <- ifelse(df$diff==1 & df$diff_region==0,0,1)
consecutive_count <- function(x)  {
  x <- !x
  rl <- rle(x)
  len <- rl$lengths
  v <- rl$values
  cumLen <- cumsum(len)
  z <- x
  # replace the 0 at the end of each zero-block in z by the 
  # negative of the length of the preceding 1-block....
  iDrops <- c(0, diff(v)) < 0
  z[ cumLen[ iDrops ] ] <- -len[ c(iDrops[-1],FALSE) ]
  # ... to ensure that the cumsum below does the right thing.
  # We zap the cumsum with x so only the cumsums for the 1-blocks survive:
  x*cumsum(z)
}
df$Consecutive <- consecutive_count(df$flag)

podcols<-c("Jpres", "Kpres", "Lpres", "AllSRpres")
pods<-c("J","K","L","SRs")

    if (r=="ps"){
      regdat<-regdat[as.numeric(regdat$day)>273,]#look at data only after Sept 30 for PS
    }
    boxplot(as.numeric(regdat$day)~as.factor(regdat$year),
            horizontal=TRUE,las=1,
            ylab="year",xlab="DOY",main=paste(r))
    
    if(r=="uss"){mtext(paste(pods[p]), side=3,line=2, adj=0.5)}
    
  }
}

