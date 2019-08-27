
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
library(RColorBrewer)
library(rworldmap)
library(scales)
library(matrixStats)
# 1. Choose the years, regions of interest, assumption about reports in the OrcaMaster and get the data
includeCanada=TRUE
firstyear=1976#probably set to 1975 or 1976 (Olson et al)
assumeSRKW=TRUE #If true, assume that "Orcas" means SRKW unless noted otherwuse (i.e. Transients or NRKWs)
use3regions=FALSE#If true, separate out the straight of Juan de Fuca as a 3rd region, distinct from CSS and PS (all code not yet working for 3 regions!)
d <- read.csv("data/AppendixII.csv")
quads<-read.csv("data/QuadCentroids.csv")
dim(d)#105344     18 on August 8, 2019

# 2. Clean the data (also saved in output/AppendixII_cleaned,csv)
source("analyses/orcaphen/source/clean_orca.R")
dim(d)#105339     21 on August 8, 2019


# 3. Limit space and time to firstyear or later and Salish Sea, Puget Sound, Washington Outer Coast 
source("analyses/orcaphen/source/orca_limitspacetime.R")
dim(d)#103289     22
#table(d$FishArea,d$region)#check regions are correct

#4. Get data in terms of number of observations per day and "whale days": days on which whales were seen (presence/absence for each day)
source("analyses/orcaphen/source/orca_get_whaledays.R")

#5. Summarize and plot whale days 
wdays<-as.data.frame(tapply(orcasum.days$AllSRpres,list(orcasum.days$year,orcasum.days$region),sum))
wdays.J<-as.data.frame(tapply(orcasum.days$Jpres,list(orcasum.days$year,orcasum.days$region),sum))
wdays.K<-as.data.frame(tapply(orcasum.days$Kpres,list(orcasum.days$year,orcasum.days$region),sum))
wdays.L<-as.data.frame(tapply(orcasum.days$Lpres,list(orcasum.days$year,orcasum.days$region),sum))
source("analyses/orcaphen/source/orca_plot_whaledays.R")

#6. if you want to do some other basic plots of the data. This includes proportion of days in a week in which whales were observed up by week and decade
#source("analyses/orcaphen/source/orca_plotdata.R")

#7. Make a map of the SRKW sightings in ps and uss
source("analyses/orcaphen/source/orca_makemap.R")
#Take home from the above plots: Seasonality is hard to see in the figure with a separate line for each decade. I also plotted them with a single line showing the mean across all decades, and I standardized the proportions by substracting the means and dividing by the standard deviation. 
#I did this to try to make seasonal patterns more obvious. It only sort of made them more obvious. Here's what I take away from these figures:
#1) There is a very clear seasonal pattern in the SRKW use of the upper salish sea. This season ranges roughly from week 20 (mid May) through week 40 (early October).
#2) The seasonal pattern in the SRKW use is less obvious for Puget Sound, because the maximum proportion is lower over all. However, the season during which the proportion is consistently above the mean probability
#ranges from week 40 (early October) through week 2 (early January)

#use these for whale seasons to investigate shifts over time
#use apr 1 for uss season, aug 1 for ps season as start dates
#use oct 31 for uss season, jan31 for ps season, as end dates

#8. Fit some basic linear models to all srkw data
regions=unique(orcasum.days$region)
podcols<-c("Jpres","Kpres","Lpres","AllSRpres")
pods<-c("J", "K","L","SRs")
source("analyses/orcaphen/source/orca_runlinmods.R")

#9. Fit some basic linear models to lime kiln data only
limed<-d[d$Source=="TWM-Otis",]
source("analyses/orcaphen/source/orca_runlinmods_lime.R")

#10. Fit some basic linear models to west seattle only
searsd<-read.csv("data/SearsCompiled.csv")
#first clean the data and get in same format as orcamaster
source("analyses/orcaphen/source/clean_sears.R")

source("analyses/orcaphen/source/orca_runlinmods_sears.R")

#11.Try the pearse approach to compare to using data at face-value for estimating first/last events
#lime.df contains first, last, mean dates from data face value
#orcasum.days.lime contains whale days
#unique(orcasum.days.lime$AllSRpres)#all presence
source("analyses/orcaphen/source/pearse_fxns.R")
alpha=0.10
k=20
pods.all.p<-c()
regions.all.p<-c()
years.all.p<-c()
nobs.all.p<-c()
firstest.all.p<-c()
lastest.all.p<-c()
mean.all.p<-c()
meandiffs.all<-c()
firstdiffs.all<-c()
years<-unique(orcasum.days.lime$orcayear)
p=1
for(p in 1:length(podcols)){
  colnum<-which(colnames(orcasum.days.lime)==podcols[p])
  
  regdat<-orcasum.days.lime
  for(y in 1:length(years)){
    yrdat<-regdat[regdat$orcayear==years[y],]
    pods.all.p<-c(pods.all.p,pods[p])
    years.all.p<-c(years.all.p,years[y])
    nobs.all.p<-c(nobs.all.p,length(yrdat$day[yrdat[,colnum]==1]))
    
    firstest.all.p<-rbind(firstest.all.p,est.limit(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]),alpha=alpha,k=k))
    lastest.all.p<-rbind(lastest.all.p,est.limit(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]), upper=TRUE,alpha=alpha,k=k))
    mean.all.p<-rbind(mean.all.p,
                      c(mean(yrdat$daysaftmar31[yrdat[,colnum]==1], na.rm=TRUE),#mean
                        quantile(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]),alpha),#lower ci
                        quantile(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]),1-alpha)))#upper ci
  meandiffs.all<-c(meandiffs.all,mean(diff(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]))))
  firstdiffs.all<-c(firstdiffs.all,diff(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]))[1])
  
  }
}

lime.df <- cbind(lime.df,pods.all.p,years.all.p,nobs.all.p,firstest.all.p,lastest.all.p,mean.all.p)

colnames(lime.df)[10:18]<-c("firstest.p","firstest.plcl","firstest.pucl","lastest.p","lastest.plcl","lastest.pucl","mean.p","mean.plcl","mean.pucl")
lime.df$year<-as.numeric(lime.df$year)+1
lime.df$years.all.p<-as.numeric(lime.df$years.all.p)+1

lime.df$firstest.all<-as.numeric(lime.df$firstest.all)
lime.df$lastest.all<-as.numeric(lime.df$lastest.all)
lime.df$nobs<-as.numeric(lime.df$nobs)
#plot to compare
lime.df<-lime.df[-1,]
meandiffs.all<-meandiffs.all[-1]
quartz(height=8,width=20)
par(mfrow=c(1,5))
plot(lime.df$year,lime.df$firstest.all,type="p",pch=16, col = "black",xlab="Year",ylab="Days after March 31", cex=1.2, bty="l", ylim=c(25,70), main = "Arrival")
arrows(lime.df$year,lime.df$firstest.plcl,lime.df$year,lime.df$firstest.pucl,col="blue",code=3,length=0)
points(lime.df$years.all.p,lime.df$firstest.p,pch=16, col = "blue",cex=1.2)
mod<-lm(lime.df$firstest.all~lime.df$year)
if(summary(mod)$coef[2,4]<alpha){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]>alpha){abline(mod, lty=3)}
mod<-lm(lime.df$firstest.p~lime.df$year)
if(summary(mod)$coef[2,4]<alpha){abline(mod, lty=1, col="blue")}
if(summary(mod)$coef[2,4]>alpha){abline(mod, lty=3, col="blue")}

plot(lime.df$year,lime.df$lastest.all,type="p",pch=16, col = "black",xlab="Year",ylab="Days after March 31", cex=1.2, bty="l", ylim=c(110,190), main = "Departure")
arrows(lime.df$year,lime.df$lastest.plcl,lime.df$year,lime.df$lastest.pucl,col="blue",code=3,length=0)
points(lime.df$year,lime.df$lastest.p,pch=16, col = "blue",cex=1.2)
mod<-lm(lime.df$lastest.all~lime.df$year)
if(summary(mod)$coef[2,4]<alpha){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]>alpha){abline(mod, lty=3)}
mod<-lm(lime.df$lastest.p~lime.df$year)
if(summary(mod)$coef[2,4]<alpha){abline(mod, lty=1, col="blue")}
if(summary(mod)$coef[2,4]>alpha){abline(mod, lty=3, col="blue")}

plot(lime.df$year,lime.df$mean.p,type="p",pch=16, col = "black",xlab="Year",ylab="Days after March 31", cex=1.2, bty="l", ylim=c(50,150), main = "Mean")
arrows(lime.df$year,lime.df$mean.plcl,lime.df$year,lime.df$mean.pucl,col="blue",code=3,length=0)
points(lime.df$year,lime.df$mean.p,pch=16, col = "blue",cex=1.2)
mod<-lm(lime.df$mean.p~lime.df$year)
if(summary(mod)$coef[2,4]<alpha){abline(mod, lty=1, col="blue")}
if(summary(mod)$coef[2,4]>alpha){abline(mod, lty=3, col="blue")}

plot(lime.df$year,meandiffs.all,type="p",pch=16, col = "red",xlab="Year",ylab="Mean Days Between Sightings", cex=1.2, bty="l", ylim=c(1,10), main = "Mean difference")
#plot(lime.df$year,firstdiffs.all,type="p",pch=16, col = "red",xlab="Year",ylab="Days Between First 2 Sightings", cex=1.2, bty="l", ylim=c(1,10), main = "First difference")
mod<-lm(meandiffs.all~lime.df$year)
if(summary(mod)$coef[2,4]<alpha){abline(mod, lty=1, col="red")}
if(summary(mod)$coef[2,4]>alpha){abline(mod, lty=3, col="red")}

plot(lime.df$year,lime.df$nobs,type="p",pch=16, col = "black",xlab="Year",ylab="# days", cex=1.2, bty="l", ylim=c(1,70), main = "Whale Days")
mod<-lm(lime.df$nobs~lime.df$year)
if(summary(mod)$coef[2,4]<alpha){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]>alpha){abline(mod, lty=3)}

#firstlastmod<-lm(lastest.all[2:dim(lime.df)[1]]~firstest.all[2:dim(lime.df)[1]], data=lime.df)
#firstlastestmod<-lm(lastest.p[2:dim(lime.df)[1]]~firstest.p[2:dim(lime.df)[1]], data=lime.df)
#removing first value doesn't have much effect
firstlastmod<-lm(lastest.all~firstest.all, data=lime.df)
firstlastestmod<-lm(lastest.p~firstest.p, data=lime.df)

summary(firstlastmod)
summary(firstlastestmod)

daysfirstmod<-lm(firstest.all~nobs, data=lime.df)
dayslastmod<-lm(lastest.all~nobs, data=lime.df)
daysfirstestmod<-lm(firstest.p~nobs, data=lime.df)
dayslastestmod<-lm(lastest.p~nobs, data=lime.df)

summary(daysfirstmod)#later first obs in years with few whale days
summary(dayslastmod)#earlier last obs in years with few whale days
summary(daysfirstestmod)#later first obs in years with few whale days
summary(dayslastestmod)#earlier last obs in years with few whale days
#estimates are more conservative...

#Relate limekiln data to chinook phenology in the Fraser river
albchin<-read.csv("analyses/output/albionchiphen.csv", header = TRUE)
#restrict to years that we have SRKW data for
albchin95<-albchin[albchin$year>1993  & albchin$year<2018,]
albchin95<-albchin95[-which(albchin95$year==2014),]
quartz(height=8,width=25)
par(mfrow=c(1,6))
plot(albchin95$firstobsdate,lime.df$firstest.all,type="p",pch=16, col = "black",xlab="Chinook Arrival DOY",ylim = c(40,70),ylab="SRKW Arrival DOY", cex=1.2, bty="l")
#what is the really late salmon year?
#albchin95$year[which(albchin95$firstobsdate==max(albchin95$firstobsdate, na.rm=TRUE))]#2007
mod<-lm(lime.df$firstest.all~albchin95$firstobsdate)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3)}
points(albchin95$firstobsdate,lime.df$firstest.p,pch=16, col = "blue",cex=1.2)
mod<-lm(lime.df$firstest.p~albchin95$firstobsdate)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "blue")}

#First obs of SRKW vs total run size
plot(albchin95$alltotal,lime.df$firstest.all,type="p",pch=16, col = "black",xlab="Chinook Run Size",ylim = c(40,70),ylab="SRKW Arrival DOY", cex=1.2, bty="l")
mod<-lm(lime.df$firstest.all~albchin95$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]<.1){abline(mod, lty=3)}
points(albchin95$alltotal,lime.df$firstest.p,pch=16, col = "blue",cex=1.2)
mod<-lm(lime.df$firstest.p~albchin95$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "blue")}


#First obs of SRKW vs peak run date
plot(albchin95$peakobsdate,lime.df$firstest.all,type="p",pch=16, col = "black",xlab="Chinook Peak DOY",ylim = c(40,70),ylab="SRKW Arrival DOY", cex=1.2, bty="l")
mod<-lm(lime.df$firstest.all~albchin95$peakobsdate)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3)}
points(albchin95$peakobsdate,lime.df$firstest.p,pch=16, col = "blue",cex=1.2)
mod<-lm(lime.df$firstest.p~albchin95$peakobsdate)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "blue")}


#Last obs of SRKW vs last obs of salmon
plot(albchin95$lastobsdate,lime.df$lastest.all,type="p",pch=16, col = "black",xlab="Chinook Last Obs DOY",ylab="SRKW Departure DOY", cex=1.2, bty="l")
mod<-lm(lime.df$lastest.all~albchin95$lastobsdate)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3)}
points(albchin95$lastobsdate,lime.df$lastest.p,pch=16, col = "blue",cex=1.2)
mod<-lm(lime.df$lastest.p~albchin95$lastobsdate)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "blue")}

#Lastobs of SRKW vs total run size
plot(albchin95$alltotal,lime.df$lastest.all,type="p",pch=16, col = "black",xlab="Chinook Run Size",ylab="SRKW Departure", cex=1.2, bty="l")
mod<-lm(lime.df$lastest.all~albchin95$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3)}
points(albchin95$alltotal,lime.df$lastest.p,pch=16, col = "blue",cex=1.2)
mod<-lm(lime.df$lastest.p~albchin95$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "blue")}

#whale days vs chinook run size
plot(albchin95$alltotal,lime.df$nobs,type="p",pch=16, col = "black",xlab="Chinook Run Size",ylab="Whale days", cex=1.2, bty="l")
mod<-lm(lime.df$nobs~albchin95$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3)}

#Now try for sears data
alpha=0.10
k=20
pods.all.p<-c()
regions.all.p<-c()
years.all.p<-c()
nobs.all.p<-c()
firstest.all.p<-c()
lastest.all.p<-c()
mean.all.p<-c()
meandiffs.all<-c()
firstdiffs.all<-c()
years<-unique(sears.df$years.all)
p=1
for(p in 1:length(podcols)){
  colnum<-which(colnames(orcasum.days.lime)==podcols[p])
  
  regdat<-orcasum.days.sears
  for(y in 1:length(years)){
    yrdat<-regdat[regdat$orcayear==years[y],]
    pods.all.p<-c(pods.all.p,pods[p])
    years.all.p<-c(years.all.p,years[y])
    nobs.all.p<-c(nobs.all.p,length(yrdat$day[yrdat[,colnum]==1]))
    
    firstest.all.p<-rbind(firstest.all.p,est.limit(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]),alpha=alpha,k=k))
    lastest.all.p<-rbind(lastest.all.p,est.limit(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]), upper=TRUE,alpha=alpha,k=k))
    mean.all.p<-rbind(mean.all.p,
                      c(mean(yrdat$daysaftmar31[yrdat[,colnum]==1], na.rm=TRUE),#mean
                        quantile(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]),alpha),#lower ci
                        quantile(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]),1-alpha)))#upper ci
    meandiffs.all<-c(meandiffs.all,mean(diff(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]))))
    firstdiffs.all<-c(firstdiffs.all,diff(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]))[1])
    
  }
}

sears.df <- cbind(sears.df,pods.all.p,years.all.p,nobs.all.p,firstest.all.p,lastest.all.p,mean.all.p)
sears.df$years.all<-as.numeric(sears.df$years.all)
sears.df$firstest.all<-as.numeric(sears.df$firstest.all)
quartz(height=8,width=25)
par(mfrow=c(1,6))

plot(sears.df$years.all,sears.df$firstest.all,xlab="year",ylab="first obs doy", main="", bty="l", pch=16)
mod<-lm(sears.df$firstest.all~sears.df$years.all)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]<.1){abline(mod, lty=3)}
mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
mod<-lm(lime.df$firstest.p~lime.df$year)
if(summary(mod)$coef[2,4]<alpha){abline(mod, lty=1, col="blue")}
if(summary(mod)$coef[2,4]>alpha){abline(mod, lty=3, col="blue")}


plot(df$year,df$lastest.all,xlab="year",ylab="last obs doy", main="", bty="l", pch=21, bg="gray")
mod<-lm(df$lastest.all~df$year)
abline(mod, lty=2)
mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)

plot(df$year,df$mean.all,xlab="year",ylab="mean obs doy", main="", bty="l", pch=21, bg="gray")
mod<-lm(df$mean.all~df$year)
abline(mod)
mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
