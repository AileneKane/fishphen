#Look at some basic stats about orca observations
#Started with orca_dataprep_occmodel.R code
#4 February 2019

#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Set working directory: 
#setwd("~/GitHub/fishphen")
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
library(plotfunctions)
library(igraph)
library(brms)
# 1. Choose the years, regions of interest, assumption about reports in the OrcaMaster and get the data
includeCanada=TRUE
firstyear=1976#probably set to 1975 or 1976 (Olson et al)
assumeSRKW=FALSE #If true, assume that "Orcas" means SRKW unless noted otherwuse (i.e. Transients or NRKWs)
use3regions=FALSE#If true, separate out the straight of Juan de Fuca as a 3rd region, distinct from CSS and PS (all code not yet working for 3 regions!)
#Set start of seasons
ps.start<-182#July 1 = 182
uss.start<-91#April 1 = 91, 


d <- read.csv("data/AppendixII.csv")
quads<-read.csv("data/QuadCentroids.csv")
dim(d)#105344  18 on Nov 08, 2019

# 2. Clean the data (also saved in output/AppendixII_cleaned,csv)
source("analyses/orcaphen/source/clean_orca.R")
dim(d)#105339     21 on Nov 8, 2019


# 3. Limit space and time to firstyear or later and Salish Sea, Puget Sound, Washington Outer Coast 
source("analyses/orcaphen/source/orca_limitspacetime.R")
dim(d)#103121      22
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
source("analyses/orcaphen/source/orca_plotdata.R")
#To fix in above: remove all quartz() and windwos() commands


#7. Make a map of the SRKW sightings in ps and uss
source("analyses/orcaphen/source/orca_makemap.R")
#To fix in above: check code and remove extraneous code

#use these for whale seasons to investigate shifts over time
#use apr 1 for uss season, jul 1 for ps season as start dates
#use oct 31 for uss season, jan31 for ps season, as end dates

#8a. Prep the lime kiln only data for either gams or linear models
source("analyses/orcaphen/source/orca_get_whaledays_lime.R")
 
#8b. Fit some basic linear models to all srkw data
styr=1978#or 2001
alph<-0.05
source("analyses/orcaphen/source/orca_runlinmods.R")
#the above code also includes simulated data- check this and add to supplement!

#9. Fit some basic linear models to lime kiln data only
#alph = 0.95
#To do: review above and see if it is necessary or useful. fix errors

#10. Fit some basic linear models to west seattle only
#searsd<-read.csv("data/SearsCompiled.csv")
#first clean the data and get in same format as orcamaster
#source("analyses/orcaphen/source/clean_sears.R")

#source("analyses/orcaphen/source/orca_runlinmods_sears.R")

#11.Try the pearse approach to compare to using data at face-value for estimating first/last events
#lime.df contains first, last, mean dates from data face value
#orcasum.days.lime contains whale days
#unique(orcasum.days.lime$AllSRpres)#all presence
#source("analyses/orcaphen/source/pearse_fxns.R")
 #alpha=0.10

# k=20
 #source("analyses/orcaphen/source/orca_runlinmods_lime_pearse.R")
 source("analyses/orcaphen/source/orca_runlinmods_lime.R")
 
#12. Relate limekiln data to chinook phenology in the Fraser river
albchin<-read.csv("analyses/output/albionchiphen_allyear.csv", header = TRUE)
albchinest<-read.csv("analyses/output/albionchiphenbrms.csv", header = TRUE)

#restrict to years that we have SRKW data for

albchin95<-albchin[albchin$year>1993  & albchin$year<2018,]
albchin95<-albchin95[-which(albchin95$year==2014),]
albchinest95<-albchinest[albchinest$year>1993  & albchinest$year<2018,]
albchinest95<-albchinest95[-which(albchinest95$year==2014),]

#Plot curves of salmon abundance vs day vs mean whale day

chinab<-read.csv("analyses/output/albiongamests.csv", header = TRUE)
chinab.old<-chinab[chinab$year>1993 & chinab$year<2006,] 
chinab.rec<-chinab[chinab$year>=2006 & chinab$year<2018,] 
cpue.old<-cbind(aggregate(chinab.old$cpue,by=list(chinab.old$doy),mean), aggregate(chinab.old$cpue,by=list(chinab.old$doy),sd)$x)
cpue.rec<-cbind(aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),mean),aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),sd)$x)
colnames(cpue.old)<-colnames(cpue.rec)<-c("doy","cpue.mean","cpue.sd")

#fit gams of srkw prob of presence
#source(orca_rungams_lime.R)#take a long time so just read in model ests
#Below needs work!
limegests<-read.csv("analyses/output/lime_prob.occ.75.csv", header=TRUE)#also 0.90 and 0.95
limegests$year<-rep(unique(lime.df$year), each=75)

pod = "SR"#choices are S,J.K,L
minprob = 0.2
get.gests<-function(limegests,pod){
  peakoc.doy<-c()
  peakoc<-c()
  firstprob<-c()
  lastprob<-c()
  meanprobs<-c()
  prob.lc<-c()
  prob.uc<-c()
  years<-c()
  for(y in unique(limegests$year)){
    yeardat<-limegests[limegests$year==y,]
    podcol<-yeardat[,which(colnames(yeardat)==paste(pod,"prob.Estimate",sep=""))]
    yrpeakoc<-max(podcol)
    yrpeakoc.doy<-yeardat$doy[which(podcol==yrpeakoc)]
    yrfirst.doy<-yeardat$doy[min(which(podcol>minprob))]
    yrlast.doy<-yeardat$doy[max(which(podcol>minprob))]
    meanprob<-mean(podcol)
    lprob<-quantile(podcol,0.25)
    uprob<-quantile(podcol,0.75)
    peakoc<-c(peakoc,yrpeakoc)
    peakoc.doy<-c(peakoc.doy,yrpeakoc.doy)
    firstprob<-c(firstprob,yrfirst.doy)
    lastprob<-c(lastprob,yrlast.doy)
    meanprobs<-c(meanprobs,meanprob)
    prob.lc<-c(prob.lc,lprob)
    prob.uc<-c(prob.uc,uprob)
    years<-c(years,y)
  }
  gests<-as.data.frame(cbind(years,peakoc,peakoc.doy,firstprob,lastprob,meanprobs,prob.lc,prob.uc))
  row.names(gests)<-NULL
  return(gests)
}
gests<-get.gests(limegests,"SR")
jgests<-get.gests(limegests,"J")
kgests<-get.gests(limegests,"K")
lgests<-get.gests(limegests,"L")
gests$years<-as.numeric(gests$years)
gests$peakoc.doy<-as.numeric(gests$peakoc.doy)
jgests$peakoc.doy<-as.numeric(jgests$peakoc.doy)

meanmod<-summary(lm(gests$meanprobs~gests$years))#trend is getting lower

peakmod<-summary(lm(as.numeric(gests$peakoc.doy)~gests$years))#trend is getting later
confint(lm(gests$peakoc.doy~gests$years),level= .80)
summary(lm(jgests$peakoc.doy~jgests$years))# getting later
summary(lm(kgests$peakoc.doy~kgests$years))# getting later
summary(lm(lgests$peakoc.doy~lgests$years))#not getting later

confint(lm(gests$lastprob~gests$year),level= .90)
confint(lm(gests$firstprob~gests$year), level=.90)

#quartz(height=4,width=12)
albchiphenest<-read.csv("analyses/output/albionchiphenest.csv", header=TRUE)

#restrict SRKW and chin data to consistent years
albchinest90<-albchiphenest[albchiphenest$year>1990,]
albchinest90<-albchinest90[albchinest90$year<2017,]
#albchinest90<-albchinest90[albchinest90$year!=1991,]
albchinest90<-albchinest90[albchinest90$year!=1992,]
albchinest90<-albchinest90[albchinest90$year!=2013,]
gests<-gests[gests$years>1991,]
myPalette <- colorRampPalette(brewer.pal(length(unique(albchinest90$year)), "Blues")) #### Gives us a heat map look
cols = rev(myPalette(length(unique(albchinest90$year))))
png(file="analyses/orcaphen/figures/lime_albchin_gam.png",height=1500,width=4500, res = 300)
#quartz()
par(mfrow=c(1,3), mar=c(5, 5, 4, 2) + 0.1)

plot(albchinest90$peakobsdate,gests$peakoc.doy,type="p",pch=21, cex.axis=1.8,cex.lab=1.8,bg = cols[factor(albchinest90$year)],xlab="Day of Peak Chinook Abundance Index (DOY)",ylab="Day of Peak SRKW Occupancy Probability (DOY)", cex=1.8, bty="l")
mod<-lm(gests$peakoc.doy~albchinest90$peakobsdate)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3,  lwd=2)}
mtext("C)", side = 3, line = 1, adj=0)

plot(albchinest90$alltotal,gests$peakoc.doy, type="p",pch=21, cex.axis=1.8,cex.lab=1.8,bg = cols[factor(albchinest90$year)],xlab="Chinook Abundance Index (CPUE)",ylab="Day of Peak SRKW Occupancy Probability (DOY)", cex=1.8, bty="l")
mod<-lm(gests$peakoc.doy~albchinest90$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3,  lwd=2)}
mtext("D)", side = 3, line = 1, adj=0)
#Now calculate Eric's new whale days metric:
limewhaledays.prob<-aggregate(limegests$SRprob.Estimate, by=list(limegests$year), sum)
colnames(limewhaledays.prob)<-c("year","whdays.prob")
limewhaledays.prob$year<-unique(lime.df$year)
limewhaledays.prob<-limewhaledays.prob[limewhaledays.prob$year>1992,]

plot(albchinest90$alltotal,limewhaledays.prob$whdays.prob,type="p",pch=21, bg = cols[factor(albchin95$year)],cex.axis=1.8,cex.lab=1.8,xlab="Chinook Abundance Index (CPUE)",ylab="Whale days (#)", cex=1.8, bty="l")

mod<-lm(limewhaledays.prob$whdays.prob~albchinest90$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3, lwd=2)}
mtext("E)", side = 3, line = 1, adj=0)

dev.off()
