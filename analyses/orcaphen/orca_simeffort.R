#Simulate orca data and vary number of observations to see if increasing effort yields shift in estimate of first, peak, or last observation day.

#First, look at actual data so that we can make our simulation match it:

#Only use fishing areas that have atleast 4 years with >20 observations:
#Look at some basic stats about orca observations
#Started with orca_dataprep_occmodel.R code
#4 February 2019

#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Set working directory: 
setwd("~/GitHub/fishphen")
#or from laptop:
#setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

# Load libraries
library(dplyr)
library(mgcv)
library(scales)
library(RColorBrewer)
# 1. Choose the years, regions of interest, assumption about reports in the OrcaMaster and get the data
includeCanada=TRUE
firstyear= 1976#probably set to 1975 or 1976 (Olson et al)
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


#5. Fit some basic linear models to all srkw data
styr = 2001#choose 1978 or 2001 to coincide with our ms
if(styr == 2001){bkyr = 2009}
if(styr == 1978){bkyr = 1997}
source("analyses/orcaphen/source/orca_runlinmods.R")
#the above code also includes simulated data- check this and add to supplement!

#6. Summarize changes in effort over time
pres<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region, orcasum.days$year),sum)
#summary of the number of days whales with rows of data (observed or not) in each region, by year:
totobs<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region, orcasum.days$year),length)#this is change in effort
prob<-pres/totobs
pres.all<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region),sum)
#summary of the number of days whales with rows of data (observed or not) in each region, by year:
totobs.all<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region),length)#this is change in effort
prob.all<-pres.all/totobs.all
pres.doy<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region, orcasum.days$day),sum)
#summary of the number of days whales with rows of data (observed or not) in each region, across all tears:
totobs.doy<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region, orcasum.days$day),length)
prob.doy<-pres.doy/totobs.doy
#calculation mean probability by season 
orcasum.days$season<-"summer"#(May 1 through Sept 30)
orcasum.days$season[orcasum.days$daysaftmar31>=154]<-"fall"#oct 1 through jan 31)
orcasum.days$season[orcasum.days$daysaftmar31>=277]<-"spring"#(feb 1 through april 30)

#limit to start year
orcasum.days<-orcasum.days[orcasum.days$year>=styr,]

#get probabilities by season

pres.seas<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region, orcasum.days$season),sum)
#summary of the number of days whales with rows of data (observed or not) in each region, by year:
totobs.seas<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region, orcasum.days$season),length)#this is change in effort
prob.seas<-pres.seas/totobs.seas

#summary of effort (the number of days whales with rows of data (observed or not)) in each region, by decade:
totobs.dec<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region,orcasum.days$decade,orcasum.days$season),length)#this is change in effort

#summary of effort (the number of days whales with rows of data (observed or not)) in each region, by period (first two decades versus last two decades):
totobs.pd<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region,orcasum.days$period,orcasum.days$season),length)#this is change in effort
reg=c(TRUE,FALSE)


for (r in 1:length(reg)){
uss=reg[r]#TRUE for uss (upper salish sea), peak prob season is 1 May through 30 sept, and prob obs prob.all[3] (~.76)
       #FALSE  for ps (puget sound), season is 1 Oct through 31 Jan, and prob obs prob.all[2] (~.52)
#biasearly=FALSE#TRUE if you want observations to be biased early in the season
              #FALSE if you want observations to be unbiased
wholeyear=FALSE#look at each region seasonally (not the whole year)


#Create a dataset with a highest probability of seeing the whales in the summer, lowest in the spring and fall
#explore how chaning the number of observations changes our estimates of first, last, peak whale presence
#probability of seeing a whale depends on day of year
#for days 1:180, probability is low (0.10); for days 181-260, proability is higher (0.75); and for days 261-366, probabilty is quite low again
if(uss==TRUE){
  springprob<-prob.seas[which(row.names(prob.seas)=="uss"),2]
  summerprob<-prob.seas[which(row.names(prob.seas)=="uss"),3]
  fallprob<-prob.seas[which(row.names(prob.seas)=="uss"),1]}
if(uss==FALSE){
  springprob<-prob.seas[which(row.names(prob.seas)=="ps"),2]
  summerprob<-prob.seas[which(row.names(prob.seas)=="ps"),3]
  fallprob<-prob.seas[which(row.names(prob.seas)=="ps"),1]}

springdays<-seq(277,366, by=1)
summerdays<-seq(1,153, by=1)
falldays<-seq(154,276, by=1)

#Look at effect of increasing effort on estimate of first observation and estimate of last observation
nreps<-20#e.g. number of years
early<-which(colnames(pres)==styr)

if(uss==TRUE & wholeyear==FALSE){
  lowobs<-as.integer(mean(pres[3,which(colnames(pres)==styr):which(colnames(pres)==bkyr)]))#104#mean whales days/year during the summer first 20 years: uss=104.4
  highobs<-as.integer(mean(pres[3,which(colnames(pres)==bkyr+1):length(colnames(pres))]))##mean whales days during the summer last 10 years: uss=132.9
}

if(uss==FALSE & wholeyear==FALSE){
  lowobs<-as.integer(mean(pres[2,which(colnames(pres)==styr):which(colnames(pres)==bkyr)]))#104#mean whales days/year during the summer first 20 years: uss=104.4
  highobs<-as.integer(mean(pres[2,which(colnames(pres)==bkyr+1):length(colnames(pres))]))##mean whales days during the summer last 10 years: uss=132.9
}

effort<-c(rep(lowobs,nreps),rep(highobs,nreps))
npresreps<-20
sum.df <- as.data.frame(matrix(nrow=npresreps,ncol=7))
colnames(sum.df)<-c("lowobs","hiobs","probobs","firstdif","firstdifp","lastdif","lastdifp")

for(j in 1:npresreps){#create 20 true whale presence datasets
  springobs<-sample(c(0,1), length(springdays), replace=TRUE, prob = c(1-springprob,springprob))
  summerobs<-sample(c(0,1), length(summerdays), replace=TRUE, prob = c(1-summerprob,summerprob))
  fallobs<-sample(c(0,1), length(falldays), replace=TRUE, prob = c(1-fallprob,fallprob))
  #put together the true data
  fallobs<-sample(c(0,1), length(falldays), replace=TRUE, prob = c(1-fallprob,fallprob))
df <- as.data.frame(matrix(nrow=length(effort),ncol=7))
 colnames(df)<-c("numobs","firstest","lastest","meanest","firsttrue", "lasttrue","meantrue")
#for each whale presence dataset, create nreps observation datasets
for (i in 1:length(effort)){
    if(uss==TRUE & wholeyear==FALSE){
      days<-summerdays
      whaleobs<-summerobs
    }
    if(uss==FALSE & wholeyear==FALSE){
      days<-falldays
      whaleobs<-fallobs
    }
    if(wholeyear==TRUE){
    days<-c(springdays,summerdays,falldays)
    whaleobs<-c(springobs,summerobs,fallobs)
    }
    whalepres<-as.data.frame(cbind(days, whaleobs))
    obs<-as.data.frame(whalepres[sample(nrow(whalepres), replace=TRUE,effort[i]), ])#random sample from whale true presence data
  
    #unbiasprop<-14/nrow(whalepres)#first 2 weeks should be 0.14 of full season of observations
    #bias=b #what proportion more observations occur in the first 2 weeks of observation than the rest of the study period
    #biasprop<-unbiasprop*bias
    #earlyobs<-as.data.frame(whalepres[sample(nrow(whalepres), replace=TRUE,biasprop*numobs[i]), ])
    #otherobs<-as.data.frame(whalepres[sample(nrow(whalepres), replace=TRUE,(1-biasprop)*numobs[i]), ])
  #obs<-as.data.frame(rbind(earlyobs,otherobs))
  df$numobs[i]<-effort[i]
  df$firstest[i]<- min(obs$days[obs$whaleobs==1])
  df$lastest[i]<-max(obs$days[obs$whaleobs==1])
  df$meanest[i]<-mean(obs$days[obs$whaleobs==1])
  df$firsttrue[i]<-min(whalepres$days[whalepres$whaleobs==1])
  df$lasttrue[i]<-max(whalepres$days[whalepres$whaleobs==1])
  df$meantrue[i]<-mean(whalepres$days[whalepres$whaleobs==1])
  #look at difference between first and last doy in 2 time periods
  }
 #print(df)
 par(mfrow=c(1,2))
 #First obs)
 boxplot(df$firstest~as.factor(df$numobs), xlab="Number of days observed", ylab="Estimate of first obs (doy)", main=paste("First obs"))
 first.t<-t.test(df$firstest~as.factor(df$numobs), conf.level=0.5) 
 #Last obs
 boxplot(df$lastest~as.factor(df$numobs), xlab="Number of days observed", ylab="Estimate of last obs (doy)", main="Last obs")
 last.t<-t.test(df$lastest~as.factor(df$numobs), conf.level=0.5)
 sum.df$lowobs<-lowobs
 sum.df$hiobs<-highobs
 if(uss==FALSE & wholeyear==FALSE){
   sum.df$probobs<-fallprob
 }
 if(uss==TRUE & wholeyear==FALSE){
   sum.df$probobs<-summerprob
 }
 sum.df$firstdif[j]<-first.t$estimate[2]-first.t$estimate[1]
 sum.df$firstdifp[j]<-first.t$p.value
 sum.df$lastdif[j]<-last.t$estimate[2]-last.t$estimate[1]
 sum.df$lastdifp[j]<-last.t$p.value
}

#save simulation estimates
if(uss==TRUE & wholeyear==FALSE){
  write.csv(sum.df,"orcasimeffort_usssum.csv")
  usssum.df<-sum.df
}
if(uss==FALSE & wholeyear==FALSE){
  write.csv(sum.df,"orcasimeffort_psfall.csv")
  pssum.df<-sum.df
  }
}

      #plot simulations and data together
      x<-c(1,2)
      y<-c(mean(pssum.df$firstdif),mean(pssum.df$lastdif))
      png(paste("analyses/orcaphen/figures/simeffort",styr,"-2017.png",sep=""),height=480, width=480)
      #first ps
      plot(x,y,pch=16,col="gray",ylim=c(-30,30), xlim=c(0,5), typ="p", bty="l", cex=1.5, xlab="", xaxt="n", ylab="Change in day of year", main = paste(styr,"-2017",sep=""))
      ylci<-c(quantile(pssum.df$firstdif,probs=0.25),quantile(pssum.df$lastdif, probs = 0.25))
      yuci<-c(quantile(pssum.df$firstdif, probs = 0.75),quantile(pssum.df$lastdif, probs = 0.75))
      
      arrows(x,yuci,x,ylci, length=0, col="gray", lwd=2)
      y<-c(mean(usssum.df$firstdif),mean(usssum.df$lastdif))
      points(x+2,y,pch=16,col="gray", cex=1.5)
      ylci<-c(quantile(usssum.df$firstdif,probs=0.25),quantile(usssum.df$lastdif, probs = 0.25))
      yuci<-c(quantile(usssum.df$firstdif, probs = 0.75),quantile(usssum.df$lastdif, probs = 0.75))
      
      arrows(x+2,yuci,x+2,ylci, length=0, col="gray", lwd=2)
      x.dat<-c(change.df$first.dif[1],change.df$last.dif[1]) 
      #ps
      points(x,as.numeric(c(change.df$first.dif[1],change.df$last.dif[1])),pch=16,col="black", cex=1.5) 
      arrows(x,as.numeric(c(change.df$first.uci[1],change.df$last.uci[1])),x,as.numeric(c(change.df$first.lci[1],change.df$last.lci[1])), length=0, col="black", lwd=1)
      #uss
      points(x+2,as.numeric(c(change.df$first.dif[2],change.df$last.dif[2])),pch=16,col="black", cex=1.5) 
      arrows(x+2,as.numeric(c(change.df$first.uci[2],change.df$last.uci[2])),x+2,as.numeric(c(change.df$first.lci[2],change.df$last.lci[2])), length=0, col="black", lwd=1)

      abline(h=0)
      
      axis(side=1, at=c(1,2,3,4), labels=c("first obs","last obs","first obs","last obs"))   
      
      axis(side=1, at=c(1.5,3.5), labels=c("Puget Sound","Upper Salish Sea"),line=1, lty=0)
      legend("topleft",legend=c("Expected change due to change in effort alone", "Observed change"),col=c("gray","black"),pch=16)
      dev.off()
       
      