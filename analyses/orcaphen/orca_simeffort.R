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
setwd("~/Documents/GitHub/fishphen")
#or from laptop:
#setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

# Load libraries
library(dplyr)
library(mgcv)
library(scales)
library(RColorBrewer)
# 1. Get the observation data together and plot it, do linear models and t-tests with the data
source("analyses/orcaphen/orca_phen_linmods.R")

#2. Summarize changes in effort over time
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
orcasum.days$season[orcasum.days$daysaftapr30>=154]<-"fall"#oct 1 through jan 31)
orcasum.days$season[orcasum.days$daysaftapr30>=277]<-"spring"#(feb 1 through april 30)
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
  springprob<-0.5
  summerprob<-0.85
  fallprob<-0.5}
if(uss==FALSE){
  springprob<-0.3
  summerprob<-0.5
  fallprob<-0.6}
  
springdays<-seq(277,366, by=1)
summerdays<-seq(1,153, by=1)
falldays<-seq(154,276, by=1)

#Look at effect of increasing effort on estimate of first observation and estimate of last observation
nreps<-20#e.g. number of years

if(uss==TRUE & wholeyear==FALSE){
  lowobs<-104#mean whales days/year during the summer first 20 years: uss=104.4
  highobs<-133#mean whales days during the summer last 10 years: uss=132.9
}

if(uss==FALSE & wholeyear==FALSE){
  lowobs<-15#mean whales days during the fall first 10 years: ps=15.3
  highobs<-39#mean whales days last 10 years: ps=39.25
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
 
 #quartz()
 par(mfrow=c(1,2))
 #First obs)
 boxplot(df$firstest~as.factor(df$numobs), xlab="Number of days observed", ylab="Estimate of first obs (doy)", main=paste("First obs"))
 first.t<-t.test(df$firstest~as.factor(df$numobs), conf.level=0.95) 
 #Last obs
 boxplot(df$lastest~as.factor(df$numobs), xlab="Number of days observed", ylab="Estimate of last obs (doy)", main="Last obs")
 last.t<-t.test(df$lastest~as.factor(df$numobs), conf.level=0.95)
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


      
      x<-c(1,2,4,5)
      quartz()
      plot(x,as.numeric(c(change.df$first.dif[1],change.df$last.dif[1],change.df$first.dif[2],change.df$last.dif[2])),pch=16,col="black",ylim=c(-20,20), xlim=c(0,6), typ="p", bty="l", cex=1.5, xlab="", xaxt="n", ylab="Change in day of year")
      arrows(x,c(quantile(pssum.df$firstdif,.9),quantile(pssum.df$lastdif,.9),quantile(usssum.df$firstdif,.9),quantile(usssum.df$lastdif,.9)),
              x,c(quantile(pssum.df$firstdif,.1),quantile(pssum.df$lastdif,.1),quantile(usssum.df$firstdif,.1),quantile(usssum.df$lastdif,.1)),length=0, col="gray", lwd=10)
           

      
      points(x,as.numeric(c(change.df$first.dif[1],change.df$last.dif[1],change.df$first.dif[2],change.df$last.dif[2])),pch=16,col="black", cex=1.5)
      
      abline(h=0)
      
      axis(side=1, at=c(1,2,4,5), labels=c("first obs","last obs","first obs","last obs"))   
      
      axis(side=1, at=c(1.5,4.5), labels=c("Puget Sound","Upper Salish Sea"),line=1, lty=0)

      
      
      ##Alterntsive version
      
      #plot simulations and data together
      x<-c(1,2)
      y<-c(mean(pssum.df$firstdif),mean(pssum.df$lastdif))
      quartz()
      plot(x,y,pch=16,col="gray",ylim=c(-20,20), xlim=c(0,5), typ="p", bty="l", cex=1.5, xlab="", xaxt="n", ylab="Change in day of year")
      ysd<-c(sd(pssum.df$firstdif),mean(pssum.df$lastdif))
      
      arrows(x,y+ysd,x,y-ysd, length=0, col="gray", lwd=2)
      y<-c(mean(usssum.df$firstdif),mean(usssum.df$lastdif))
      points(x+2,y,pch=16,col="gray", cex=1.5)
      ysd<-c(sd(usssum.df$firstdif),mean(usssum.df$lastdif))
      arrows(x+2,y+ysd,x+2,y-ysd, length=0, col="gray", lwd=2)
      x.dat<-c(change.df$first.dif[1],change.df$last.dif[1]) 
      points(x,as.numeric(c(change.df$first.dif[1],change.df$last.dif[1])),pch=16,col="black", cex=1.5) 
      points(x+2,as.numeric(c(change.df$first.dif[2],change.df$last.dif[2])),pch=16,col="black", cex=1.5) 
      abline(h=0)
      
      axis(side=1, at=c(1,2,3,4), labels=c("first obs","last obs","first obs","last obs"))   
      
      axis(side=1, at=c(1.5,3.5), labels=c("Puget Sound","Upper Salish Sea"),line=1, lty=0)
      legend("topleft",legend=c("Expected change, given change in effort", "Observed change"),col=c("gray","black"),pch=16)
      
      