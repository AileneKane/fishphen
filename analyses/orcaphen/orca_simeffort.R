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
# 1. Get the data
d <- read.csv("data/AppendixII.csv")

# 2. Clean the data (also saved in output/AppendixII_cleaned,csv)
source("analyses/orcaphen/source/clean_orca.R")

# 3. Limit space and time to 1975 or later and Salish Sea, Puget Sound, Washington Outer Coast 
source("analyses/orcaphen/source/orca_limitspacetime.R")

#4. Get data in terms of number of observations per day and "whale days": days on which whales were seen (presence/absence for each day instead of )
source("analyses/orcaphen/source/orca_get_whaledays.R")

#5. Summarize changes in effort over time
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


uss=FALSE#TRUE for uss (upper salish sea), peak prob season is 1 May through 30 sept, and prob obs prob.all[3] (~.76)
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
nreps<-10#

if(uss==TRUE & wholeyear==FALSE){
  lowobs<-89#mean whales days during the summer first 10 years: uss=89.1
  highobs<-123#mean whales days during the summer last 10 years: uss=122.8
}

if(uss==FALSE & wholeyear==FALSE){
  lowobs<-16#mean whales days during the fall first 10 years: ps=16.2
  highobs<-40#mean whales days last 10 years: ps=40.18
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
 print(df)
 
 quartz()
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
}
if(uss==FALSE & wholeyear==FALSE){
  write.csv(sum.df,"orcasimeffort_psfall.csv")
}
