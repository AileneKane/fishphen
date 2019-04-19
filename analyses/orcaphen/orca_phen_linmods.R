
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

#5. summarize whale days per year by region
wdays<-as.data.frame(tapply(orcasum.days$AllSRpres,list(orcasum.days$year,orcasum.days$region),sum))
colMeans(wdays[1:20,], na.rm=TRUE)

wdays.J<-as.data.frame(tapply(orcasum.days$Jpres,list(orcasum.days$year,orcasum.days$region),sum))
wdays.K<-as.data.frame(tapply(orcasum.days$Kpres,list(orcasum.days$year,orcasum.days$region),sum))
wdays.L<-as.data.frame(tapply(orcasum.days$Lpres,list(orcasum.days$year,orcasum.days$region),sum))

#6. if you want to do some basic plots of the data. This includes proportion of days in a week in which whales were observed up by week and decade
#source("analyses/orcaphen/source/orca_plotdata.R")

#Take home from the above plots: Seasonality is hard to see in the figure with a separate line for each decade. I also plotted them with a single line showing the mean across all decades, and I standardized the proportions by substracting the means and dividing by the standard deviation. 
#I did this to try to make seasonal patterns more obvious. It only sort of made them more obvious. Here's what I take away from these figures:
#1) There is a very clear seasonal pattern in the SRKW use of the upper salish sea. This season ranges roughly from week 20 (mid May) through week 40 (early October).
#2) The seasonal pattern in the SRKW use is less obvious for Puget Sound, because the maximum proportion is lower over all. However, the season during which the proportion is consistently above the mean probability
#ranges from week 40 (early October) through week 2 (early January)

#use these for whale seasons to investigate shifts over time
#use may 1 for uss season, oct 1 for ps season as start dates
#use oct 31 for uss season, jan31 for ps season, as end dates
regions=unique(orcasum.days$region)
podcols<-c("AllSRpres")#just do for all SRs for now
pods<-c("SRs")

#Create dataframe with first, last obs for each start date in each year
pods.all<-c()
regions.all<-c()
years.all<-c()
nobs.all<-c()
#firstest.1may.all<-c()#for uss
#firstest.1oct.all<-c()#for ps
#lastest.31oct.all<-c()#for uss
#lastest.31jan.all<-c()#for ps
firstest.all<-c()
lastest.all<-c()

#p=1
#r=1
years<-seq(1978,2017, by=1)#restrict to these years for orcayears
#unique(orcasum.days$orcayear)#use may 1 as start of orca year, as this will encompass min start date window that i want to try

for(p in 1:length(podcols)){
  colnum<-which(colnames(orcasum.days)==podcols[p])
  for(r in 1:2){
    regdat<-orcasum.days[orcasum.days$region==regions[r],]
    
     if (regions[r]=="uss"){
       regdat<-regdat[as.numeric(regdat$daysaftapr30)<185,]#may 1 through oct 31 for summer season
     }
    if (regions[r]=="ps"){
      regdat<-regdat[as.numeric(regdat$daysaftapr30)>=154 &as.numeric(regdat$daysaftapr30)<277,]#include data oct 1 through jan 31
    }
    for(y in 1:length(years)){
      yrdat<-regdat[regdat$orcayear==years[y],]
      pods.all<-c(pods.all,pods[p])
      regions.all<-c(regions.all,regions[r])
      years.all<-c(years.all,years[y])
      nobs.all<-c(nobs.all,length(yrdat$day[yrdat[,colnum]==1]))
      
#      if (regions[r]=="uss"){
        #firstest.1may.all<-c(firstest.1may.all,min(yrdat$daysaftapr30[yrdat[,colnum]==1], na.rm=TRUE))#1may=1 day after apr30stdays
        #lastest.31oct.all<-c(lastest.31oct.all,max(yrdat$daysaftapr30[yrdat[,colnum]==1], na.rm=TRUE))#31oct= 184 days after apr30
        #firstest.1oct.all<-c(firstest.1oct.all,NA)
        #lastest.31jan.all<-c(lastest.31jan.all,NA)
 #       }
#      if (regions[r]=="ps"){
 #       firstest.1oct.all<-c(firstest.1oct.all,min(yrdat$daysaftapr30[yrdat[,colnum]==1], na.rm=TRUE))}
#        lastest.31jan.all<-c(lastest.31jan.all,max(yrdat$daysaftapr30[yrdat[,colnum]==1], na.rm=TRUE))
 #       firstest.1may.all<-c(firstest.1may.all,NA)
  #      lastest.31oct.all<-c(lastest.31oct.all,NA)
   #     }
    firstest.all<-c(firstest.all,min(yrdat$daysaftapr30[yrdat[,colnum]==1], na.rm=TRUE))
    lastest.all<-c(lastest.all,max(yrdat$daysaftapr30[yrdat[,colnum]==1], na.rm=TRUE))
    
    }
  }
}

df <- as.data.frame(cbind(pods.all,regions.all,years.all,nobs.all,firstest.all,lastest.all))
colnames(df)[1:4]<-c("pod","region","year","nobs")

#Plot trends using different start and end dates

pod.df=df[df$pod=="SRs",]
pod.df$firstest.all[which(pod.df$firstest.all=="Inf")]<-NA
pod.df$lastest.all[which(pod.df$lastest.all=="-Inf")]<-NA

#
pod.df$firstest.all<-as.numeric(pod.df$firstest.all)

pod.df$lastest.all<-as.numeric(pod.df$lastest.all)

pod.df$year<-as.numeric(pod.df$year)
quartz(height=6,width=7)
par(mfrow=c(2,2))
for(r in 1:2){
  reg.df<-pod.df[pod.df$region==regions[r],]


  #first obs 
  plot(reg.df$year,reg.df$firstest.all,xlab="year",ylab="first obs doy", main=paste(regions[r]), bty="l", pch=21, bg="gray")
  mod<-lm(reg.df$firstest.all~reg.df$year)
  abline(mod)
  mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
  #last obs
  plot(reg.df$year,reg.df$lastest.all,xlab="year",ylab="last obs doy", main=paste(regions[r]), bty="l", pch=21, bg="gray")
  mod<-lm(reg.df$lastest.all~reg.df$year)
  mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  abline(mod)
  print(summary(mod))
  mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)

}

#Linear models don't make sense given the data- look like nonlinear relationships
#Make boxplots similar to the way that i made them for test data
pod.df=df[df$pod=="SRs",]
pod.df$firstest[which(pod.df$firstest=="Inf")]<-NA
pod.df$lastest[which(pod.df$lastest=="-Inf")]<-NA
pod.df$firstest<-as.numeric(pod.df$firstest)
pod.df$lastest<-as.numeric(pod.df$lastest)

pod.df$decade<-"1977-1986"
pod.df$decade[pod.df$year>1986]<-"1987-1996"
pod.df$decade[pod.df$year>1996]<-"1997-2006"
pod.df$decade[pod.df$year>2006]<-"2007-2016"
pod.df$period<-"1978-1997"
pod.df$period[pod.df$year>1997]<-"1998-2017"

quartz()
par(mfrow=c(1,2))
#First obs
boxplot(as.numeric(pod.df$firstest[pod.df$region=="ps"])~as.factor(pod.df$period[pod.df$region=="ps"]), xlab="Period", ylab="Estimate of first obs (doy) in PS", main="First obs")
first.t.ps<-t.test(as.numeric(pod.df$firstest[pod.df$region=="ps"])~pod.df$period[pod.df$region=="ps"], paired = FALSE, var.equal = FALSE,conf.level=0.95)
#mtext(paste("Change=",-1*round(t$estimate[1]-t$estimate[2], digits=1),"(",-1*round(t$conf.int[1],digits=1),",",-1*round(t$conf.int[2],digits=1),")", sep=""),side=3,line=-3, adj=1)

#Last obs
boxplot(as.numeric(pod.df$lastest[pod.df$region=="ps"])~as.factor(pod.df$period[pod.df$region=="ps"]), xlab="Period", ylab="Estimate of last obs (doy) in PS", main="Last obs")
last.t.ps<-t.test(as.numeric(pod.df$lastest[pod.df$region=="ps"])~as.factor(pod.df$period[pod.df$region=="ps"]), conf.level=0.95)
#mtext(paste("Change=",-1*round(t$estimate[1]-t$estimate[2], digits=1),"(",-1*round(t$conf.int[1],digits=1),",",-1*round(t$conf.int[2],digits=1),")", sep=""),side=1,line=-3, adj=1)

#create a vector with the differences, like in the simulation code:
firstdif.ps<-first.t.ps$estimate[2]-first.t.ps$estimate[1]
firstdifp.ps<-first.t.ps$p.value
lastdif.ps<-last.t.ps$estimate[2]-last.t.ps$estimate[1]
lastdifp.ps<-last.t.ps$p.value

quartz()
par(mfrow=c(1,2))
#First obs
boxplot(as.numeric(pod.df$firstest[pod.df$region=="uss"])~as.factor(pod.df$period[pod.df$region=="uss"]), xlab="Period", ylab="Estimate of first obs (doy) in USS", main="First obs")
first.t.uss<-t.test(as.numeric(pod.df$firstest[pod.df$region=="uss"])~pod.df$period[pod.df$region=="uss"], paired = FALSE, var.equal = FALSE,conf.level=0.95)
#mtext(paste("Change=",-1*round(t$estimate[1]-t$estimate[2], digits=1),"(",-1*round(t$conf.int[1],digits=1),",",-1*round(t$conf.int[2],digits=1),")", sep=""),side=3,line=-3, adj=1)

#Last obs
boxplot(as.numeric(pod.df$lastest[pod.df$region=="uss"])~as.factor(pod.df$period[pod.df$region=="uss"]), xlab="Period", ylab="Estimate of last obs (doy) in USS", main="Last obs")
last.t.uss<-t.test(as.numeric(pod.df$lastest[pod.df$region=="uss"])~as.factor(pod.df$period[pod.df$region=="uss"]), conf.level=0.95)
#mtext(paste("Change=",-1*round(t$estimate[1]-t$estimate[2], digits=1),"(",-1*round(t$conf.int[1],digits=1),",",-1*round(t$conf.int[2],digits=1),")", sep=""),side=1,line=-3, adj=1)

#create a vector with the differences, like in the simulation code:
firstdif.uss<-first.t.uss$estimate[2]-first.t.uss$estimate[1]
firstdifp.uss<-first.t.uss$p.value
lastdif.uss<-last.t.uss$estimate[2]-last.t.uss$estimate[1]
lastdifp.uss<-last.t.uss$p.value

change.df<-as.data.frame(rbind(c("SRs","ps",mean(as.numeric(pod.df$nobs[pod.df$region=="ps"], na.rm=TRUE)),NA,firstdif.ps,firstdifp.ps,lastdif.ps,lastdifp.ps),
                  c("SRs","uss",mean(as.numeric(pod.df$nobs[pod.df$region=="uss"], na.rm=TRUE)),NA,firstdif.uss,firstdifp.uss,lastdif.uss,lastdifp.uss)))
colnames(change.df)<-c("pod","region","nobs","prob","first.dif","first.p","last.dif","last.p")

###To do:
#Make clockplots of data: #11R (http://www.r-graph-gallery.com/49-clock-plot/)

####################################################  
####################################################  
######################OLD CODE######################
####################################################
####################################################

# #it appears to matter a lot which window you look at (when you start looking...) so, how to choose?
# # Also, linear models don't make sense becuase the trends look nonlinear.
# 
# tapply(pod.df$firstest.all,list(pod.df$region),mean, na.rm=TRUE)
# tapply(pod.df$lastest.all,list(pod.df$region),mean, na.rm=TRUE)
# 
# 
# 
# #See if there is a shift in first, last, mean  by decade
# regions=unique(orcasum.days$region)
# podcols<-c("Jpres", "Kpres", "Lpres", "AllSRpres")
# pods<-c("J","K","L","SRs")
# years<-unique(orcasum.days$year)
# pods.all<-c()
# regions.all<-c()
# years.all<-c()
# nobs.all<-c()
# firstest.all<-c()
# lastest.all<-c()
# meanest.all<-c()
# firstest.sept30.all<-c()
# lastest.sept30.all<-c()
# meanest.sept30.all<-c()
# firstest.apr30.all<-c()
# lastest.apr30.all<-c()
# meanest.apr30.all<-c()
# for(p in 1:length(podcols)){
#   colnum<-which(colnames(orcasum.days)==podcols[p])
#   for(r in 1:length(regions)){
#     regdat<-orcasum.days[orcasum.days$region==regions[r],]
#     if (regions[r]=="uss"){
#       regdat<-regdat[as.numeric(regdat$day)<273,]#look at data before Sept 30 for USS
#       regdat<-regdat[as.numeric(regdat$day)>121,]#look at data after MAy 1 for USS
#     }
#     
#     for(y in 1:length(years)){
#       yrdat<-regdat[regdat$year==years[y],]
#       pods.all<-c(pods.all,pods[p])
#       regions.all<-c(regions.all,regions[r])
#       years.all<-c(years.all,years[y])
#       nobs.all<-c(nobs.all,length(yrdat$day[yrdat[,colnum]==1]))
#       firstest.all<-c(firstest.all,min(yrdat$day[yrdat[,colnum]==1], na.rm=TRUE))
#       lastest.all<-c(lastest.all,max(yrdat$day[yrdat[,colnum]==1], na.rm=TRUE))
#       meanest.all<-c(meanest.all,mean(as.numeric(yrdat$day[yrdat[,colnum]==1]), na.rm=TRUE))
#       orcayrdat<-regdat[regdat$orcayear==years[y],]
#       orcayrdat2<-regdat[regdat$orcayear2==years[y],]
#       firstest.sept30.all<-c(firstest.sept30.all,min(orcayrdat$daysaftsept30[orcayrdat[,colnum]==1], na.rm=TRUE))
#       lastest.sept30.all<-c(lastest.sept30.all,max(orcayrdat$daysaftsept30[orcayrdat[,colnum]==1], na.rm=TRUE))
#       meanest.sept30.all<-c(meanest.sept30.all,mean(as.numeric(orcayrdat$daysaftsept30[orcayrdat[,colnum]==1]), na.rm=TRUE))
#       firstest.apr30.all<-c(firstest.apr30.all,min(orcayrdat2$daysaftapr30[orcayrdat2[,colnum]==1], na.rm=TRUE))
#       lastest.apr30.all<-c(lastest.apr30.all,max(orcayrdat2$daysaftapr30[orcayrdat2[,colnum]==1], na.rm=TRUE))
#       meanest.apr30.all<-c(meanest.apr30.all,mean(as.numeric(orcayrdat2$daysaftapr30[orcayrdat2[,colnum]==1]), na.rm=TRUE))
#       
#     }
#   }
# }
# 
# # 
# # df <- as.data.frame(cbind(pods.all,regions.all,years.all,nobs.all,firstest.all,lastest.all,meanest.all,firstest.sept30.all,lastest.sept30.all,meanest.sept30.all,firstest.apr30.all,lastest.apr30.all,meanest.apr30.all))
# # colnames(df)<-c("pod","region","year","nobs","firstest","lastest","meanest","firstest.sept30","lastest.sept30","meanest.sept30","firstest.apr30","lastest.apr30","meanest.apr30") 
# #Now fit some linear models and plots
# 
# quartz()
# par(mfrow=c(3,2))
# pod.df=df[df$pod=="SRs",]
# pod.df$firstest[which(pod.df$firstest=="Inf")]<-NA
# pod.df$lastest[which(pod.df$lastest=="-Inf")]<-NA
# pod.df$firstest<-as.numeric(pod.df$firstest)
# pod.df$lastest<-as.numeric(pod.df$lastest)
# pod.df$meanest=as.numeric(pod.df$meanest)
# 
# pod.df$firstest.sept30[which(pod.df$firstest.sept30=="Inf")]<-NA
# pod.df$lastest.sept30[which(pod.df$lastest.sept30=="-Inf")]<-NA
# 
# pod.df$firstest.apr30[which(pod.df$firstest.apr30=="Inf")]<-NA
# pod.df$lastest.apr30[which(pod.df$lastest.apr30=="-Inf")]<-NA
# 
# for(i in 1:length(regions)){
#   reg.df=pod.df[pod.df$region==regions[i],]
#   reg.df$year=as.numeric(reg.df$year)
#   
#   #if(regions[i]=="ps" & unique(reg.df$pod)=="SRs"){reg.df<-reg.df[1:40,]}
#   #if(regions[i]=="uss"& unique(reg.df$pod)=="SRs"){reg.df[81:120,]}
#   #first obs
#   plot(reg.df$year,reg.df$firstest,xlab="year",ylab="first obs doy", main=paste(unique(reg.df$pod)), bty="l", pch=21, bg="gray")
#   mod<-lm(reg.df$firstest~reg.df$year)
#   abline(mod)
#   mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
#   mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
#   
#   #last obs
#   plot(reg.df$year,reg.df$lastest,xlab="year",ylab="last obs doy", main=paste(regions[i]), bty="l", pch=21, bg="gray")
#   mod<-lm(reg.df$lastest~reg.df$year)
#   mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
#   abline(mod)
#   mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
#   
  # #mean obs
  # plot(reg.df$year,reg.df$meanest,xlab="year",ylab="mean obs doy", main="", bty="l", pch=21, bg="gray")
  # mod<-lm(reg.df$meanest~reg.df$year)
  # mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  # abline(mod)
  # mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
  # 
  # #duration
  # plot(reg.df$year,reg.df$duration,xlab="year",ylab="duration (days)", main="", bty="l", pch=21, bg="gray")
  # mod<-lm(reg.df$duration~reg.df$year)
  # mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  # mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
  # 
  # abline(mod)
  # 
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