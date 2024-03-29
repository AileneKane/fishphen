# Code for Phenology of SRKWs and their prey
# SRKW data from lime-kiln; Prey data = albion test fishery data
# Ailene Ettinger
# ailene.ettinger@tnc.org
# Updated March 2020
#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/GitHub/fishphen")

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

#load a function that pulls out phenology dates
source("analyses/orcaphen/source/phen.functions.R")

# 1. Choose the years, regions of interest, assumption about reports in the OrcaMaster and get the data
includeCanada=TRUE
firstyear=1976#probably set to 1975 or 1976 (Olson et al)
assumeSRKW=FALSE #If true, assume that "Orcas" means SRKW unless noted otherwuse (i.e. Transients or NRKWs)
use3regions=FALSE#If true, separate out the straight of Juan de Fuca as a 3rd region, distinct from CSS and PS (all code not yet working for 3 regions!)
plotPNG =FALSE#if false, plots pdfs

#Set start of seasons
ps.start<-182#July 1 = 182
uss.start<-91#April 1 = 91, 


d <- read.csv("data/AppendixII.csv")
quads<-read.csv("data/QuadCentroids.csv")
dim(d)#105339  21 on July 8, 2021

# 2. Clean the data (also saved in output/AppendixII_cleaned,csv)
source("analyses/orcaphen/source/clean_orca.R")
dim(d)#102512     22 on July 8, 2021


# 3. Limit space and time to firstyear or later and Salish Sea, Puget Sound, Washington Outer Coast 
source("analyses/orcaphen/source/orca_limitspacetime.R")
dim(d)# 102511     22
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
#To fix in above: check code and remove extraneous code

#use these for whale seasons to investigate shifts over time
#use apr 1 for uss season, jul 1 for ps season as start dates
#use oct 31 for uss season, jan31 for ps season, as end dates

#8. Prep the lime kiln only data for either gams or linear models
source("analyses/orcaphen/source/orca_get_whaledays_lime.R")

#9a. Fit gams in brms to limekiln data or load models that were already fit
#source("analyses/orcaphen/source/orca_rungams_lime.R")
#this takes a while, so just read in the relevant data from this below

#OR

#9b. Fit gams in brms to limekiln data or load models that were already fit
source("analyses/orcaphen/source/orca_loadgams_lime.R")


#10. Make est whale days plot (FigS5)
source("analyses/orcaphen/source/make_estwhaledayslime_plot.R")

#10. Read in model results from albion test fishery gams (in albion.brms.R)
#albchin<-read.csv("analyses/output/albionchiphen_allyear.csv", header = TRUE)
albchinest<-read.csv("analyses/output/albionchiphenestbrms.csv", header = TRUE)
#albchinest<-read.csv("analyses/output/albionchiphenest.csv", header = TRUE)

albchinest95<-albchinest[albchinest$year>1993  & albchinest$year<2018,]
albchinest95<-albchinest95[-which(albchinest95$year==2014),]

#function to make plots and fit linear models correlating phenology of SRKW to prey
#choose pod to use for SRKW data
pod = "SR"#choices are "SR" "J" "K" "L"
brkyr = 2006#try 2005, 2006, 2007, 2008


#read in lime.df!!!
source("analyses/orcaphen/source/orca_get_limedf.R")

limegests<-read.csv("analyses/output/lime_prob.occ.75.csv", header=TRUE)#also 0.90 and 0.95
chinab<-read.csv("analyses/output/albionbrmsests.csv", header = TRUE)

source("analyses/orcaphen/source/makeplots_srchinoverlap.R")

#add 90 days to limekiln doy columns to make comparable to chinook
lime2002<-lime.df[lime.df$year>2001,]
lime2002<-lime2002[lime2002$year!=2018,]
lime2002$firstest<-as.numeric(lime2002$firstest.all)+90
lime2002$mean<-as.numeric(lime2002$mean.all)+90
lime2002$lastest<-as.numeric(lime2002$lastest.all)+90
lime1995<-lime.df[lime.df$year>=1993,]
lime1995<-lime1995[lime1995$year!=2018,]
lime1995$firstest<-as.numeric(lime1995$firstest.all)+90
lime1995$mean<-as.numeric(lime1995$mean.all)+90
lime1995$lastest<-as.numeric(lime1995$lastest.all)+90


#quartz(height=4,width=12)
pdf(file="analyses/orcaphen/figures/lime_albchin.pdf",height=4,width=12)
myPalette <- colorRampPalette(brewer.pal(length(unique(albchinest95$year)), "Blues")) #### Gives us a heat map look
cols = rev(myPalette(length(unique(albchinest95$year))))
par(mfrow=c(1,3))
plot(albchinest95$firstobsdate,lime1995$firstest.all[lime1995$pod=="SRs"],type="p",pch=21, cex.axis=1.3,cex.lab=1.3,bg = cols[factor(albchinest95$year)],xlab="Chinook Arrival (doy)",ylab="SRKW Arrival (doy)", cex=1.4, bty="l")

#legend("topleft",legend=c("1994","2017"), pch=21, pt.bg=c(cols[1], cols[length(cols)]), bty="n", cex=1.4)
lime1995$firstest.all<-as.numeric(lime1995$firstest.all)
mod<-lm(lime1995$firstest.all[lime1995$pod=="SRs"]~albchinest95$firstobsdate)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3,  lwd=2)}

#whale days vs chinook run size
plot(albchinest95$alltotal,lime1995$nobs[lime1995$pod=="SRs"],type="p",pch=21, bg = cols[factor(albchinest95$year)],cex.axis=1.3,cex.lab=1.3,xlab="Chinook Abundance Index",ylab="Whale days", cex=1.2, bty="l")

mod<-lm(lime1995$nobs[lime1995$pod=="SRs"]~albchinest95$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3, lwd=2)}

#compare lime kiln phenology to whol region phenology from 2002-2017
#Lastobs of SRKW vs total run size###Could add this
plot(albchinest95$alltotal,lime1995$lastest.all[lime1995$pod=="SRs"],type="p",pch=21, bg = cols[factor(albchinest95$year)],cex.axis=1.3,cex.lab=1.3,xlab="Chinook Abundance Index",ylab="SRKW Departure (doy)", cex=1.2, bty="l")

mod<-lm(lime1995$lastest.all[lime1995$pod=="SRs"]~albchinest95$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1,lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3,lwd=2)}
dev.off()



j.occest<-read.csv("analyses/output/J_2uss_doy92-303_1978-2017occprobdoy.csv", header=TRUE)
j2002<-j.occest[j.occest$year>2001,]
j2002<-j2002[j2002$year!=2015,]
j1995<-j.occest[j.occest$year>1994,]
j1995<-j1995[j1995$year!=2015,]
lime2002<-lime.df[lime.df$year>2001,]
#Plot first ests. should i use pod specific estimates to match occupancy model?
quartz()
par(mfcol=c(3,2))
plot(lime2002$firstest.all,j2002$first.psi,type="p",pch=16, col = "darkblue", xlab="Lime Kiln first obs",ylab="J pod arrival est (occmod)", cex=1.2, bty="l", main= "2002-2016")
mod<-lm(j2002$first.psi~lime2002$firstest.all)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="darkblue")}
if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "darkblue")}

plot(lime2002$mean.all,j2002$peak.psi,type="p",pch=16, col = "darkblue", xlab="Lime Kiln mean obs doy",ylab="J pod peak est (occmod)", cex=1.2, bty="l")
mod<-lm(j2002$peak.psi~as.numeric(lime2002$mean.all))
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="darkblue")}
if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "darkblue")}

plot(lime2002$lastest.all,j2002$last.psi,type="p",pch=16, col = "darkblue", xlab="Lime Kiln last obs doy",ylab="J pod last est (occmod)", cex=1.2, bty="l")
mod<-lm(j2002$last.psi~as.numeric(lime2002$lastest.al))
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="darkblue")}
if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "darkblue")}

plot(lime1995$firstest.all,j1995$first.psi,type="p",pch=16, col = "darkblue", xlab="Lime Kiln first obs",ylab="J pod arrival est (occmod)", cex=1.2, bty="l", main= "1995-2016")
mod<-lm(j1995$first.psi~lime1995$firstest)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="darkblue")}
if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "darkblue")}

plot(lime1995$mean,j1995$peak.psi,type="p",pch=16, col = "darkblue", xlab="Lime Kiln mean obs doy",ylab="J pod peak est (occmod)", cex=1.2, bty="l")
mod<-lm(j1995$peak.psi~as.numeric(lime1995$mean))
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="darkblue")}
if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "darkblue")}

plot(lime1995$lastest,j1995$last.psi,type="p",pch=16, col = "darkblue", xlab="Lime Kiln last obs doy",ylab="J pod last est (occmod)", cex=1.2, bty="l")
mod<-lm(j1995$last.psi~as.numeric(lime1995$lastest))
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="darkblue")}
if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "darkblue")}


#Plot curves of salmon abundance vs day vs mean whale day

chinab<-read.csv("analyses/output/albionbrmsests.csv", header = TRUE)
chinab.old<-chinab[chinab$year>1993 & chinab$year<2006,] 
chinab.rec<-chinab[chinab$year>=2006 & chinab$year<2018,] 
cpue.old<-cbind(aggregate(chinab.old$cpue,by=list(chinab.old$doy),mean), aggregate(chinab.old$cpue,by=list(chinab.old$doy),sd)$x)
cpue.rec<-cbind(aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),mean),aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),sd)$x)
colnames(cpue.old)<-colnames(cpue.rec)<-c("doy","cpue.mean","cpue.sd")

#fit gams of srkw prob of presence
#source(orca_rungams_lime.R)#take a long time so just read in model ests

gests<-get.gests(limegests,"SRprob.Estimate")
jgests<-get.gests(limegests,"Jprob.Estimate")
kgests<-get.gests(limegests,"Kprob.Estimate")
lgests<-get.gests(limegests,"Lprob.Estimate")
jgests$meanprobs
meanmod<-summary(lm(gests$meanprobs~gests$year))#trend is getting lower
summary(lm(jgests$meanprobs~jgests$year))#not getting lower
summary(lm(kgests$meanprobs~kgests$year))#getting lower
summary(lm(lgests$meanprobs~lgests$year))#getting lower


peakmod<-summary(lm(gests$peakoc.doy~gests$year))#trend is getting later
confint(lm(gests$peakoc.doy~gests$year),level= .90)
summary(lm(jgests$peakoc.doy~jgests$year))# getting later
summary(lm(kgests$peakoc.doy~kgests$year))# getting later
summary(lm(lgests$peakoc.doy~lgests$year))#not getting later

confint(lm(gests$lastprob~gests$year),level= .90)
confint(lm(gests$firstprob~gests$year), level=.90)


