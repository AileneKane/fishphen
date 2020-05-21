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
#To fix in above: remove all quartz()


#7. Make a map of the SRKW sightings in ps and uss
source("analyses/orcaphen/source/orca_makemap.R")
#To fix in above: check code and remove extraneous code

#use these for whale seasons to investigate shifts over time
#use apr 1 for uss season, jul 1 for ps season as start dates
#use oct 31 for uss season, jan31 for ps season, as end dates

#8. Prep the lime kiln only data for either gams or linear models
source("analyses/orcaphen/source/orca_get_whaledays_lime.R")

#9. Fit gams in brms to limekiln data 
#source("analyses/orcaphen/source/orca_rungams_lime.R")

#this takes a while, so just read in the relevant data from this below
  
#10. Read in model results from albion test fishery gams (in albion.brms.R)
#albchin<-read.csv("analyses/output/albionchiphen_allyear.csv", header = TRUE)
#albchinest<-read.csv("analyses/output/albionchiphenbrmslog.csv", header = TRUE)
 albchinest<-read.csv("analyses/output/albionchiphenest.csv", header = TRUE)

#albchinest95<-albchinest[albchinest$year>1993  & albchinest$year<2018,]
albchinest95<-albchinest95[-which(albchinest95$year==2014),]


#function to make plots and fit linear models correlating phenology of SRKW to prey
#choose pod to use for SRKW data
pod = "SR"#choices are "SR" "J" "K" "L"
brkyr = 2006#try 2005, 2006, 2007, 2008


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
myPalette <- colorRampPalette(brewer.pal(length(unique(albchin95$year)), "Blues")) #### Gives us a heat map look
cols = rev(myPalette(length(unique(albchin95$year))))
par(mfrow=c(1,3))
plot(albchinest95$firstobsdate,lime1995$firstest.all,type="p",pch=21, cex.axis=1.3,cex.lab=1.3,bg = cols[factor(albchin95$year)],xlab="Chinook Arrival (doy)",ylab="SRKW Arrival (doy)", cex=1.4, bty="l")
#what is the really late salmon year?
#albchin95$year[which(albchin95$firstobsdate==max(albchin95$firstobsdate, na.rm=TRUE))]#2007
#emptyPlot(1,1, main="Test plot")
#gradientLegend(valRange=range(albchinest95$year),color=cols,pos=.7, side=2, n.seg=2, coords=TRUE, fit.margin=FALSE, length=.25, depth = .05)

#legend("topleft",legend=c("1994","2017"), pch=21, pt.bg=c(cols[1], cols[length(cols)]), bty="n", cex=1.4)
lime1995$firstest.all<-as.numeric(lime1995$firstest.all)
mod<-lm(lime1995$firstest.all~albchin95$firstobsdate)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3,  lwd=2)}
points(albchin95$firstobsdate,lime.df$firstest.p,pch=16, col = "blue",cex=1.2)
#mod<-lm(lime.df$firstest.p~albchin95$firstobsdate)
#if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
#if(summary(mod)$coef[2,4]<.10){abline(mod, lty=3, col = "blue")}

#First obs of SRKW vs total run size
# plot(albchin95$alltotal,lime.df$firstest.all,type="p",pch=16, col = "black",xlab="Chinook Run Size",ylim = c(40,70),ylab="SRKW Arrival DOY", cex=1.2, bty="l")
# mod<-lm(lime.df$firstest.all~albchin95$alltotal)
# if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1)}
# if(summary(mod)$coef[2,4]<.1){abline(mod, lty=3)}
# points(albchin95$alltotal,lime.df$firstest.p,pch=16, col = "blue",cex=1.2)
# mod<-lm(lime.df$firstest.p~albchin95$alltotal)
# if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
# if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "blue")}


# #First obs of SRKW vs peak run date
# plot(albchin95$peakobsdate,lime.df$firstest.all,type="p",pch=16, col = "black",xlab="Chinook Peak DOY",ylim = c(40,70),ylab="SRKW Arrival DOY", cex=1.2, bty="l")
# mod<-lm(lime.df$firstest.all~albchin95$peakobsdate)
# if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1)}
# if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3)}
# points(albchin95$peakobsdate,lime.df$firstest.p,pch=16, col = "blue",cex=1.2)
# mod<-lm(lime.df$firstest.p~albchin95$peakobsdate)
# if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
# if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "blue")}

# 
# #Last obs of SRKW vs last obs of salmon
# plot(albchin95$lastobsdate,lime.df$lastest.all,type="p",pch=16, col = "black",xlab="Chinook Last Obs DOY",ylab="SRKW Departure DOY", cex=1.2, bty="l")
# mod<-lm(lime.df$lastest.all~albchin95$lastobsdate)
# if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1)}
# if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3)}
# points(albchin95$lastobsdate,lime.df$lastest.p,pch=16, col = "blue",cex=1.2)
# mod<-lm(lime.df$lastest.p~albchin95$lastobsdate)
# if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
# if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "blue")}


#whale days vs chinook run size
plot(albchin95$alltotal,lime1995$nobs,type="p",pch=21, bg = cols[factor(albchin95$year)],cex.axis=1.3,cex.lab=1.3,xlab="Chinook Abundance Index",ylab="Whale days", cex=1.2, bty="l")
#points(albchin95$alltotal,lime.df$nobs,pch=21, bg = cols[factor(albchin95$year)],cex=1.5)

mod<-lm(lime1995$nobs~albchin95$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3, lwd=2)}

#compare lime kiln phenology to whol region phenology from 2002-2017
#Lastobs of SRKW vs total run size###Could add this
plot(albchin95$alltotal,lime1995$lastest.all,type="p",pch=21, bg = cols[factor(albchin95$year)],cex.axis=1.3,cex.lab=1.3,xlab="Chinook Abundance Index",ylab="SRKW Departure (doy)", cex=1.2, bty="l")
#points(albchin95$alltotal,lime.df$lastest.all,pch=21, bg = cols[factor(albchin95$year)],cex=1.5)

mod<-lm(lime1995$lastest.all~albchin95$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1,lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3,lwd=2)}
# points(albchin95$alltotal,lime.df$lastest.p,pch=16, col = "blue",cex=1.2)
# mod<-lm(lime.df$lastest.p~albchin95$alltotal)
# if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
# if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "blue")}
dev.off()


#Make same figure with years listed
#quartz(height=4,width=12)
pdf(file="analyses/orcaphen/figures/lime_albchin.pdf",height=4,width=12)
myPalette <- colorRampPalette(brewer.pal(length(unique(albchin95$year)), "Blues")) #### Gives us a heat map look
cols = rev(myPalette(length(unique(albchin95$year))))
lime.df
par(mfrow=c(1,3))
plot(albchinest95$firstobsdate,lime1995$firstest.all,type="p",pch=21, cex.axis=1.3,cex.lab=1.3,bg = cols[factor(albchin95$year)],xlab="Chinook Arrival (doy)",ylab="SRKW Arrival (doy)", cex=1.2, bty="l")
#what is the really late salmon year?
#albchin95$year[which(albchin95$firstobsdate==max(albchin95$firstobsdate, na.rm=TRUE))]#2007
#points(albchin95$firstobsdate,lime1995$firstest.all,pch=21, bg = cols[factor(albchin95$year)],cex=1.5)
legend("topleft",legend=c("1994","2017"), pch=21, pt.bg=c(cols[1], cols[length(cols)]), bty="n", cex=1.4)
mod<-lm(lime.df$firstest.all~albchin95$firstobsdate)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3,  lwd=2)}
#points(albchin95$firstobsdate,lime.df$firstest.p,pch=16, col = "blue",cex=1.2)
#mod<-lm(lime.df$firstest.p~albchin95$firstobsdate)
#if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
#if(summary(mod)$coef[2,4]<.10){abline(mod, lty=3, col = "blue")}

#First obs of SRKW vs total run size
# plot(albchin95$alltotal,lime.df$firstest.all,type="p",pch=16, col = "black",xlab="Chinook Run Size",ylim = c(40,70),ylab="SRKW Arrival DOY", cex=1.2, bty="l")
# mod<-lm(lime.df$firstest.all~albchin95$alltotal)
# if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1)}
# if(summary(mod)$coef[2,4]<.1){abline(mod, lty=3)}
# points(albchin95$alltotal,lime.df$firstest.p,pch=16, col = "blue",cex=1.2)
# mod<-lm(lime.df$firstest.p~albchin95$alltotal)
# if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
# if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "blue")}


# #First obs of SRKW vs peak run date
# plot(albchin95$peakobsdate,lime.df$firstest.all,type="p",pch=16, col = "black",xlab="Chinook Peak DOY",ylim = c(40,70),ylab="SRKW Arrival DOY", cex=1.2, bty="l")
# mod<-lm(lime.df$firstest.all~albchin95$peakobsdate)
# if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1)}
# if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3)}
# points(albchin95$peakobsdate,lime.df$firstest.p,pch=16, col = "blue",cex=1.2)
# mod<-lm(lime.df$firstest.p~albchin95$peakobsdate)
# if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
# if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "blue")}

# 
# #Last obs of SRKW vs last obs of salmon
# plot(albchin95$lastobsdate,lime.df$lastest.all,type="p",pch=16, col = "black",xlab="Chinook Last Obs DOY",ylab="SRKW Departure DOY", cex=1.2, bty="l")
# mod<-lm(lime.df$lastest.all~albchin95$lastobsdate)
# if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1)}
# if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3)}
# points(albchin95$lastobsdate,lime.df$lastest.p,pch=16, col = "blue",cex=1.2)
# mod<-lm(lime.df$lastest.p~albchin95$lastobsdate)
# if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
# if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "blue")}


#whale days vs chinook run size
plot(albchin95$alltotal,lime.df$nobs,type="p",pch=21, bg = cols[factor(albchin95$year)],cex.axis=1.3,cex.lab=1.3,xlab="Chinook Abundance Index",ylab="Whale days", cex=1.2, bty="l")
#points(albchin95$alltotal,lime.df$nobs,pch=21, bg = cols[factor(albchin95$year)],cex=1.5)

mod<-lm(lime.df$nobs~albchin95$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3, lwd=2)}

#compare lime kiln phenology to whol region phenology from 2002-2017
#Lastobs of SRKW vs total run size###Could add this
plot(albchin95$alltotal,lime.df$lastest.all,type="p",pch=21, bg = cols[factor(albchin95$year)],cex.axis=1.3,cex.lab=1.3,xlab="Chinook Abundance Index",ylab="SRKW Departure (doy)", cex=1.2, bty="l")
#points(albchin95$alltotal,lime.df$lastest.all,pch=21, bg = cols[factor(albchin95$year)],cex=1.5)

mod<-lm(lime.df$lastest.all~albchin95$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1,lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3,lwd=2)}
# points(albchin95$alltotal,lime.df$lastest.p,pch=16, col = "blue",cex=1.2)
# mod<-lm(lime.df$lastest.p~albchin95$alltotal)
# if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
# if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "blue")}
dev.off()


#quartz(height=4,width=12)
pdf(file="analyses/orcaphen/figures/lime_albchin_bin.pdf",height=4,width=12)

par(mfrow=c(1,3))
boxplot(lime.df$firstest.all~as.factor(albchin95$firstobsbin),cex.axis=1.3,cex.lab=1.3,xlab="Chinook Arrival",names= c("Early", "Average","Late"),ylab="SRKW Arrival (doy)", cex=1.2, bty="l", col="darkblue")
#what is the really late salmon year?
#albchin95$year[which(albchin95$firstobsdate==max(albchin95$firstobsdate, na.rm=TRUE))]#2007
points(albchin95$firstobsdate,lime.df$firstest.all,pch=21, bg = cols[factor(albchin95$year)],cex=1.5)
mod<-lm(lime.df$firstest.all~albchin95$firstobsdate)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3,  lwd=2)}

#whale days vs chinook run size
plot(albchin95$alltotal,lime.df$nobs,type="p",pch=21, bg = cols[factor(albchin95$year)],cex.axis=1.3,cex.lab=1.3,xlab="Chinook Abundance Index",ylab="Whale days", cex=1.2, bty="l")
points(albchin95$alltotal,lime.df$nobs,pch=21, bg = cols[factor(albchin95$year)],cex=1.5)
legend("topleft",legend=c("1994","2017"), pch=21, pt.bg=c(cols[1], cols[length(cols)]), bty="n", cex=1.4)

mod<-lm(lime.df$nobs~albchin95$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3, lwd=2)}

#compare lime kiln phenology to whol region phenology from 2002-2017
#Lastobs of SRKW vs total run size###Could add this
plot(albchin95$alltotal,lime.df$lastest.all,type="p",pch=21, bg = cols[factor(albchin95$year)],cex.axis=1.3,cex.lab=1.3,xlab="Chinook Abundance Index",ylab="SRKW Departure (doy)", cex=1.2, bty="l")
points(albchin95$alltotal,lime.df$lastest.all,pch=21, bg = cols[factor(albchin95$year)],cex=1.5)

mod<-lm(lime.df$lastest.all~albchin95$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1,lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3,lwd=2)}
# points(albchin95$alltotal,lime.df$lastest.p,pch=16, col = "blue",cex=1.2)
# mod<-lm(lime.df$lastest.p~albchin95$alltotal)
# if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
# if(summary(mod)$coef[2,4]<.15){abline(mod, lty=3, col = "blue")}
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

chinab<-read.csv("analyses/output/albiongamests.csv", header = TRUE)
chinab.old<-chinab[chinab$year>1993 & chinab$year<2006,] 
chinab.rec<-chinab[chinab$year>=2006 & chinab$year<2018,] 
cpue.old<-cbind(aggregate(chinab.old$cpue,by=list(chinab.old$doy),mean), aggregate(chinab.old$cpue,by=list(chinab.old$doy),sd)$x)
cpue.rec<-cbind(aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),mean),aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),sd)$x)
colnames(cpue.old)<-colnames(cpue.rec)<-c("doy","cpue.mean","cpue.sd")

#fit gams of srkw prob of presence
#source(orca_rungams_lime.R)#take a long time so just read in model ests
limegests<-read.csv("analyses/output/lime_prob.occ.50.csv", header=TRUE)#also 0.90 and 0.95

get.gests<-function(limegests,occprobs){
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
  occprobcol<-yeardat[,which(colnames(yeardat)==occprobs)]
  yrpeakoc<-max(occprobcol)
  yrpeakoc.doy<-yeardat$doy[which(occprobcol==yrpeakoc)]
  yrfirst.doy<-yeardat$doy[min(which(occprobcol>0.1))]
  yrlast.doy<-yeardat$doy[max(which(occprobcol>0.1))]
  meanprob<-mean(occprobcol)
  lprob<-quantile(occprobcol,0.25)
  uprob<-quantile(occprobcol,0.75)
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
gests<-get.gests(limegests,"prob.occ")
jgests<-get.gests(limegests,"jprob.occ")
kgests<-get.gests(limegests,"kprob.occ")
lgests<-get.gests(limegests,"lprob.occ")

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

orcasum.days.lime1<-limegests[limegests$year>1993 & limegests$year<2006,]
orcasum.days.lime2<-limegests[limegests$year>=2006 & limegests$year<2018,]

wdays.old<-cbind(aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),mean),aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),sd)$x,aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),length)$x)
wdays.rec<-cbind(aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),mean),aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),sd)$x,aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),length)$x)
pdf("analyses/orcaphen/figures/orcachinphenoverlap.pdf",height=6, width=12)
#png("analyses/orcaphen/figures/orcachinphenoverlap.png",height=6, width=12)

# to figure out how much to shift the salmon curve earlier- because they are measured at ft. langely on the frasier river, but we are interested in when they are at lime kiln
# lime  kiln is ~160 km from lime kiln "as the fish swims" and fish swim about 70 km per day! only need to shift by 2-3 days?
par(oma=c(1,1,1,3), mar=c(4,4,4,6))
shift<--14
plot(cpue.old$doy[7:220]+shift,running_mean(cpue.old$cpue.mean,7),xlim=c(120,250),ylim=c(0,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)),type="l", bty="u",col="salmon", lwd=2, lty=2,ylab="Chinook Abundance (mean cpue)", xlab="Day of Year")
polygon(c(rev(cpue.old$doy[7:220]+shift),cpue.old$doy[7:220]+shift),c(rev(running_mean(cpue.old$cpue.mean,7)+cpue.old$cpue.sd[7:220]),running_mean(cpue.old$cpue.mean,7)-cpue.old$cpue.sd[7:220]),col=alpha("salmon",0.1),lty=0)
polygon(c(rev(cpue.rec$doy[7:205]+shift),cpue.rec$doy[7:205]+shift),c(rev(running_mean(cpue.rec$cpue.mean,7)+cpue.rec$cpue.sd[7:205]),running_mean(cpue.rec$cpue.mean,7)-cpue.rec$cpue.sd[7:205]),col=alpha("salmon",0.1),lty=0)

lines(cpue.rec$doy[7:205]+shift,running_mean(cpue.rec$cpue.mean,7), lwd=2,col="salmon")
par(new = TRUE)

plot(wdays.old$doy[7:129],running_mean(wdays.old$meanocc,7), type="l",lty=2, lwd=2,col="black", xlim=c(120,250), ylim=c(0,1), yaxt="n", ylab="",xaxt="n", xlab="", bty="l")
polygon(c(rev(wdays.old$doy[7:129]),wdays.old$doy[7:129]),c(rev(running_mean(wdays.old$meanocc,7)+wdays.old$sdocc[7:129]),running_mean(wdays.old$meanocc,7)-wdays.old$sdocc[7:129]),col=alpha("black",0.05),lty=0)
polygon(c(rev(wdays.rec$doy[7:129]),wdays.rec$doy[7:129]),c(rev(running_mean(wdays.rec$meanocc,7)+wdays.rec$sdocc[7:129]),running_mean(wdays.rec$meanocc,7)-wdays.rec$sdocc[7:129]),col=alpha("black",0.05),lty=0)

lines(wdays.rec$doy[7:129],running_mean(wdays.rec$mean,7), lwd=2,col="black")
axis(side = 4)

mtext("SRKW presence",side=4, adj=.5, line=2)
legend(115,1,legend=c("1994-2005","2006-2017"),lty=c(2,1),lwd=2,col="black", bty="n")
legend(115,.85,legend=c("SRKW","salmon"),lty=1,lwd=2,col=c("black","salmon"), bty="n")

dev.off()

colnames(wdays.old)<-colnames(wdays.rec)<-c("doy","meanocc","sdocc","n")
wdays.old$seocc<-wdays.old$sdocc/sqrt(wdays.old$n)
wdays.rec$seocc<-wdays.rec$sdocc/sqrt(wdays.rec$n)

#quartz(height=6, width=12)
gests95<-gests[gests$years>1993,]
gests95<-gests95[gests95$years!=2014,]

