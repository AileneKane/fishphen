##This code includes just the code for stuff in the manuscript- no data exploration or anything.

#16 Januuary 2019

#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Set working directory: 
#setwd("~/GitHub/fishphen")
#or from laptop:
setwd("C:/Users/ailene.ettinger.TNC/OneDrive - The Nature Conservancy/Documents/GitHub/fishphen")

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
dim(d)#105344  18 on Nov 08, 2019

# 2. Clean the data (also saved in output/AppendixII_cleaned,csv)
source("analyses/orcaphen/source/clean_orca.R")
dim(d)#105339     21 on Nov 8, 2019


# 3. Limit space and time to firstyear or later and Salish Sea, Puget Sound, Washington Outer Coast 
source("analyses/orcaphen/source/orca_limitspacetime.R")
dim(d)#102512     22
#table(d$FishArea,d$region)#check regions are correct

#4. Get data in terms of number of observations per day and "whale days": days on which whales were seen (presence/absence for each day)
source("analyses/orcaphen/source/orca_get_whaledays.R")

#5. Summarize and plot whale days 
wdays<-as.data.frame(tapply(orcasum.days$AllSRpres,list(orcasum.days$year,orcasum.days$region),sum))
wdays.J<-as.data.frame(tapply(orcasum.days$Jpres,list(orcasum.days$year,orcasum.days$region),sum))
wdays.K<-as.data.frame(tapply(orcasum.days$Kpres,list(orcasum.days$year,orcasum.days$region),sum))
wdays.L<-as.data.frame(tapply(orcasum.days$Lpres,list(orcasum.days$year,orcasum.days$region),sum))

source("analyses/orcaphen/source/orca_plot_whaledays.R")

#6. Make a map of the SRKW sightings in ps and uss (note that this map is not the map used in the ms- Erica made the map)
source("analyses/orcaphen/source/orca_makemap.R")

#8a. Prep the lime kiln only sdata for either gams or linear models
source("analyses/orcaphen/source/orca_get_whaledays_lime.R")
#7.Fit gams to Lime Kiln SRKW data and 

#source("analyses/orcaphen/source/orca_rungams_lime.R")#take a long time, also- should replace with occupancy model fit in jags
limegests<-read.csv("analyses/output/limekiln.srkw.gamests.csv", header=TRUE)

#Plot curves of salmon abundance vs day vs mean whale day

chinab<-read.csv("analyses/output/albiongamests.csv", header = TRUE)
chinab.old<-chinab[chinab$year>1993 & chinab$year<2006,] 
chinab.rec<-chinab[chinab$year>=2006 & chinab$year<2018,] 
cpue.old<-cbind(aggregate(chinab.old$cpue,by=list(chinab.old$doy),mean), aggregate(chinab.old$cpue,by=list(chinab.old$doy),sd)$x)
cpue.rec<-cbind(aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),mean),aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),sd)$x)
colnames(cpue.old)<-colnames(cpue.rec)<-c("doy","cpue.mean","cpue.sd")
