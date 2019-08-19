
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
use3regions=FALSE#If true, separate out the straight of Juan de Fuca as a 3rd region, distinct from CSS and PS
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

#9. Fit some basic linear models to subsets of data (lime kiln and west seattle only)
regions=unique(orcasum.days$region)
podcols<-c("Jpres","Kpres","Lpres","AllSRpres")
pods<-c("J", "K","L","SRs")
source("analyses/orcaphen/source/orca_runlinmods_subsets.R")
