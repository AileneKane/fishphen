# Orca analysis: occupancy models
# Started November 1327, 2018
# by Ailene Ettinger ailene.ettinger@noaa.gov
#housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/GitHub/fishphen")

# Load libraries
library(dplyr)

# 1. Choose the years, regions of interest, assumption about reports in the OrcaMaster and get the data
includeCanada=TRUE
firstyear=1978#
assumeSRKW=FALSE #If true, assume that "Orcas" means SRKW unless noted otherwuse (i.e. Transients or NRKWs)
use3regions=FALSE#If true, separate out the straight of Juan de Fuca as a 3rd region, distinct from CSS and PS
d <- read.csv("data/AppendixII.csv")
quads<-read.csv("data/QuadCentroids.csv")


# 2. Clean the data (also saved in output/AppendixII_cleaned,csv)
source("analyses/orcaphen/source/clean_orca.R")


# 3. Limit space and time to firstyear or later and Salish Sea, Puget Sound, Washington Outer Coast 
source("analyses/orcaphen/source/orca_limitspacetime.R")


#4. Get data in terms of number of observations per day and "whale days": days on which whales were seen (presence/absence for each day)
source("analyses/orcaphen/source/orca_get_whaledays.R")

#Only use fishing areas that have atleast 4 years with >20 observations:
#tab.fa.yr<-table(d$FishArea,d$Year)
#tab.fa.yr[tab.fa.yr < 20] <- 0
#tab.fa.yr[tab.fa.yr >= 20] <- 1
#sites.touse<-rownames(tab.fa.yr)[rowSums(tab.fa.yr)>4]
#d<-d[d$FishArea %in% c(sites.touse),]

#Prepare data for running model from strebel et al 2014
#"nrep" "ndet" "site" "day" "year"

# Add a column for total number of observations 
#of all SRKWs during each week of each year year to estimate detection
#d$wkyrfa<-paste(d$Year, d$week,d$FishArea,sep="_")#if we decide to add FishArea to detection estimates for model
#d$wkyr<-paste(d$Year, d$week,sep="_")
#unique(d$FishArea)
d<-d[-which(is.na(d$Year)),]
d$yrdayfa<-paste(d$Year, d$day,d$FishArea,sep="_")

#Raw detection ratios:
obs = aggregate(Obs ~yrdayfa, data = d,sum)

# presence of each pod
js = aggregate(J ~yrdayfa, data = d,sum)
ks = aggregate(K~yrdayfa, data = d,sum)
ls = aggregate(L~yrdayfa, data = d,sum)
srs = aggregate(SRKW~yrdayfa, data = d,sum)
det<-cbind(js,ks[,2],ls[,2],srs[,2],obs[2])
colnames(det)[2:6]<-c("Jobs","Kobs","Lobs","AllSRobs","nrep")
det$year<-substr(det$yrdayfa,1,4)
det$day<-substr(det$yrdayfa,6,8)
det$fa<-substr(det$yrdayfa,10,nchar(det$yrdayfa))
#det<-det[!det$year=="NA_N",]

#4.#Assign region to this new aggregated dataset, based on fishing area:
if(use3regions==FALSE){
  det$region<-"ps"
  det$region[det$fa=="04"|det$fa=="05"|det$fa=="06"|det$fa=="07"|det$fa== "16C"|det$fa=="17C"|det$fa== "18C"|det$fa=="19C"|det$fa=="20C"|det$fa=="29C"]<-"uss"
  det$region[det$fa=="01"|det$fa=="02"|det$fa=="03"]<-"oc"#outer coast
}

if(use3regions==TRUE){
  
  det$region<-"ps"
  det$region[det$fa=="06"|det$fa=="07"|det$fa== "16C"|det$fa=="17C"|det$fa== "18C"|det$fa=="19C"|det$fa=="29C"]<-"uss"
  det$region[det$fa=="01"|det$fa=="02"|det$fa=="03"]<-"oc"#outer coast
  det$region[det$fa=="04"|det$fa=="05"|det$fa=="20C"|det$fa=="21C"|det$fa=="22C"|det$fa=="121C"]<-"jf"#straight of juan de fuca
}

det$site<-as.numeric(as.factor(det$fa))
det$day<-as.numeric(det$day)
det$year<-as.numeric(det$year)
jdet<-subset(det,select=c(nrep,Jobs,site,day,year,region))

#use only sites that have atleast 1 observation in all years
jdet <- jdet[apply(jdet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
kdet<-subset(det,select=c(nrep,Kobs,site,day,year,region))
kdet <- kdet[apply(kdet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
ldet<-subset(det,select=c(nrep,Lobs,site,day,year,region))
ldet <- ldet[apply(ldet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
srdet<-subset(det,select=c(nrep,AllSRobs,site,day,year,region))
srdet <- srdet[apply(srdet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

colnames(srdet)[2]<-colnames(jdet)[2]<-colnames(kdet)[2]<-colnames(ldet)[2]<-"ndet"
colnames(srdet)[5]<-colnames(jdet)[5]<-colnames(kdet)[5]<-colnames(ldet)[5]<-"year"


if(assumeSRKW==FALSE){
  write.csv(srdet,"analyses/output/allsr_dat.csv",row.names = FALSE)
  write.csv(ldet,"analyses/output/l_dat.csv",row.names = FALSE)
  write.csv(kdet,"analyses/output/k_dat.csv",row.names = FALSE)
  write.csv(jdet,"analyses/output/j_dat.csv",row.names = FALSE)
}
if(assumeSRKW==TRUE){
  write.csv(srdet,"analyses/output/allsr_dat_assumeSRKW.csv",row.names = FALSE)
  write.csv(ldet,"analyses/output/l_dat_assumeSRKW.csv",row.names = FALSE)
  write.csv(kdet,"analyses/output/k_dat_assumeSRKW.csv",row.names = FALSE)
  write.csv(jdet,"analyses/output/j_dat_assumeSRKW.csv",row.names = FALSE)
}


#Check that fishing areas are correct and write out lat/longs for making map
#write out csv of all lat/longs for map
d$lat.long<-paste(d$Lat,d$Long,sep=".")
orcalatlon<-d%>% # start with the data frame
  distinct(lat.long, .keep_all = TRUE) %>% # establishing grouping variables
  filter(SRKW == 1) %>%#select only SRKWs
  dplyr::select(LikelyPod, region,FishArea,Lat,Long)

#remove weird longitudes
# orcalatlon<-orcalatlon[orcalatlon$Long!="0",]
# orcalatlon<-orcalatlon[orcalatlon$region!="oc",]
# write.csv(orcalatlon,"analyses/output/srkw_mapdatfile_new.csv", row.names = FALSE)
# library(rworldmap)
# library(scales)
# newmap<-getMap(resolution = "low")
# plot(newmap, xlim=range(as.numeric(orcalatlon$Long)), ylim = range(as.numeric(orcalatlon$Lat)))
# points(orcalatlon$Long[orcalatlon$region == "ps"],orcalatlon$Lat[orcalatlon$region == "ps"], type = "p", pch = 16, col = adjustcolor("goldenrod",alpha.f = 0.6))
# points(orcalatlon$Long[orcalatlon$region == "uss"],orcalatlon$Lat[orcalatlon$region == "uss"], type = "p", pch = 16, col = adjustcolor("darkblue",alpha.f = 0.6))
# 
# unique(orcalatlon$FishArea[orcalatlon$region=="uss"])
# #checked and fixws 06,18C,19C, 29C, 17C, 20C, 05, 07

