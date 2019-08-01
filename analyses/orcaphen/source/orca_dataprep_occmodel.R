# Orca analysis: occupancy models
# Started November 1327, 2018
# by Ailene Ettinger ailene.ettinger@noo.gov
#NEed to fix this
#housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/GitHub/fishphen")

# Load libraries
library(dplyr)
#1. Choose the years and regions of interest and get the data
includeCanada=TRUE
firstyear=1975#probably set to 1975 or 1976 (Olson et al)
d <- read.csv("data/AppendixII.csv")

# 2. Clean the data (also saved in output/AppendixII_cleaned,csv)
source("analyses/orcaphen/source/clean_orca.R")

# 1. Choose the years and regions of interest and get the data
includeCanada=TRUE
firstyear=1975#set to 1975 or 1976 (Olson et al)
d <- read.csv("data/AppendixII.csv")

# 2. Clean the data (also saved in output/AppendixII_cleaned,csv)
source("analyses/orcaphen/source/clean_orca.R")

# 3. Limit space and time to firstyear or later and Salish Sea, Puget Sound, Washington Outer Coast 
source("analyses/orcaphen/source/orca_limitspacetime.R")

#Only use fishing areas that have atleast 4 years with >20 observations:
#11 fishing areas with >5
# 8 fishing areas with >10
# 8 fishing areas with >20
tab.fa.yr<-table(d$FishArea,d$Year)
tab.fa.yr[tab.fa.yr < 20] <- 0
tab.fa.yr[tab.fa.yr >= 20] <- 1
sites.touse<-rownames(tab.fa.yr)[rowSums(tab.fa.yr)>4]
d<-d[d$FishArea %in% c(sites.touse),]

#Add week and day of year (day)
d$day<-strftime(strptime(paste(d$Month, d$Day, d$Year, sep="."),format= "%m.%d.%Y"),format= "%j")
d$week<-strftime(strptime(paste(d$Month, d$Day, d$Year, sep="."),format= "%m.%d.%Y"), format = "%V")#new weeks start on mondays

#Add a column for presences (1/0) for each pod, for Ts, and for SRKWs

d$J<-0
d$J[grep("J",d$Pod.cl)]<- 1
d$K<-0
d$K[grep("K",d$Pod.cl)]<- 1
d$L<-0
d$L[grep("L",d$Pod.cl)]<- 1
d$SRKW<-0
d$SRKW[grep("SR",d$Pod.cl)]<- 1
d$SRKW[d$J==1|d$K==1|d$L==1]<- 1   
d$Orcas<-1

#Prepare data for running model from strebel et al 2014
#"nrep" "ndet" "site" "day" "year"

# Add a column for total number of observations 
#of all SRKWs during each week of each year year to estimate detection
#d$wkyrfa<-paste(d$Year, d$week,d$FishArea,sep="_")#if we decide to add FishArea to detection estimates for model
#d$wkyr<-paste(d$Year, d$week,sep="_")
#unique(d$FishArea)
d$yrdayfa<-paste(d$Year, d$day,d$FishArea,sep="_")

#Raw detection ratios:
obs = aggregate(Orcas ~yrdayfa, data = d,sum)

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
#assign to ps (puget sound) or uss (upper salish sea) using fishing area
det$region<-"ps"
det$region[det$fa=="07"|det$fa=="06"|det$fa=="05"|det$fa=="04"|det$fa=="19C"|det$fa== "18C"|det$fa=="20C"|det$fa=="29C"]<-"uss"
det$region[det$fa=="01"|det$fa=="02"|det$fa=="03"]<-"oc"#outer coast

det$site<-as.numeric(as.factor(det$fa))
det$day<-as.numeric(det$day)
det$year<-as.numeric(det$year)

#Add a column for "season" and divide up by season.Not sure if these are best...
#start with winter vs summer
det$season<-NA
#det$season[det$day>274|det$day<60]<-1#winter (Oct 1-March)
det$season[det$day>274]<-1#winter (Oct 1-Dec 31)

det$season[det$day>121 & det$day<274]<-2#summer (May-Sept 31)
#add an "orca year" which runs Oct 1-Sept 31
#det$orcayear<-det$year
#det$orcayear[which(det$day>273)]<-det$year[which(det$day>273)]+1
#det$daysaftsept30<-NA
#det$daysaftsept30[which(det$day>273 & det$day<367)]<-det$day[which(det$day>273 & det$day<367)]-273
#det$daysaftsept30[which(det$day<274)]<-det$day[which(det$day<274)]+93#this should actually vary depending on whether or not it is a leap year
#jdet<-subset(det,select=c(nrep,Jobs,site,day,orcayear,daysaftsept30,season,region))
jdet<-subset(det,select=c(nrep,Jobs,site,day,year,day,season,region))
#i.e. use only sites that have atleast 1 observation in all years

jdet <- jdet[apply(jdet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#jdet<- jdet[apply(jdet,1,function(row) all(row!=0)),]
#kdet<-subset(det,select=c(nrep,Kobs,site,day,orcayear,daysaftsept30,season,region))
kdet<-subset(det,select=c(nrep,Kobs,site,day,year,day,season,region))

kdet <- kdet[apply(kdet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#ldet<-subset(det,select=c(nrep,Lobs,site,day,orcayear,daysaftsept30,season,region))
ldet<-subset(det,select=c(nrep,Lobs,site,day,year,day,season,region))
ldet <- ldet[apply(ldet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#srdet<-subset(det,select=c(nrep,AllSRobs,site,day,orcayear,daysaftsept30,season,region))
srdet<-subset(det,select=c(nrep,AllSRobs,site,day,year,day,season,region))
srdet <- srdet[apply(srdet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

colnames(srdet)[2]<-colnames(jdet)[2]<-colnames(kdet)[2]<-colnames(ldet)[2]<-"ndet"
colnames(srdet)[5]<-colnames(jdet)[5]<-colnames(kdet)[5]<-colnames(ldet)[5]<-"year"


write.csv(srdet,"analyses/output/allsr_dat.csv",row.names = FALSE)
write.csv(ldet,"analyses/output/l_dat.csv",row.names = FALSE)
write.csv(kdet,"analyses/output/k_dat.csv",row.names = FALSE)
write.csv(jdet,"analyses/output/j_dat.csv",row.names = FALSE)
