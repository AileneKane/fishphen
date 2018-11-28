# Orca analysis: occupancy models
# Started November 1327, 2018
# by Ailene Ettinger ailene.ettinger@noo.gov

#housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/GitHub/fishphen")

# 1. Get the data
d <- read.csv("data/AppendixII.csv")

# 2. Clean the data (also saved in output/AppendixII_cleaned,csv)
source("analyses/clean_orca.R")

#Create a new column that combines Pod and Likely Pod columna and removes spaces
d$Pod.cl<-d$Pod


#Always use Likely Pod column, when it is not blank:
d$Pod.cl[d$LikelyPod!="" & d$LikelyPod!=" "]<-d$LikelyPod[d$LikelyPod!="" & d$LikelyPod!=" "]
#perhaps also stick with Pod when LikelyPod has a "?" grep("?",d$LikelyPod,)

#remove non-orca data
d<-d[d$Pod.cl!="HB?"|d$Pod.cl!="Not Orcas",]


#Add week and day of year (doy)
d$doy<-strftime(strptime(paste(d$Month, d$Day, d$Year, sep="."),format= "%m.%d.%Y"),format= "%j")
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

# Restrict analysis to only SRKWs (pods J,K,L)
#d<-d[d$SRKW==1,]#do not do this for now- perhaps this will  reduce the number of "perfect detections"?

#only data after 1978
d<-d[d$Year>1978,]

#Prepare data for running model from strebel et al 2014
#"nrep" "ndet" "site" "day" "year"

# Add a column for total number of observations 
#of all SRKWs during each week of each year year to estimate detection
#d$wkyrfa<-paste(d$Year, d$week,d$FishArea,sep="_")#if we decide to add FishArea to detection estimates for model
#d$wkyr<-paste(d$Year, d$week,sep="_")
d$yrdoyfa<-paste(d$Year, d$doy,d$FishArea,sep="_")
#Raw detection ratios:
obs = aggregate(Orcas ~yrdoyfa, data = d,sum)

# presence of each pod
js = aggregate(J ~yrdoyfa, data = d,sum)
ks = aggregate(K~yrdoyfa, data = d,sum)
ls = aggregate(L~yrdoyfa, data = d,sum)
det<-cbind(js,ks[,2],ls[,2],obs[2])
colnames(det)[2:5]<-c("Jobs","Kobs","Lobs","nrep")

det$year<-substr(det$yrdoyfa,1,4)
det$doy<-substr(det$yrdoyfa,6,8)
det$site<-substr(det$yrdoyfa,10,nchar(det$yrdoyfa))
det$site<-as.factor(as.numeric(det$site))
jdet<-subset(det,select=c(nrep,Jobs,site,doy,year))
jdet <- jdet[apply(jdet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

kdet<-subset(det,select=c(nrep,Kobs,site,doy,year))
kdet <- kdet[apply(kdet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

ldet<-subset(det,select=c(nrep,Lobs,site,doy,year))
ldet <- ldet[apply(ldet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

write.csv(ldet,"analyses/output/l_dat.csv")
write.csv(kdet,"analyses/output/k_dat.csv")
write.csv(jdet,"analyses/output/j_dat.csv")

# Make Figure 1 (detectability index bar plot) for J, K, and L pod in 2017
# We will have to decide if we want detectability to vary by fishing area as well
#convert to proportion
#det$Jprop<-det$Jobs/det$totob
#det$Kprop<-det$Kobs/det$totob
#det$Lprop<-det$Lobs/det$totob

years<-unique(det$year)
for(i in 1:length(years)){
  quartz(width=9,height=4)
  par(mfrow=c(1,3))
  yrdet=det[det$year==years[i],]
  barplot(yrdet$Jprop,names.arg=yrdet$wk, main="J")
  mtext("Detectability index", side=2, line=2, cex=0.9)
  mtext("Week", side=1, line=2, cex=0.9)
  barplot(yrdet$Kprop,names.arg=yrdet$wk, main="K")
  mtext("Week", side=1, line=2, cex=0.9)
  mtext(paste(years[i]), side=1, line=3)
  barplot(yrdet$Lprop,names.arg=yrdet$wk, main="L")
  mtext("Week", side=1, line=2, cex=0.9)
}
# Questions about full model: what to use for J (number of days in a season)
# I think season length can vary from one year to the next
# Fit the model separately for each pod (min day in the year- max day in the year- may be 365 in some years)