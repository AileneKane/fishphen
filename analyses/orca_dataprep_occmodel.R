# Orca analysis: occupancy models
# Started November 1327, 2018
# by Ailene Ettinger ailene.ettinger@noo.gov

#housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/GitHub/fishphen")

# Load libraries

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

#only using fishing areas in Washington's Salish Sea
d<-d[d$FishArea %in% c("01","02","03","04","05","06","07","09","10","11","12","13","81","82"),]#not sure where 17, 18, 19, 20, 28, 29 are...need to find out. also, where is 42583,42584

#Only use fishing areas that have atleast 4 years with >20 observations:
tab.fa.yr<-table(d$FishArea,d$Year)
tab.fa.yr[tab.fa.yr < 20] <- 0
tab.fa.yr[tab.fa.yr >= 20] <- 1
###Need to then select only the fishing areas that are 1 in tab.fa.yr

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
unique(d$FishArea)
d$yrdayfa<-paste(d$Year, d$day,d$FishArea,sep="_")


#Raw detection ratios:
obs = aggregate(Orcas ~yrdayfa, data = d,sum)

# presence of each pod
js = aggregate(J ~yrdayfa, data = d,sum)
ks = aggregate(K~yrdayfa, data = d,sum)
ls = aggregate(L~yrdayfa, data = d,sum)
det<-cbind(js,ks[,2],ls[,2],obs[2])
colnames(det)[2:5]<-c("Jobs","Kobs","Lobs","nrep")

det$year<-substr(det$yrdayfa,1,4)
det$day<-substr(det$yrdayfa,6,8)
det$site<-substr(det$yrdayfa,10,nchar(det$yrdayfa))
det$site<-as.numeric(as.factor(det$site))
det$day<-as.numeric(det$day)
det$year<-as.numeric(det$year)

jdet<-subset(det,select=c(nrep,Jobs,site,day,year))
#remove any rows with 0s in them
#i.e. use only sites that have atleast 1 observation in all years

jdet <- jdet[apply(jdet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#jdet<- jdet[apply(jdet,1,function(row) all(row!=0)),]
kdet<-subset(det,select=c(nrep,Kobs,site,day,year))
kdet <- kdet[apply(kdet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

ldet<-subset(det,select=c(nrep,Lobs,site,day,year))
ldet <- ldet[apply(ldet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
colnames(jdet)[2]<-colnames(kdet)[2]<-colnames(ldet)[2]<-"ndet"
#i coudn't run model code, perhaps because some years have no observations?
#rowSums(table(jdet$site,jdet$year))#There are data in all years and all sites
#or maybe because there are 366 days in the year?
#jdet$day[jdet$day==366]<-365

write.csv(ldet,"analyses/output/l_dat.csv",row.names = FALSE)
write.csv(kdet,"analyses/output/k_dat.csv",row.names = FALSE)
write.csv(jdet,"analyses/output/j_dat.csv",row.names = FALSE)

##The below code does not work anymore
# Make Figure 1 (detectability index bar plot) for J, K, and L pod in 2017
# We will have to decide if we want detectability to vary by fishing area as well
#convert to proportion
#det$Jprop<-det$Jobs/det$totob
#det$Kprop<-det$Kobs/det$totob
#det$Lprop<-det$Lobs/det$totob

#years<-unique(det$year)
#for(i in 1:length(years)){
#  quartz(width=9,height=4)
#  par(mfrow=c(1,3))
#  yrdet=det[det$year==years[i],]
#  barplot(yrdet$Jprop,names.arg=yrdet$wk, main="J")
#  mtext("Detectability index", side=2, line=2, cex=0.9)
#  mtext("Week", side=1, line=2, cex=0.9)
#  barplot(yrdet$Kprop,names.arg=yrdet$wk, main="K")
#  mtext("Week", side=1, line=2, cex=0.9)
#  mtext(paste(years[i]), side=1, line=3)
#  barplot(yrdet$Lprop,names.arg=yrdet$wk, main="L")
#  mtext("Week", side=1, line=2, cex=0.9)
#}
# Questions about full model: what to use for J (number of days in a season)
# I think season length can vary from one year to the next
# Fit the model separately for each pod (min day in the year- max day in the year- may be 365 in some years)