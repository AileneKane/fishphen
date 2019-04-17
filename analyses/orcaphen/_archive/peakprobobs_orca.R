#For Bob!
#Identify peak doy (or week?) to see orcas on San Juan Island
#Plan: 
#Fit gams by DOY or week of year number of obs, with an offset of the total number of observations during the year. 
#housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/GitHub/fishphen")

# Load libraries
library(mgcv)
library(dplyr)
# 1. Get the orca data
d <- read.csv("data/AppendixII.csv")
# 2. Clean the data (also saved in output/AppendixII_cleaned,csv)
source("analyses/clean_orca.R")

  #Create a new column that combines Pod and Likely Pod columna and removes spaces
  d$Pod.cl<-d$Pod


#Always use Likely Pod column, when it is not blank:
d$Pod.cl[d$LikelyPod!="" & d$LikelyPod!=" "]<-d$LikelyPod[d$LikelyPod!="" & d$LikelyPod!=" "]
#perhaps also stick with Pod when LikelyPod has a "?" grep("?",d$LikelyPod,)

d$Pod.cl[d$Pod.cl=="j"|d$Pod.cl==" J"|d$Pod.cl=="J "|d$Pod.cl=="J  "|d$Pod.cl=="J   "|d$Pod.cl=="J?"|d$Pod.cl=="J "|d$Pod.cl=="J  "|d$Pod.cl=="J+"|d$Pod.cl=="Jp"|d$Pod.cl=="Jp "|d$Pod.cl=="Jp  "|d$Pod.cl=="Jp?"| d$Pod.cl=="Js"]<-"J"
d$Pod.cl[d$Pod.cl=="J1 "]<-"J1"
d$Pod.cl[d$Pod.cl=="K "|d$Pod.cl=="K  "|d$Pod.cl=="K?"|d$Pod.cl=="K+"|d$Pod.cl=="Kp"|d$Pod.cl=="KP"|d$Pod.cl=="Kp  "|d$Pod.cl=="Kp  "]<-"K"
d$Pod.cl[d$Pod.cl=="L "|d$Pod.cl=="L  "|d$Pod.cl=="L?"|d$Pod.cl=="L+"|d$Pod.cl=="L+?"|d$Pod.cl=="LP"|d$Pod.cl=="Lp  "|d$Pod.cl=="Lp "|d$Pod.cl=="Lp?"|d$Pod.cl=="Ls"|d$Pod.cl=="Ls?"]<-"L"
d$Pod.cl[d$Pod.cl=="JK "|d$Pod.cl=="JK  "|d$Pod.cl=="JK   "|d$Pod.cl=="J?K?"|d$Pod.cl=="KJ?"|d$Pod.cl=="JK+"|d$Pod.cl=="JKp+"|d$LikelyPod=="Jp+K?"]<-"JK"
d$Pod.cl[d$Pod.cl=="JL "|d$Pod.cl=="JL  "|d$Pod.cl=="JL   "|d$Pod.cl=="JLp? "|d$Pod.cl=="JpLp?"|d$Pod.cl=="JLp"]<-"JL"
d$Pod.cl[d$Pod.cl=="KL "|d$Pod.cl=="KL  "|d$Pod.cl=="KL?"|d$Pod.cl=="KL+? "|d$Pod.cl=="KpLp"|d$Pod.cl=="KpL"|d$Pod.cl=="KpL "|d$Pod.cl=="KpL  "|d$Pod.cl=="KpLp?"|d$Pod.cl=="LK"|d$Pod.cl=="LK?"]<-"KL"
d$Pod.cl[d$Pod.cl=="JL "|d$Pod.cl=="JL  "|d$Pod.cl=="JL   "|d$Pod.cl=="JLp? "|d$Pod.cl=="JpLp?"|d$Pod.cl=="JLp"]<-"JL"
d$Pod.cl[d$Pod.cl=="JKLp"|d$Pod.cl=="JKLm"|d$Pod.cl=="JKl"|d$Pod.cl=="JKL  "|d$Pod.cl=="JKL?"|d$Pod.cl=="JKLm"|d$Pod.cl=="JpKL"|d$Pod.cl=="JKLp"]<-"JKL"

d$Pod.cl[d$Pod.cl=="O?"|d$Pod.cl=="Ts?"]<-"Ts"



sort(unique(d$Pod.cl))

#remove non-orca data
#d<-d[d$Pod.cl!="HB?"|d$Pod.cl!="Not Orcas",]


#4. Add week and day of year (doy)
d$doy<-strftime(strptime(paste(d$Month, d$Day, d$Year, sep="."),format= "%m.%d.%Y"),format= "%j")
d$week<-strftime(strptime(paste(d$Month, d$Day, d$Year, sep="."),format= "%m.%d.%Y"), format = "%V")#new weeks start on mondays

#5. Add a column for presences (1/0) for each pod, for Ts, and for SRKWs

d$J<-0
d$J[grep("J",d$Pod.cl)]<- 1
d$K<-0
d$K[grep("K",d$Pod.cl)]<- 1
d$L<-0
d$L[grep("L",d$Pod.cl)]<- 1
d$SRKW<-0
d$SRKW[grep("SR",d$Pod.cl)]<- 1


d$SRKW[d$J==1|d$K==1|d$L==1]<- 1   
d$Transients<-0
d$Transients[grep("T",d$Pod.cl)]<- 1

d$Orcas<-1
d$Orcas[d$Pod.cl=="Not Orcas"|d$Pod.cl=="HB?"]<-0
d$observations<-1

#6. Add a column for total number of observations during the year to standardize
obs = aggregate(observations ~Year, data = d,sum)
colnames(obs)[2]<-"totobsyr"
d2<-left_join(d,obs,b="Year")

#get total obsfor each day of year
dailyobs = aggregate(observations ~doy, data = d,sum)
weeklyobs = aggregate(observations ~week, data = d,sum)

# Sum up SRs for each calendar day across areas
SRs = aggregate(SRKW ~ doy, data = d2,sum)
SRs.wk = aggregate(SRKW ~ week, data = d2,sum)

#Sum up SRs for each calendar day on San Juan Island (Fish Area=7)
SRs.FA<-tapply(d2$SRKW, list(d2$doy,d2$FishArea),sum)
SRs.FA07<-SRs.FA[,which(colnames(SRs.FA)=="07")]
SRs.FA.wk<-tapply(d2$SRKW, list(d2$week,d2$FishArea),sum)
SRs.FA07.wk<-SRs.FA.wk[,which(colnames(SRs.FA.wk)=="07")]
#
# Fit the gam, using dailyobs as offset
doy = as.numeric(names(SRs.FA07))
week = as.numeric(names(SRs.FA07.wk))

srs= as.numeric(SRs.FA07)
srs.wk= as.numeric(SRs.FA07.wk)

#plot the data

quartz()
  #fit a gam to daily data
  g = gam(log(srs+1) ~ s(doy) + offset(log(dailyobs$observations)))
  
  plot(doy[1:365],exp(g$fitted.values), type="l", xlab= "doy",ylab = "Orca observations")
  points(doy,srs, pch=21,bg="gray")

quartz()
#fit a gam to weekly data
gwk = gam(log(srs.wk+1) ~ s(week) + offset(log(weeklyobs$observations)))

plot(week,exp(gwk$fitted.values), type="l", xaxt="n",ylab = "Orca observations", xlab="Month")
points(week,srs.wk, pch=21,bg="gray" )
axis(1,labels=c("Jan","Mar","May","Jul","Sep","Nov"), at=c(1,9,18,27,35,44))
campdates<-c(12,17,1,8)
campmonth<-c(5,5,6,6)
campweek<-strftime(strptime(paste(campmonth, campdates, 2019, sep="."),format= "%m.%d.%Y"), format = "%V")#new weeks start on mondays

abline(v=campweek,col="red")
#When is the peak week or doy?
pk<-max(g$fitted.values)
pkdoy<-doy[which.max(g$fitted.values)]
pk.wk<-max(gwk$fitted.values)
pkweek<-as.numeric(week[which.max(gwk$fitted.values)])
pkdate<-strftime(strptime(pkdoy,format= "%j"), format = "%m.%d.%Y")#new weeks start on mondays

month<-c(1,3,5,7,9,11)
day<-rep(1, times=6)
strftime(strptime(paste(month, day, 2019, sep="."),format= "%m.%d.%Y"), format = "%V")#new weeks start on mondays
