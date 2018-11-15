#Exploring the orcamaster data frmo 2017
#housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/GitHub/fishphen")

# Load libraries
library(mgcv)
library(dplyr)
# 1. Get the data
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
d<-d[d$Pod.cl!="HB?"|d$Pod.cl!="Not Orcas",]


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
d$Orcas<-1


#6. Add a column for total number of observations during the year to standardize
obs = aggregate(Orcas ~Year, data = d,sum)
colnames(obs)[2]<-"totobsyr"
d2<-left_join(d,obs,b="Year")
# Sum up Chinook for each calendar day across areas
SRs = aggregate(SRKW ~ week, data = d2,sum)
# Fit the gam, using log(effort) as offset
w = as.numeric(SRs$week)
effort = as.numeric(d2$totob)

quartz()
g = gam(log(SRs$SRKW) ~ s(as.numeric(SRs$week)))
plot(w,exp(g$fitted.values), type="l",lwd=3,xlab = "Week of Year",
     ylab = "whale obs")
points(SRs$week,SRs$SRKW)

# Loop over years and look at peak activity date
orcaphen.yrs<-data.frame(year=numeric(length(1976:2017)), 
                         peak=numeric(length(1976:2017)), 
                         stringsAsFactors=FALSE)

quartz(height=8, width=10)
par(mfrow=c(6,7))
for(y in 1976:2017) {
  orcaYear = aggregate(SRKW ~ week, data = d2[which(d2$Year==y),],sum)
  w = as.numeric(orcaYear$week)
  c= as.numeric(orcaYear$SRKW)
  #plot the data
  plot(w,c, pch=21,bg="gray",xlab = "Week", ylab = "Orca observations", main = paste("Year: ",y), bty="l")
  #fit a gam to weekly data
  g = gam(log(c+1) ~ s(w))
  lines(w,exp(g$fitted.values),lwd=3)
  #add line for peak activity week
  pk<-max(g$fitted.values)
  pkdoy<-w[which.max(g$fitted.values)]
  abline(v=pkdoy, col="red", lwd=2)
  orcaphen.yrs$year[y-1975]<-y
  orcaphen.yrs$peak[y-1975]<-pkdoy
}
