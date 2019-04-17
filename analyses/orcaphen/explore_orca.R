#Plot orcamaster data with chinook data, by area
#Goal is to evaluate area of overlap between two species in particular years
#Plan: Divide into seasons (Week0-20, Wekk 21-40, and Week 41-53). this seems to fit with different chinook runs (and possibly with other species runs)
#Start with JPod only
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
# or just jpod?
dat<-read.csv("analyses/output/j_dat.csv",header=T)

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
de
plot(w,exp(g$fitted.values), type="l",lwd=3,xlab = "Week of Year",
     ylab = "whale obs")
points(SRs$week,SRs$SRKW)

# Loop over years and look at peak activity date
orcaphen.yrs<-data.frame(year=numeric(length(1976:2017)), 
                         peak=numeric(length(1976:2017)), 
                         stringsAsFactors=FALSE)
phen.yr.all<-c()
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
  
  #save phenophase weeks into dataframe
  phen.yr<-c(y,exp(pk),pkdoy)
  phen.yr.all<-rbind(phen.yr.all,phen.yr)
}

colnames(phen.yr.all)<-c("year","pk.obs","pk.doy")
rownames(phen.yr.all)<-NULL
phen.yr.all<-as.data.frame(phen.yr.all)
phen.yr.all<-phen.yr.all[phen.yr.all$year<2017,]
phen.yr.all$yeartype<-"odd"
phen.yr.all$yeartype[(phen.yr.all$year %% 2)==0]<-"even"


  #Is there a trend in pk.doy over time?
  trend.mod<-lm(pk.doy~year, data=phen.yr.all)
  summary(trend.mod)#no!
quartz()
plot(phen.yr.all$year,phen.yr.all$pk.doy, pch=21,bg="gray",xlab = "Peak Week", ylab = "Peak DOY of observations")
abline(trend.mod, col="red")

quartz()
plot(phen.yr.all$year,phen.yr.all$pk.doy,xlab = "Peak Week", ylab = "Peak DOY of observations", type="l")





#fit a gam to weekly data
  #Is there a difference bewteen odd and even years?
oddeven.mod<-lm(pk.doy~as.factor(yeartype), data=phen.yr.all)
summary(oddeven.mod)#no!
odd<-phen.yr.all[phen.yr.all$yeartype=="odd",]
even<-phen.yr.all[phen.yr.all$yeartype=="even",]
t.test(odd$pk.doy,even$pk.doy[1:length(odd$pk.doy)], paired=TRUE)
#No difference in odd versus even years



#The below is much messier
#by doy
phen.yr.all.doy<-c()
quartz(height=8, width=10)
par(mfrow=c(6,7))
for(y in 1978:2017) {
  orcaYear = aggregate(SRKW ~ doy, data = d2[which(d2$Year==y),],sum)
  d = as.numeric(orcaYear$doy)
  c= as.numeric(orcaYear$SRKW)
  #plot the data
  plot(d,c, pch=21,bg="gray",xlab = "Doy", ylab = "Orca observations", main = paste("Year: ",y), bty="l")
  #fit a gam to weekly data
  g = gam(log(c+1) ~ s(d))
  lines(d,exp(g$fitted.values),lwd=3)
  #add line for peak activity week
  pk<-max(g$fitted.values)
  pkdoy<-d[which.max(g$fitted.values)]
  abline(v=pkdoy, col="red", lwd=2)
  #save phenophase weeks into dataframe
  phen.yr<-c(y,exp(pk),pkdoy)
  phen.yr.all.doy<-rbind(phen.yr.all.doy,phen.yr)
}


colnames(phen.yr.all.doy)<-c("year","pk.obs","pk.doy")
rownames(phen.yr.all.doy)<-NULL
phen.yr.all.doy<-as.data.frame(phen.yr.all.doy)
phen.yr.all.doy<-phen.yr.all.doy[phen.yr.all.doy$year<2017,]
#phen.yr.all.doy<-phen.yr.all.doy[phen.yr.all.doy$pk.doy>10,]#remove weird outlier with january 1 as peak doy!
cbind(phen.yr.all.doy$year,phen.yr.all.doy$pk.doy)
phen.yr.all.doy$yeartype<-"odd"
phen.yr.all.doy$yeartype[(phen.yr.all.doy$year %% 2)==0]<-"even"
#fit a gam to weekly data
#Is there a difference bewteen odd and even years?
oddeven.mod<-lm(pk.doy~as.factor(yeartype), data=phen.yr.all.doy)
summary(oddeven.mod)#no!
odd<-phen.yr.all.doy[phen.yr.all.doy$yeartype=="odd",]
even<-phen.yr.all.doy[phen.yr.all.doy$yeartype=="even",]
t.test(odd$pk.doy,even$pk.doy[1:length(odd$pk.doy)], paired=TRUE)
#No difference in odd versus even years
boxplot(pk.doy~as.factor(yeartype), data=phen.yr.all.doy)


#Is there a trend in pk.doy over time?
trend.mod.doy<-lm(pk.doy~year, data=phen.yr.all.doy)
summary(trend.mod.doy)#no!
quartz()
plot(phen.yr.all.doy$year,phen.yr.all.doy$pk.doy, pch=21,bg="gray",xlab = "Year", ylab = "Peak DOY of observations")
abline(trend.mod.doy, col="red")

quartz()
plot(phen.yr.all.doy$year,phen.yr.all.doy$pk.doy,xlab = "Year", ylab = "Peak DOY of observations", type="l")





#fit a gam to weekly data
#Is there a difference bewteen odd and even years?
oddeven.mod<-lm(pk.doy~as.factor(yeartype), data=phen.yr.all)
summary(oddeven.mod)#no!
odd<-phen.yr.all[phen.yr.all$yeartype=="odd",]
even<-phen.yr.all[phen.yr.all$yeartype=="even",]
t.test(odd$pk.doy,even$pk.doy[1:length(odd$pk.doy)], paired=TRUE)
#No difference in odd versus even years

#Make plots of fishing areas by year with chinook and orcas present
#Read in the chinook data (WDFW recreational fishing data)
dat<-read.csv("data/2001-2013PSChinookLandings.csv", header=TRUE)
#get dates in day-of-year (doy) and week of year
dat$doy<-strftime(strptime(paste(dat$Month, dat$Day, dat$Year, sep="."),format= "%m.%d.%Y"),format= "%j")
dat$week<-strftime(strptime(paste(dat$Month, dat$Day, dat$Year, sep="."),format= "%m.%d.%Y"), format = "%V")#new weeks start on mondays

#puget sound is areas 5:13 (including 81, 82). I'm curious what the other area numbers are...
#for now, include outer coast and puget sound/salish sea
dat<-dat[dat$CRCarea %in% c("01","02","03","04","05","06","07","09","10","11","12","13","81","82"),]

#Eric's code splits out the records into boat data and fish data
chin= dat[which(is.na(dat$Anglers)==FALSE),]
fish = dat[which(is.na(dat$Anglers)==TRUE),]
# Put the fish caught into the effort database
chin$Chinook = 0
indx = match(paste(fish$SampleDate,fish$LoCode,fish$CRCarea),
             paste(chin$SampleDate,chin$LoCode,chin$CRCarea))
fishCaught = fish$ChinookCaught[-which(is.na(indx))]
chin$Chinook[indx[-which(is.na(indx))]] = fishCaught


#The below code keeps the daily patterns in place and accounts for effort
#use one peak for chinook and one peak for orcas for now

areas<-unique(dat$CRCarea)
#Now try making a separate plot for each CRC area AND year
phen.yr.all<-c()
for(i in 1:length(areas)){
  crcdat = chin[chin$CRCarea==areas[i],]
  orcdat = d2[d2$FishArea==areas[i],]
  crcdat=Chinook
  orcdat=d2
  # Loop over years
  quartz(width=8, height=8)
  par(mfrow=c(4,4), mai=c(1,0.7,0.2,0.4))
  for(y in 2001:2013) {
    # Sum up numbers of chinook by week for each year, across areas
    if(dim(crcdat[which(crcdat$Year==y),])[1]>9){
    chinookYear = aggregate(Chinook ~ week, data = crcdat[which(crcdat$Year==y),],sum)
    w = as.numeric(chinookYear$week)
    c= as.numeric(chinookYear$Chinook)
    #plot the data
    if(dim(chinookYear)[1]>9){
    plot(w,c, pch=21,bg="salmon", xlab= "week",ylab = "Chinook caught", main = paste("Year: ",y,"Area",areas[i]))
    #fit a gam to weekly data
    g = gam(log(c+1) ~ s(w))
    lines(w,exp(g$fitted.values),lwd=3, col="salmon")
    pkc<-max(g$fitted.values)
    pkcdoy<-w[which.max(g$fitted.values)]}
    }
    #add orca data
    if(dim(orcdat[which(orcdat$Year==y),])[1]>9){
    orcaYear = aggregate(SRKW ~ week, data = orcdat[which(orcdat$Year==y),],sum)
    wo = as.numeric(orcaYear$week)
    o= as.numeric(orcaYear$SRKW)
    #plot the data
    par(new = T)
    plot(wo,o, pch=21,bg="gray", xaxt='n', yaxt='n',ann=FALSE)
    axis(side = 4)
    mtext(side = 4, line = 2, 'Orca obs', cex=0.7)
    #fit a gam to weekly data, if there are enough data
    if(length(o)>10){go = gam(log(o+1) ~ s(wo))
          lines(wo,exp(go$fitted.values),lwd=3)}
    pko<-max(go$fitted.values)
    pkodoy<-w[which.max(go$fitted.values)]}
    #save phenophase weeks into dataframe
    phen.yr<-c(y,areas[i],pkc,pkcdoy,pko,pkodoy)
  }  
  phen.yr.all<-rbind(phen.yr.all,phen.yr)
}


#ACross all areas, by year:
crcdat=chin
orcdat=d2
# Loop over years
quartz(width=8, height=8)
par(mfrow=c(4,4), mai=c(1,0.7,0.2,0.4))
for(y in 2001:2013) {
  # Sum up numbers of chinook by week for each year, across areas
    chinookYear = aggregate(Chinook ~ week, data = crcdat[which(crcdat$Year==y),],sum)
    w = as.numeric(chinookYear$week)
    c= as.numeric(chinookYear$Chinook)
    #plot the data
      plot(w,c, pch=21,bg="salmon", xlab= "week",ylab = "Chinook caught", main = paste("Year: ",y))
      #fit a gam to weekly data
      g = gam(log(c+1) ~ s(w))
      lines(w,exp(g$fitted.values),lwd=3, col="salmon")
      pkc<-max(g$fitted.values)
      pkcdoy<-w[which.max(g$fitted.values)]
  
  #add orca data
 
    orcaYear = aggregate(SRKW ~ week, data = orcdat[which(orcdat$Year==y),],sum)
    wo = as.numeric(orcaYear$week)
    o= as.numeric(orcaYear$SRKW)
    #plot the data
    par(new = T)
    plot(wo,o, pch=21,bg="gray", xaxt='n', yaxt='n',ann=FALSE)
    axis(side = 4)
    mtext(side = 4, line = 2, 'Orca obs', cex=0.7)
    #fit a gam to weekly data, if there are enough data
    go = gam(log(o+1) ~ s(wo))
    lines(wo,exp(go$fitted.values),lwd=3)
    pko<-max(go$fitted.values)
    pkodoy<-w[which.max(go$fitted.values)]
  #save phenophase weeks into dataframe
  phen.yr<-c(y,areas[i],pkc,pkcdoy,pko,pkodoy)
}  








chinphen.yrs.cpue
pairs(chinphen.yrs.cpue[,2:10])
#ACross all years
par(new = T)
g = gam(log(SRs$SRKW) ~ s(as.numeric(SRs$week)))

plot(w,exp(g$fitted.values), type="l",lwd=3,xaxt='n', yaxt='n',ann=FALSE)

points(SRs$week,SRs$SRKW)
axis(side = 4)
mtext(side = 4, line = 2, 'Orca obs', cex=0.7)

##Make plots of effort over time, for orcas and chinook
quartz(height=7, width=20)
par(mfrow=c(1,3))

plot(obs$Year,obs$totobsyr,type="b",pch=16,col="black",xlab="Year",ylab="Number of observations", main="Orcas")
chinboats = aggregate(Boats ~Year, data = dat,sum)
chinanglers = aggregate(Anglers ~Year, data = dat,sum)

plot(chinboats$Year,chinboats$Boats,type="b",pch=16,col="black",xlab="Year",ylab="Number of boats", main="Chinook-Boats")
plot(chinanglers$Year,chinanglers$Anglers,type="b",pch=16,col="black",xlab="Year",ylab="Number of Anglers", main="Chinook-Anglers")
