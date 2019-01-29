#Identify peak doy (or week?) of observing orcas in salish sea and in puget sound
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

#sort(unique(d$Pod.cl))

#remove non-orca data
#d<-d[d$Pod.cl!="HB?"|d$Pod.cl!="Not Orcas",]


#4. Add week and day of year (doy)
d$doy<-strftime(strptime(paste(d$Month, d$Day, d$Year, sep="."),format= "%m.%d.%Y"),format= "%j")
d$week<-strftime(strptime(paste(d$Month, d$Day, d$Year, sep="."),format= "%m.%d.%Y"), format = "%W")#new weeks start on mondays

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

#only use data after 1978
d<-d[d$Year>1978,]

#Only use observations in Washington's Salish sea
d<-d[d$FishArea %in% c("01","02","03","04","05","06","07","09","10","11","12","13","81","82"),]#not sure where 17, 18, 19, 20, 28, 29 are...need to find out. also, where is 42583,42584

#add region. assign to ps (puget sound) or uss (upper salish sea) using fishing area
d$region<-"ps"
d$region[d$FishArea=="07"|d$FishArea=="06"|d$FishArea=="02"|d$FishArea=="04"]<-"uss"
d$season<-"winter"#winter=Oct-April
d$season[d$day>121 & d$day<274]<-"summer"
#add an "orca year" which runs Oct 1-Sept 31
d$orcayear<-NA
d$orcayear[which(d$Month<10)]<-d$Year[which(d$Month<10)]
d$orcayear[which(d$Month>=10 & d$Day>=1)]<-d$Year[which(d$Month>=10 & d$Day>=1)]+1
d$weeksaftsept30<-NA
d$weeksaftsept30[which(d$Month>=10 & d$Day>=1)]<-as.numeric(d$week[which(d$Month>=10 & d$Day>=1)])-as.numeric(strftime(strptime(paste(9, 30, d$Year[which(d$Month>=10 & d$Day>=1)], sep="."),format= "%m.%d.%Y"), format = "%W"))#new weeks start on mondays
#figure out how many weeks between 0ct 1 and jan 1 for each year
d$weekstoaddsept30<-NA
d$weekstoaddsept30[which(d$Month<10)]<-as.numeric(strftime(strptime(paste(12, 31, d$Year[which(d$Month<10)]-1, sep="."),format= "%m.%d.%Y"), format = "%W"))-as.numeric(strftime(strptime(paste(09, 30, d$Year[which(d$Month<10)]-1, sep="."),format= "%m.%d.%Y"), format = "%W"))#new weeks start on mondays
d$weeksaftsept30[which(d$Month<10)]<-as.numeric(d$week[which(d$Month<10)])+d$weekstoaddsept30[which(d$Month<10)]

#now do days
d$daysaftsept30<-NA
d$daysaftsept30[which(d$Month>=10 & d$Day>=1)]<-as.numeric(d$doy[which(d$Month>=10 & d$Day>=1)])-as.numeric(strftime(strptime(paste(9, 30, d$Year[which(d$Month>=10 & d$Day>=1)], sep="."),format= "%m.%d.%Y"), format = "%j"))
#figure out how many days between 0ct 1 and jan 1 for each year
d$daystoaddsept30<-NA
d$daystoaddsept30[which(d$Month<10)]<-as.numeric(strftime(strptime(paste(12, 31, d$Year[which(d$Month<10)]-1, sep="."),format= "%m.%d.%Y"), format = "%j"))-as.numeric(strftime(strptime(paste(09, 30, d$Year[which(d$Month<10)]-1, sep="."),format= "%m.%d.%Y"), format = "%j"))
d$daysaftsept30[which(d$Month<10)]<-as.numeric(d$doy[which(d$Month<10)])+d$daystoaddsept30[which(d$Month<10)]

#6. Get total number of observations during the year to standardize
obs = aggregate(observations ~Year, data = d,sum)
colnames(obs)[2]<-"totobsyr"
orcayr.obs = aggregate(observations ~orcayear, data = d,sum)
colnames(orcayr.obs)[2]<-"totobsyr"
#remove incomplete orcayeears:
orcayr.obs<-orcayr.obs[2:(dim(orcayr.obs)[1]-1),]



region<-unique(d$region)
pod<-c("J","K","L","SRKW")
figpath <- "analyses/figures/gams_year"


#The below code yields weird models...
#for each year, region and pod, estimate peak day of observed activity
allpeakest<-c()
for (i in 1:length(region)){
  if(region[i]=="uss"){years<-obs$Year}
  if(region[i]=="ps"){years<-orcayr.obs$orcayear}
  for(p in 1:length(pod)){
    for(y in years){
      if(region[i]=="uss"){
        dat<-d[d$Year==y,]
        dat<-dat[dat$Month>4 & dat$Month<10,]#for upper salish sea, look at summer phenology- may-oct
        }
      if(region[i]=="ps"){
        dat<-d[d$orcayear==y,]
        dat<-dat[dat$Month>=10 |dat$Month<3,]#for puget, look at winter phenology- oct-feb
        }
      
      #get effort estimate (total observations per day, across all salish sea and all pods)
      #if(region[i]=="uss"){
      #  obs = aggregate(observations ~week, data = dat,sum)
       # obs.doy = aggregate(observations ~doy, data = dat,sum)}
      #if(region[i]=="ps"){
      #  obs = aggregate(observations ~weeksaftsept30, data = dat,sum)
      #  obs.doy = aggregate(observations ~daysaftsept30, data = dat,sum)}
      
      dat<-dat[dat$region==region[i],]
      dat$pod.obs<-dat[,which(colnames(dat)==pod[p])]
      
      #aggregate by week and doy
      if(region[i]=="uss"){
      pod.wk = aggregate(pod.obs~ week, data = dat,sum)
      #pod.wk<-full_join(pod.wk,obs)
      pod.wk<-pod.wk[order(pod.wk$week),]
      week = as.numeric(pod.wk$week)
      pod.doy = aggregate(pod.obs~ doy, data = dat,sum)
      #pod.doy<-left_join(pod.doy,obs.doy)
      pod.doy<-pod.doy[order(pod.doy$doy),]
      #pod.doy$pod.obs[which(is.na(pod.doy$pod.obs))]<-0
      c= as.numeric(pod.doy$pod.obs)
      #plot the data
      plot(doy,c, pch=21,bg="gray",xlab = "Days after Sept 30", ylab = "Orca observations", main = paste("Year: ",y), bty="l")
      #fit a gam to daily data
      g = gam(log(c+1) ~ s(doy))
      lines(d,exp(g$fitted.values),lwd=3)
      #add line for peak activity week
      doy = as.numeric(pod.doy$doy)
      pk<-max(g$fitted.values)
      pkdoy<-d[which.max(g$fitted.values)]
      abline(v=pkdoy, col="red", lwd=2)
      
      }
     
      if(region[i]=="ps"){
      pod.wk = aggregate(pod.obs~ weeksaftsept30, data = dat,sum)
      #pod.wk<-full_join(pod.wk,obs)
      pod.wk<-pod.wk[order(pod.wk$week),]
      week = as.numeric(pod.wk$weeksaftsept30)
      pod.doy = aggregate(pod.obs~ daysaftsept30, data = dat,sum)
      #pod.doy<-left_join(pod.doy,obs.doy, by="daysaftsept30")
      pod.doy<-pod.doy[order(pod.doy$daysaftsept30),]
      #pod.doy$pod.obs[which(is.na(pod.doy$pod.obs))]<-0
      doy = as.numeric(pod.doy$daysaftsept30)
      c= as.numeric(pod.doy$pod.obs)
      #plot the data
      plot(doy,c, pch=21,bg="gray",xlab = "Days after Sept 30", ylab = "Orca observations", main = paste("Year: ",y), bty="l")
      #fit a gam to daily data
      g = gam(log(c+1) ~ s(doy))
      lines(d,exp(g$fitted.values),lwd=3)
      #add line for peak activity week
      pk<-max(g$fitted.values)
      pkdoy<-d[which.max(g$fitted.values)]
      abline(v=pkdoy, col="red", lwd=2)
       }
   
      #try binomial gam
      # if(dim(dat)[1]>10){
      # #g = gam(log(podobs+1) ~ s(doy) + offset(log(pod.doy$observations)))} else(next)
      # #g = gam(podobs ~ s(doy))
      #   gbin<-gam(SRKW~s(daysaftsept30), data=dat, family="binomial")
      #   #pod.wk$pod.obs[which(is.na(pod.wk$pod.obs))]<-0
      #   podobs<-pod.doy$pod.obs
      #   
      #   mod = glm(SRKW~as.factor(daysaftsept30), data=dat, family="binomial")
      # } else(next)
      # #g.notlog = gam(podobs ~ s(week,bs="cc") + offset(pod.wk$observations))#nonsensical
      # 
      #pdf(file.path(figpath, paste(pod[p],y,region[i],"tempforecast.pdf", sep="_")), width = 9, height = 6)
      #quartz()
      # plot(dat$daysaftsept30,plogis(gbin$fitted.values), type="l", xaxt="n",ylab = "Probability of observing orcas", xlab="Month", main=paste(pod[p],region[i],y,sep=" "), ylim=c(0,1))
      # lines(dat$daysaftsept30,mod$fitted.values, col="blue")
      # #points(dat$daysaftsept30,mod$fitted.values, col="blue")
      
      #plot(week,exp(g.no.off$fitted.values), type="l", xaxt="n",ylab = "Orca observations", xlab="Month", main=paste(pod[p],region[i],y,sep=" "))
      #plot(week,exp(g.notlog$fitted.values), type="l", xaxt="n",ylab = "Orca observations", xlab="Month", main=paste(pod[p],region[i],y,sep=" "))
      # points(dat$daysaftsept30,dat$SRKW, pch=21,bg="gray" )
      # if(region[i]=="uss"){axis(1,labels=c("Jan","Mar","May","Jul","Sep","Nov"), at=c(1,9,18,27,35,44))}
      # if(region[i]=="ps"){axis(1,labels=c("1Sep","1Nov","1Jan"), at=c(1,62,123))}
      # 
      # pk<-max(gbin$fitted.values)
      # pktim<-as.numeric(doy[which.max(gbin$fitted.values)])
      # pk.glm<-max(mod$fitted.values)
      # pktim.glm<-as.numeric(doy[which.max(mod$fitted.values)])
      # 
      # abline(v=pktim,col="red")
      # abline(v=pktim.glm,col="blue")
      
      #pk.wk<-max(g$fitted.values)
      #pkweek<-as.numeric(week[which.max(g$fitted.values)])
      #abline(v=pkweek,col="red")
      peakest<-c(y,pk,pkdoy,pod[p],region[i])
      allpeakest<-rbind(peakest,allpeakest)
      dev.off()
    }
  }
}

allpeakest<-as.data.frame(allpeakest)
colnames(allpeakest)<-c("year","pkdoy.gam","pkdoy.glm","pod","region")

quartz()
plot(as.numeric(allpeakest$year),as.numeric(allpeakest$pkdoy.gam), type="p",pch=21,bg="gray")
points(as.numeric(allpeakest$year),as.numeric(allpeakest$pkdoy.glm))





#looking for peak across all years
# Sum up SRs for each calendar day across areas
SRs = aggregate(SRKW ~ doy, data = d2,sum)

#
# Fit the gam, using dailyobs as offset
doy = as.numeric(names(SRs.FA07))

srs= as.numeric(SRs.FA07)
srs.wk= as.numeric(SRs.FA07.wk)

#plot the data

quartz()
  #fit a gam to daily data
  
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
pkdate<-strftime(strptime(pkdoy,format= "%j"), format = "%m.%d.%Y")#new weeks start on mondays

month<-c(1,3,5,7,9,11)
day<-rep(1, times=6)
strftime(strptime(paste(month, day, 2019, sep="."),format= "%m.%d.%Y"), format = "%V")#new weeks start on mondays
