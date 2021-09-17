# Make plots of trends in Lime Kiln SRKW phenology, for main text and supplement supplement
#April 2020
#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)

#add libraries
library(dplyr)
library(RColorBrewer)
library(scales)
# Set working directory: 
#setwd("~/GitHub/fishphen")
#or from laptop:
setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

# if(pod== "SR"){load("analyses/output/sr.brmslog.Rda")}
# if(pod== "J"){load("analyses/output/j.brms.Rda")}
# if(pod== "K"){load("analyses/output/k.brms.Rda")}
# if(pod== "L"){load("analyses/output/l.brms.Rda")}
#load("analyses/output/albionchibrmslog.Rda")

limegests.org<-read.csv("analyses/output/lime_prob.occ.50.csv", header=TRUE)
colnames(limegests.org)[1]<-"styear"
#model estimates have years =1,2,etc. Need to add in actual years
actualyrs<-c(1990,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2014,2015,2016)
yearconvert<-as.data.frame(cbind(unique(limegests.org$styear),actualyrs))
colnames(yearconvert)<-c("styear","year")
limegests<-left_join(limegests.org,yearconvert, copy = TRUE)
limegests<-limegests[limegests$year>1993,]
pod = "SR"#choices are S,J.K,L
minprob = 0.2
get.gests<-function(limegests,pod){
  peakoc.doy<-c()
  peakoc<-c()
  firstprob<-c()
  lastprob<-c()
  meanprobs<-c()
  prob.lc<-c()
  prob.uc<-c()
  years<-c()
  for(y in unique(limegests$year)){
    yeardat<-limegests[limegests$year==y,]
    podcol<-yeardat[,which(colnames(yeardat)==paste(pod,"prob.Estimate",sep=""))]
    yrpeakoc<-max(podcol)
    yrpeakoc.doy<-yeardat$doy[which(podcol==yrpeakoc)]
    yrfirst.doy<-yeardat$doy[min(which(podcol>minprob))]
    yrlast.doy<-yeardat$doy[max(which(podcol>minprob))]
    meanprob<-mean(podcol)
    lprob<-quantile(podcol,0.25)
    uprob<-quantile(podcol,0.75)
    peakoc<-c(peakoc,yrpeakoc)
    peakoc.doy<-c(peakoc.doy,yrpeakoc.doy)
    firstprob<-c(firstprob,yrfirst.doy)
    lastprob<-c(lastprob,yrlast.doy)
    meanprobs<-c(meanprobs,meanprob)
    prob.lc<-c(prob.lc,lprob)
    prob.uc<-c(prob.uc,uprob)
    years<-c(years,y)
  }
  gests<-as.data.frame(cbind(years,peakoc,peakoc.doy,firstprob,lastprob,meanprobs,prob.lc,prob.uc))
  row.names(gests)<-NULL
  return(gests)
}
gests<-get.gests(limegests,"SR")
jgests<-get.gests(limegests,"J")
kgests<-get.gests(limegests,"K")
lgests<-get.gests(limegests,"L")

meanmod<-summary(lm(gests$meanprobs~gests$year))#trend is getting lower
summary(lm(jgests$meanprobs~jgests$year))#not getting lower
summary(lm(kgests$meanprobs~kgests$year))#getting lower
summary(lm(lgests$meanprobs~lgests$year))#getting lower


peakmod<-summary(lm(gests$peakoc.doy~gests$year))#trend is getting later
confint(lm(gests$peakoc.doy~gests$year),level= .75)
summary(lm(jgests$peakoc.doy~jgests$year))# getting later
summary(lm(kgests$peakoc.doy~kgests$year))# getting later
summary(lm(lgests$peakoc.doy~lgests$year))#not getting later

confint(lm(gests$lastprob~gests$year),level= .75)
confint(lm(gests$firstprob~gests$year), level=.75)
firstmod<-summary(lm(gests$firstprob~gests$year))#trend is getting later
pdf("analyses/orcaphen/figures/phentrends_lime_peak.pdf",height=6,width=12)
#quartz(height=6,width=12)
par(mfrow=c(1,3),mar=c(7, 5, 4, 2) + 0.1)
alph=0.90
myPalette <- colorRampPalette(brewer.pal(length(unique(gests$year)), "Blues")) #### Gives us a heat map look
cols = rev(myPalette(length(unique(gests$year))))

plot(gests$year,gests$firstprob,xlab= "Year", ylab= "Arrival Day of Year", pch=21, bty="l", cex.axis=1.5,cex.lab=1.6,cex=1.8,bg=cols[factor(gests$year)])
mtext(paste("A"), side = 3, line = 1, adj=0)
if(firstmod$coef[2,4]<(1-alph)){abline(firstmod$coef[,1], lty=1, lwd=2)}
print(paste("r2=",round(firstmod$r.squared, digits=2),",p=",round(firstmod$coeff[2,4], digits=3)), side=3, adj=1, cex=0.7)
print(paste("coef=",round(firstmod$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)

plot(gests$year,gests$peakoc.doy,xlab= "Year", ylab= "Day of Peak SRKW Occupancy Prob.", pch=21, bty="l", cex.axis=1.5,cex.lab=1.6,cex=1.8,bg=cols[factor(gests$year)])
if(peakmod$coef[2,4]<(1-alph)){abline(peakmod$coef[,1], lty=1, lwd=2)}
print(paste("r2=",round(peakmod$r.squared, digits=2),",p=",round(peakmod$coeff[2,4], digits=3)), side=3, adj=1, cex=0.7)
print(paste("coef=",round(peakmod$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
mtext("B)", side = 3, line = 1, adj=0)

plot(gests$year,gests$meanprobs,xlab= "Year", ylab= "Mean Occurrence Probability", pch=21,  cex.axis=1.5,cex.lab=1.6,bty="l",cex=1.5,bg=cols[factor(gests$year)])
mtext("C)", side = 3, line = 1, adj=0)
if(meanmod$coef[2,4]<(1-alph)){abline(meanmod$coef[,1], lty=1, lwd=2)}
print(paste("r2=",round(meanmod$r.squared, digits=2),",p=",round(meanmod$coeff[2,4], digits=3)), side=3, adj=1, cex=0.7)
print(paste("coef=",round(meanmod$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
 

#quartz(height=4,width=12)
albchiphenest<-read.csv("analyses/output/albionchiphenest.csv", header=TRUE)
albchiphenbrms<-read.csv("analyses/output/albionchiphenestbrms.csv", header=TRUE)

albchiphen<-read.csv("analyses/output/albionchiphen.csv", header=TRUE)

#restrict SRKW and chin data to consistent years
albchinest90<-albchiphenest[albchiphenest$year>1993,]
albchinest90<-albchinest90[albchinest90$year<2017,]

albchiphen90<-albchiphen[albchiphen$year>1993,]
albchiphen90<-albchiphen90[albchiphen90$year<2017,]

albchiphenbrms90<-albchiphenbrms[albchiphenbrms$year>1993,]
albchiphenbrms90<-albchiphenbrms90[albchiphenbrms90$year<2017,]

albchinest90<-albchinest90[albchinest90$year!=2013,]
albchiphen90<-albchiphen90[albchiphen90$year!=2013,]
albchiphenbrms90<-albchiphenbrms90[albchiphenbrms90$year!=2013,]

gests<-gests[gests$years>1990,]
#myPalette <- colorRampPalette(brewer.pal(length(unique(albchinest90$year)), "Blues")) #### Gives us a heat map look
#cols = rev(myPalette(length(unique(albchinest90$year))))
#png(file="analyses/orcaphen/figures/lime_albchin_gam.png",height=1500,width=4500, res = 300)
#quartz()
#par(mfrow=c(1,3), mar=c(5, 5, 4, 2) + 0.1)

plot(albchinest90$peakobsdate,gests$peakoc.doy,type="p",pch=21,  cex.axis=1.5,cex.lab=1.6,bg = cols[factor(albchinest90$year)],xlab="Day of Peak Chinook Abundance",ylab="Day of Peak SRKW Occupancy Prob.", cex=1.8, bty="l")
mod<-lm(gests$peakoc.doy~albchiphen90$peakobsdate)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3,  lwd=2)}
mtext("D)", side = 3, line = 1, adj=0)
print(summary(mod))
albchinest90$alltotal<-as.numeric(albchinest90$alltotal)
plot(albchinest90$alltotal,gests$peakoc.doy, type="p",pch=21, cex.axis=1.5,cex.lab=1.6,bg = cols[factor(albchinest90$year)],xlab="Chinook Abundance Index (CPUE)",ylab="Day of Peak SRKW Occupancy Prob.", cex=1.8, bty="l")
mod<-lm(gests$peakoc.doy~albchinest90$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3,  lwd=2)}
mtext("E)", side = 3, line = 1, adj=0)
#Now calculate Eric's new whale days metric:
limewhaledays.prob<-aggregate(limegests$SRprob.Estimate, by=list(limegests$year), sum)
colnames(limewhaledays.prob)<-c("year","whdays.prob")
print(summary(mod))

plot(albchinest90$alltotal,limewhaledays.prob$whdays.prob,type="p",pch=21, bg = cols[factor(albchinest90$year)], cex.axis=1.5,cex.lab=1.6,xlab="Chinook Abundance Index (CPUE)",ylab="Whale days (#)", cex=1.8, bty="l")

mod<-lm(limewhaledays.prob$whdays.prob~albchinest90$alltotal)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3, lwd=2)}
mtext("F)", side = 3, line = 1, adj=0)
print(summary(mod))

dev.off()


#comparing different models with measured data:
plot(albchinest90$peakobsdate,gests$peakoc.doy,type="p",pch=21,  cex.axis=1.5,cex.lab=1.6,bg = cols[factor(albchinest90$year)],xlab="Day of Peak Chinook Abundance",ylab="Day of Peak SRKW Occupancy Prob.", cex=1.8, bty="l")
mod<-lm(gests$peakoc.doy~albchinest90$peakobsdate)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3,  lwd=2)}
mtext("D)", side = 3, line = 1, adj=0)
print(summary(mod))

albchiphen90$peakobsdate<-as.numeric(albchiphen90$peakobsdate)
albchiphen90$alltotal<-as.numeric(albchiphen90$alltotal)

plot(albchiphen90$peakobsdate,gests$peakoc.doy,type="p",pch=21,  cex.axis=1.5,cex.lab=1.6,bg = cols[factor(albchinest90$year)],xlab="Day of Peak Chinook Abundance",ylab="Day of Peak SRKW Occupancy Prob.", cex=1.8, bty="l")
mod<-lm(gests$peakoc.doy~albchiphen90$peakobsdate)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3,  lwd=2)}
mtext("D)", side = 3, line = 1, adj=0)
print(summary(mod))


