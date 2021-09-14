# Make plots of trends in Lime Kiln SRKW phenology, for main text and supplement supplement
#August 2021- this is a reconfiguration of previous versions of figures
#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)

#add libraries
library(dplyr)
library(scales)

# Set working directory: 
#setwd("~/GitHub/fishphen")
#or from laptop:
setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

limegests.org<-read.csv("analyses/output/lime_prob.occ.75.csv", header=TRUE)
colnames(limegests.org)[1]<-"styear"
#model estimates have years =1,2,etc. Need to add in actual years
actualyrs<-c(1990,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2014,2015,2016)
yearconvert<-as.data.frame(cbind(unique(limegests.org$styear),actualyrs))
colnames(yearconvert)<-c("styear","year")
limegests<-left_join(limegests.org,yearconvert, copy = TRUE)
limegests<-limegests[limegests$year>1993,]
plotpdf = TRUE #if false, plot as png
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
    lprob<-quantile(podcol,0.125)
    uprob<-quantile(podcol,0.875)
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

#Run linear models to quantify trends
meanmod<-lm(gests$meanprobs~gests$year)
summary(meanmod)  #trend is getting lower
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

#Load model 
#limegests$year<-as.numeric(rep(seq(1994,2017,by = 1), each = 75))
if(pod== "SR"){limegests$prob.occ<-limegests$SRprob.Estimate
podname = "SRKW"
}
if(pod== "J"){limegests$prob.occ<-limegests$Jprob.Estimate
podname = "J pod"
}
if(pod== "K"){limegests$prob.occ<-limegests$Kprob.Estimate
podname = "K pod"
}
if(pod== "L"){limegests$prob.occ<-limegests$Lprob.Estimate
podname = "L pod"
}

  brkyr<-2006
  orcasum.days.lime1<-limegests[limegests$year>=1993 & limegests$year<brkyr,]
  orcasum.days.lime2<-limegests[limegests$year>=brkyr & limegests$year<2018,]
  
  wdays.old<-cbind(aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),mean),aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),sd)$x,aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),length)$x)
  wdays.rec<-cbind(aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),mean),aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),sd)$x,aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),length)$x)
  colnames(wdays.old)<-colnames(wdays.rec)<-c("doy","meanocc","sdocc","n")
  
  #Make figure
  if(plotpdf==FALSE){png("analyses/orcaphen/figures/srkwphentrends_lime.png",height=600,width=1200, res=100)}
  if(plotpdf==TRUE){pdf("analyses/orcaphen/figures/srkwphentrends_lime.pdf",height=6,width=12)}
  #quartz(height=5,width=10)
  par(mfrow=c(1,3),mar=c(7, 5, 4, 2) + 0.1)
  alph=0.75
  #if(pod =="SR" & brkyr==2006) {par(oma=c(1,1.5,1,3), mar=c(4,4.5,4,6),mfrow=c(1,3))
  #} else {par(oma=c(1,1.5,1,3), mar=c(4,4.5,4,6),mfrow=c(1,2))}
  #observatoins made at lime kiln from may 20 (doy 140) through august 10 (doy 222/3 in leap years)
  plot(wdays.old$doy,wdays.old$meanocc, type="l",lty=2, lwd=2,col="darkblue", xlim=c(140,223), ylim=c(0,1.1),  ylab="Probability of Occurrence",xlab="Day of Year", bty="l", cex.axis = 1.5, cex.lab=1.5)
  polygon(c(rev(wdays.old$doy),wdays.old$doy),c(rev(wdays.old$meanocc+wdays.old$sdocc),wdays.old$meanocc-wdays.old$sdocc),col=alpha("darkblue",0.05),lty=0)
  polygon(c(rev(wdays.rec$doy),wdays.rec$doy),c(rev(wdays.rec$meanocc+wdays.rec$sdocc),wdays.rec$meanocc-wdays.rec$sdocc),col=alpha("darkblue",0.05),lty=0)
  
  lines(wdays.rec$doy,wdays.rec$mean, lwd=2,col="darkblue")
  mtext("A)", side=3, line=2, adj=-.2, cex=1.5)
  #axis(side = 4)
  
  #mtext(paste(podname,"presence",sep=" "),side=4, adj=.5, line=2)
  #legend(135,1.2,legend=c(paste(brkyr,"2017",sep="-"),paste("1994",(brkyr-1),sep="-")),lty=c(1,2),lwd=2,col="darkblue", bty="n", cex=1.3)
  legend(130,1.1,legend=c(paste("1994",(brkyr-1),sep="-"),paste(brkyr,"2017",sep="-")),lty=c(2,1),lwd=2,col="darkblue", bty="n", cex=1.3)
  
  #Plot mean peak day across early vs late time periods
  #gests<-get.gests(limegests,"prob.occ")
  lci=.125
  uci=0.875
  
  #old time period
  pkocdoy<-mean(gests$peakoc.doy[gests$year<brkyr & gests$year>1993])
  pkocdoy.lci<-quantile(gests$peakoc.doy[gests$year<brkyr & gests$year>1993],lci)
  pkocdoy.uci<-quantile(gests$peakoc.doy[gests$year<brkyr & gests$year>1993],uci)
  arrows(pkocdoy.lci,1.1,pkocdoy.uci,1.1,code = 0, col = "darkblue", lty=2,lwd = 3)
  points(pkocdoy,1.1, pch = 21, bg="darkblue", cex = 2)
  
  #recent time period
  pkocdoy<-mean(gests$peakoc.doy[gests$year>=brkyr])
  pkocdoy.lci<-quantile(gests$peakoc.doy[gests$year>=brkyr],lci)
  pkocdoy.uci<-quantile(gests$peakoc.doy[gests$year>=brkyr],uci)
  arrows(pkocdoy.lci,1.05,pkocdoy.uci,1.05,code = 0, col = "darkblue", lty=1,lwd = 3)
  points(pkocdoy,1.05, pch = 21, bg="darkblue", cex = 2)
 
  #Peak prob
  plot(gests$year,gests$peakoc.doy,xlab= "Year", ylab= "Day of Peak Occupancy Prob.", pch=21, bty="l", cex.axis=1.5,cex.lab=1.6,cex=1.8,bg="dark blue")
  if(peakmod$coef[2,4]<(1-alph)){abline(peakmod, lty=1, lwd=2)}
  print(paste("r2=",round(peakmod$r.squared, digits=2),",p=",round(peakmod$coeff[2,4], digits=3)), side=3, adj=1, cex=0.7)
  print(paste("coef=",round(peakmod$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
  mtext("B)", side=3, line=2, adj=-.2, cex=1.5)
  
#Whale days-calculate Eric's new whale days metric:
limewhaledays.prob<-aggregate(limegests$SRprob.Estimate, by=list(limegests$year), sum)
colnames(limewhaledays.prob)<-c("year","whdays.prob")
yearsum.lc<-aggregate(limegests$SRprob.Q12.5,by=list(limegests$year),sum)
yearsum.uc<-aggregate(limegests$SRprob.Q87.5,by=list(limegests$year),sum)
colnames(yearsum.lc)<-colnames(yearsum.uc)<-c("year","wdays")

  plot(limewhaledays.prob$year,limewhaledays.prob$whdays.prob,type = "l", col="darkblue",cex.axis=1.5,cex.lab=1.6,xlab="Year",ylab="Whale days", cex=1.8, bty="l", lwd=2)
  polygon(c(rev(as.numeric(yearsum.lc$year)),as.numeric(yearsum.uc$year)), c(rev(yearsum.uc$wdays), yearsum.lc$wdays), col = alpha("darkblue", 0.2), border = NA)
  
  #mod<-lm(limewhaledays.prob$whdays.prob~limewhaledays.prob$year)
  #if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
  
  #if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3, lwd=2)}
  mtext("C)", side=3, line=2, adj=-.2, cex=1.5)
  #print(summary(mod))
  dev.off() 

  
  #plot(gests$year,gests$firstprob,xlab= "Year", ylab= "Arrival Day of Year", pch=21, bty="l", cex.axis=1.5,cex.lab=1.6,cex=1.8,bg="dark blue")
#mtext(paste("E)"), side = 3, line = 1, adj=0)
#if(firstmod$coef[2,4]<(1-alph)){abline(firstmod, lty=1, lwd=2)}
#print(paste("r2=",round(firstmod$r.squared, digits=2),",p=",round(firstmod$coeff[2,4], digits=3)), side=3, adj=1, cex=0.7)
#print(paste("coef=",round(firstmod$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)

#plot(gests$year,gests$lastprob,xlab= "Year", ylab= "Departure Day of Year", pch=21, bty="l", cex.axis=1.5,cex.lab=1.6,cex=1.8,bg="dark blue")
#mtext(paste("F)"), side = 3, line = 1, adj=0)
#if(firstmod$coef[2,4]<(1-alph)){abline(firstmod, lty=1, lwd=2)}
#print(paste("r2=",round(firstmod$r.squared, digits=2),",p=",round(firstmod$coeff[2,4], digits=3)), side=3, adj=1, cex=0.7)
#print(paste("coef=",round(firstmod$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)

  #Mean probability of occurrence across the time series
#   plot(gests$year,gests$meanprobs,xlab= "Year", ylab= "Mean Occurrence Probability", pch=21,  cex.axis=1.5,cex.lab=1.6,bty="l",cex=1.5,bg="dark blue")
#   mtext("C)", side=3, line=2, adj=-.2, cex=1.5)
#   if(meanmod$coef[2,4]<(1-alph)){abline(meanmod, lty=1, lwd=2)}
#   print(paste("r2=",round(meanmod$r.squared, digits=2),",p=",round(meanmod$coeff[2,4], digits=3)), side=3, adj=1, cex=0.7)
#   print(paste("coef=",round(meanmod$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
#   
# 
# #
# dev.off()
