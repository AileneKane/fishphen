# Make plots for supplement
#April 2020
#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)


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
confint(lm(gests$peakoc.doy~gests$year),level= .95)
summary(lm(jgests$peakoc.doy~jgests$year))# getting later
summary(lm(kgests$peakoc.doy~kgests$year))# getting later
summary(lm(lgests$peakoc.doy~lgests$year))#not getting later

confint(lm(gests$lastprob~gests$year),level= .95)
confint(lm(gests$firstprob~gests$year), level=.95)
firstmod<-summary(lm(gests$firstprob~gests$year))#trend is getting later

png(filename="analyses/orcaphen/figures/phentrends_lime_peak.png",height=400,width=1200)
#quartz(height=5,width=15)
par(mfcol=c(1,3))
alph=0.95
plot(gests$year,gests$firstprob,xlab= "Year", ylab= "Arrival Day of Year", pch=21, bty="l",cex=1.5,bg="darkblue",main = "All Pods")
mtext(paste("A"), side = 3, line = 1, adj=0)
if(firstmod$coef[2,4]<(1-alph)){abline(firstmod, lty=1, lwd=2)}
mtext(paste("r2=",round(firstmod$r.squared, digits=2),",p=",round(firstmod$coeff[2,4], digits=3)), side=3, adj=1, cex=0.7)
mtext(paste("coef=",round(firstmod$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)

plot(gests$year,gests$peakoc.doy,xlab= "Year", ylab= "Peak Occurrence Probability Day of Year", pch=21, bty="l",cex=1.5,bg="darkblue",main = "All Pods")
if(peakmod$coef[2,4]<(1-alph)){abline(peakmod, lty=1, lwd=2)}
mtext(paste("r2=",round(peakmod$r.squared, digits=2),",p=",round(peakmod$coeff[2,4], digits=3)), side=3, adj=1, cex=0.7)
mtext(paste("coef=",round(peakmod$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
mtext("B)", side = 3, line = 1, adj=0)

  plot(gests$year,gests$meanprobs,xlab= "Year", ylab= "Mean Occurrence Probability", pch=21, bty="l",cex=1.5,bg="darkblue")
  mtext("C)", side = 3, line = 1, adj=0)
  if(meanmod$coef[2,4]<(1-alph)){abline(meanmod, lty=1, lwd=2)}
  mtext(paste("r2=",round(meanmod$r.squared, digits=2),",p=",round(meanmod$coeff[2,4], digits=3)), side=3, adj=1, cex=0.7)
  mtext(paste("coef=",round(meanmod$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
  
}
dev.off()
