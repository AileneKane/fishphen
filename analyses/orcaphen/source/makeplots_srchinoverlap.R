# if(pod== "SR"){load("analyses/output/sr.brmslog.Rda")}
# if(pod== "J"){load("analyses/output/j.brms.Rda")}
# if(pod== "K"){load("analyses/output/k.brms.Rda")}
# if(pod== "L"){load("analyses/output/l.brms.Rda")}
#load("analyses/output/albionchibrmslog.Rda")

#pod = "SR"
#dim(limegests)
albchinphenest<-get.gests.chin(chinab,"cpue.est")
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
bryears<-c(2005,2006,2007,2008)
for(i in 1:length(bryears)){
  brkyr<-bryears[i]
orcasum.days.lime1<-limegests[limegests$year>1993 & limegests$year<brkyr,]
orcasum.days.lime2<-limegests[limegests$year>=brkyr & limegests$year<2018,]

wdays.old<-cbind(aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),mean),aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),sd)$x,aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),length)$x)
wdays.rec<-cbind(aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),mean),aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),sd)$x,aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),length)$x)
colnames(wdays.old)<-colnames(wdays.rec)<-c("doy","meanocc","sdocc","n")
wdays.old$seocc<-wdays.old$sdocc/sqrt(wdays.old$n)
#chinab<-read.csv("analyses/output/albiongamests.csv", header = TRUE)
chinab<-read.csv("analyses/output/albionbrmsests.csv", header = TRUE)
#restrict days
chinab<-chinab[chinab$doy<268,]
chinab<-chinab[chinab$doy>90,]

chinab.old<-chinab[chinab$year>1993 & chinab$year<brkyr,] 
chinab.rec<-chinab[chinab$year>=brkyr & chinab$year<2018,] 
cpue.old<-cbind(aggregate(chinab.old$cpue,by=list(chinab.old$doy),mean), aggregate(chinab.old$cpue,by=list(chinab.old$doy),sd)$x)
cpue.rec<-cbind(aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),mean),aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),sd)$x)
colnames(cpue.old)<-colnames(cpue.rec)<-c("doy","cpue.mean","cpue.sd")


pdf(paste("analyses/orcaphen/figures/orcachinphenoverlapbrms",pod,brkyr,".pdf",sep=""),height=5, width=15)
#png(paste("analyses/orcaphen/figures/orcachinphenoverlap",pod,brkyr,".png",sep=""),height=400, width=1200)

# to figure out how much to shift the salmon curve earlier- because they are measured at ft. langely on the frasier river, but we are interested in when they are at lime kiln
# lime  kiln is ~160 km from lime kiln "as the fish swims" and fish swim about 70 km per day! only need to shift by 2-3 days?
#windows(height=6, width=12)
if(pod =="SR" & brkyr==2006) {par(oma=c(1,1.5,1,3), mar=c(4,4.5,4,6),mfrow=c(1,3))
} else {par(oma=c(1,1.5,1,3), mar=c(4,4.5,4,6),mfrow=c(1,2))}
#observatoins made at lime kiln from may 20 (doy 140) through august 10 (doy 222/3 in leap years)
plot(wdays.old$doy,wdays.old$meanocc, type="l",lty=2, lwd=2,col="darkblue", xlim=c(140,223), ylim=c(0,1.15),  ylab="Probability of Occurrence",xlab="Day of Year", bty="l", cex.axis = 1.5, cex.lab=1.5)
polygon(c(rev(wdays.old$doy),wdays.old$doy),c(rev(wdays.old$meanocc+wdays.old$sdocc),wdays.old$meanocc-wdays.old$sdocc),col=alpha("darkblue",0.05),lty=0)
polygon(c(rev(wdays.rec$doy),wdays.rec$doy),c(rev(wdays.rec$meanocc+wdays.rec$sdocc),wdays.rec$meanocc-wdays.rec$sdocc),col=alpha("darkblue",0.05),lty=0)

lines(wdays.rec$doy,wdays.rec$mean, lwd=2,col="darkblue")
mtext("A)", side=3, line=1, adj=0, cex=1.5)
#axis(side = 4)

#mtext(paste(podname,"presence",sep=" "),side=4, adj=.5, line=2)
#legend(135,1.2,legend=c(paste(brkyr,"2017",sep="-"),paste("1994",(brkyr-1),sep="-")),lty=c(1,2),lwd=2,col="darkblue", bty="n", cex=1.3)
legend(135,1.15,legend=c(paste("1994",(brkyr-1),sep="-"),paste(brkyr,"2017",sep="-")),lty=c(2,1),lwd=2,col="darkblue", bty="n", cex=1.3)

#Plot mean peak day across early vs late time periods
gests<-get.gests(limegests,"prob.occ")
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
arrows(pkocdoy.lci,1.05,pkocdoy.uci,1.05,code = 0, col = cols[2], lty=1,lwd = 3)
points(pkocdoy,1.05, pch = 21, bg=cols[2], cex = 2)


shift<--14
plot(cpue.old$doy[7:dim(cpue.old)[1]]+shift,running_mean(cpue.old$cpue.mean,7),xlim=c(140,268),ylim=c(0,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)),type="l", bty="u",col="salmon", lwd=2, lty=2,ylab="Chinook Abundance Index", xlab="Day of Year", bty= "l", cex.axis = 1.5, cex.lab=1.5)
polygon(c(rev(cpue.old$doy[7:dim(cpue.old)[1]]+shift),cpue.old$doy[7:dim(cpue.old)[1]]+shift),c(rev(running_mean(cpue.old$cpue.mean,7)+cpue.old$cpue.sd[7:dim(cpue.old)[1]]),running_mean(cpue.old$cpue.mean,7)-cpue.old$cpue.sd[7:dim(cpue.old)[1]]),col=alpha("salmon",0.05),lty=0)
polygon(c(rev(cpue.rec$doy[7:dim(cpue.rec)[1]]+shift),cpue.rec$doy[7:dim(cpue.rec)[1]]+shift),c(rev(running_mean(cpue.rec$cpue.mean,7)+cpue.rec$cpue.sd[7:dim(cpue.rec)[1]]),running_mean(cpue.rec$cpue.mean,7)-cpue.rec$cpue.sd[7:dim(cpue.rec)[1]]),col=alpha("salmon",0.05),lty=0)

lines(cpue.rec$doy[7:dim(cpue.rec)[1]]+shift,running_mean(cpue.rec$cpue.mean,7), lwd=2,col="salmon")
#par(new = TRUE)

#Plot mean peak day across early vs late time periods
gests<-get.gests.chin(chinab,"cpue.est")
lci=.125
uci=0.875

#old time period
pkdoy<-mean(gests$peakcpue.doy[gests$year<brkyr & gests$year>1993])+shift
#pkdoy<-which(cpue.old$cpue.mean==max(cpue.old$cpue.mean))#this changes things...talk to JAmeal about whether we should show the mean day of year of peak abundance or the day of year of mean peak abundance....
pkdoy.lci<-quantile(gests$peakcpue.doy[gests$year<brkyr & gests$year>1993],lci)+shift
pkdoy.uci<-quantile(gests$peakcpue.doy[gests$year<brkyr & gests$year>1993],uci)+shift
arrows(pkdoy.lci,2.3,pkdoy.uci,2.3,code = 0, col = "salmon", lty=2,lwd = 3)
points(pkdoy,2.3, pch = 21, bg="salmon", cex = 2)

#recent time period
pkdoy<-mean(gests$peakcpue.doy[gests$year>=brkyr])+shift
pkdoy.lci<-quantile(gests$peakcpue.doy[gests$year>=brkyr],lci)+shift
pkdoy.uci<-quantile(gests$peakcpue.doy[gests$year>=brkyr],uci)+shift
arrows(pkdoy.lci,2.2,pkdoy.uci,2.2,code = 0, col = "salmon", lty=1,lwd = 3)
points(pkdoy,2.2, pch = 21, bg="salmon", cex = 2)



mtext("B)", side=3, line=1, adj=0, cex=1.5)

if(pod =="SR" & brkyr==2006) {
#restrict SRKW and chin data to consistent years
albchinest90<-albchinphenest[albchinphenest$year>1993,]
albchinest90<-albchinest90[albchinest90$year<2018,]
#albchinest90<-albchinest90[albchinest90$year!=1991,]
#albchinest90<-albchinest90[albchinest90$year!=1992,]
albchinest90<-albchinest90[albchinest90$year!=2014,]#missing from SRKW data
myPalette <- colorRampPalette(brewer.pal(length(unique(albchinest90$year)), "Blues")) #### Gives us a heat map look
cols = rev(myPalette(length(unique(albchinest90$year))))
#png(file="analyses/orcaphen/figures/lime_albchin_gam.png",height=1500,width=4500, res = 300)
#quartz()
#par(mfrow=c(1,3), mar=c(5, 5, 4, 2) + 0.1)
limegestsyr<-get.gests(limegests,"prob.occ")

limegestsyr<-limegestsyr[limegestsyr$years>1993,]

plot(albchinest90$peakcpue.doy,limegestsyr$peakoc.doy,type="p",pch=21, cex.axis=1.8,cex.lab=1.8,bg = cols[factor(albchinest90$year)],xlab="Day of Peak Chinook Abundance Index (DOY)",ylab="Day of Peak SRKW Occupancy Probability (DOY)", cex=1.8, bty="l")
mod<-lm(limegestsyr$peakoc.doy~albchinest90$peakcpue.doy )
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
if(summary(mod)$coef[2,4]<.10 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3,  lwd=2)}
mtext("C)", side=3, line=1, adj=0, cex=1.5)

}
dev.off()
}

#plot srkw vs chinook

png(file="analyses/orcaphen/figures/lime_albchin_brms.png",height=1500,width=1500, res = 300)
  #quartz()
  #par(mfrow=c(1,3), mar=c(5, 5, 4, 2) + 0.1)
albchinest90<-albchinphenest[albchinphenest$year>1993,]
albchinest90<-albchinest90[albchinest90$year<2018,]
#albchinest90<-albchinest90[albchinest90$year!=1991,]
#albchinest90<-albchinest90[albchinest90$year!=1992,]
albchinest90<-albchinest90[albchinest90$year!=2014,]#missing from SRKW data
myPalette <- colorRampPalette(brewer.pal(length(unique(albchinest90$year)), "Blues")) #### Gives us a heat map look
cols = rev(myPalette(length(unique(albchinest90$year))))

limegestsyr<-get.gests(limegests,"prob.occ")
  
  limegestsyr<-limegestsyr[limegestsyr$years>1993,]
  
  plot(albchinest90$peakcpue.doy,limegestsyr$peakoc.doy,type="p",pch=21, cex.axis=1.2,cex.lab=1.2,bg = cols[factor(albchinest90$year)],xlab="Day of Peak Chinook Abundance Index (DOY)",ylab="Day of Peak SRKW Occupancy Probability (DOY)", cex=1.8, bty="l")
  mod<-lm(limegestsyr$peakoc.doy~albchinest90$peakcpue.doy )
  if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, lwd=2)}
  if(summary(mod)$coef[2,4]<.10 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3,  lwd=2)}
  dev.off()


#Same figure but with all breakpoints in one figures
bryears<-c(2005,2006,2007)
lets<-c("A)","C)","E)")
lets2<-c("B)","D)","F)")
pdf(paste("analyses/orcaphen/figures/orcachinphenoverlapbrms",pod,"allbrkyears.pdf",sep=""),height=10, width=12)
#png(paste("analyses/orcaphen/figures/orcachinphenoverlapbrms",pod,brkyr,".png",sep=""),height=960, width=960)

# to figure out how much to shift the salmon curve earlier- because they are measured at ft. langely on the frasier river, but we are interested in when they are at lime kiln
# lime  kiln is ~160 km from lime kiln "as the fish swims" and fish swim about 70 km per day! only need to shift by 2-3 days?
par(oma=c(1,1.5,1,3), mar=c(4,4.5,4,6),mfrow=c(3,2))


for(i in 1:length(bryears)){
  brkyr<-bryears[i]
  orcasum.days.lime1<-limegests[limegests$year>1993 & limegests$year<brkyr,]
  orcasum.days.lime2<-limegests[limegests$year>=brkyr & limegests$year<2018,]
  
  wdays.old<-cbind(aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),mean),aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),sd)$x,aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),length)$x)
  wdays.rec<-cbind(aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),mean),aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),sd)$x,aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),length)$x)
  colnames(wdays.old)<-colnames(wdays.rec)<-c("doy","meanocc","sdocc","n")
  wdays.old$seocc<-wdays.old$sdocc/sqrt(wdays.old$n)
  #chinab<-read.csv("analyses/output/albiongamests.csv", header = TRUE)
  chinablog<-read.csv("analyses/output/albionbrmsests.csv", header = TRUE)
  #restrict days
  #chinab<-chinab[chinab$doy<268,]
  #chinab<-chinab[chinab$doy>90,]
  chinab<-chinablog[chinablog$doy<268,]
  chinab<-chinablog[chinablog$doy>90,]
  
  chinab.old<-chinab[chinab$year>1993 & chinab$year<brkyr,] 
  chinab.rec<-chinab[chinab$year>=brkyr & chinab$year<2018,] 
  cpue.old<-cbind(aggregate(chinab.old$cpue,by=list(chinab.old$doy),mean), aggregate(chinab.old$cpue,by=list(chinab.old$doy),sd)$x)
  cpue.rec<-cbind(aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),mean),aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),sd)$x)
  colnames(cpue.old)<-colnames(cpue.rec)<-c("doy","cpue.mean","cpue.sd")
  
  plot(wdays.old$doy,wdays.old$meanocc, type="l",lty=2, lwd=2,col="darkblue", xlim=c(140,223), ylim=c(0,1.15),  ylab="Probability of Occurrence",xlab="Day of Year", bty="l", cex.axis = 1.5, cex.lab=1.5)
  polygon(c(rev(wdays.old$doy),wdays.old$doy),c(rev(wdays.old$meanocc+wdays.old$sdocc),wdays.old$meanocc-wdays.old$sdocc),col=alpha("darkblue",0.05),lty=0)
  polygon(c(rev(wdays.rec$doy),wdays.rec$doy),c(rev(wdays.rec$meanocc+wdays.rec$sdocc),wdays.rec$meanocc-wdays.rec$sdocc),col=alpha("darkblue",0.05),lty=0)
  
  lines(wdays.rec$doy,wdays.rec$mean, lwd=2,col="darkblue")
  mtext(paste(lets[i]), side=3, line=1, adj=0, cex=1.5)
  #axis(side = 4)
  
  legend(135,1.2,legend=c(paste("1994",(brkyr-1),sep="-"),paste(brkyr,"2017",sep="-")),lty=c(2,1),lwd=2,col="darkblue", bty="l", cex=1.3)
gests<-get.gests(limegests,"prob.occ")
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
arrows(pkocdoy.lci,1.0,pkocdoy.uci,1.0,code = 0, col = cols[2], lty=1,lwd = 3)
points(pkocdoy,1.0, pch = 21, bg=cols[2], cex = 2)

shift= -14
  plot(cpue.old$doy[7:dim(cpue.old)[1]]+shift,running_mean(cpue.old$cpue.mean,7),xlim=c(140,268),ylim=c(0,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)+.2),type="l", bty="u",col="salmon", lwd=2, lty=2,ylab="Chinook Abundance Index", xlab="Day of Year", bty= "l", cex.axis = 1.5, cex.lab=1.5)
  polygon(c(rev(cpue.old$doy[7:dim(cpue.old)[1]]+shift),cpue.old$doy[7:dim(cpue.old)[1]]+shift),c(rev(running_mean(cpue.old$cpue.mean,7)+cpue.old$cpue.sd[7:dim(cpue.old)[1]]),running_mean(cpue.old$cpue.mean,7)-cpue.old$cpue.sd[7:dim(cpue.old)[1]]),col=alpha("salmon",0.1),lty=0)
  polygon(c(rev(cpue.rec$doy[7:dim(cpue.rec)[1]]+shift),cpue.rec$doy[7:dim(cpue.rec)[1]]+shift),c(rev(running_mean(cpue.rec$cpue.mean,7)+cpue.rec$cpue.sd[7:dim(cpue.rec)[1]]),running_mean(cpue.rec$cpue.mean,7)-cpue.rec$cpue.sd[7:dim(cpue.rec)[1]]),col=alpha("salmon",0.1),lty=0)
  
  lines(cpue.rec$doy[7:dim(cpue.rec)[1]]+shift,running_mean(cpue.rec$cpue.mean,7), lwd=2,col="salmon")
  #par(new = TRUE)
  mtext(paste(lets2[i]), side=3, line=1, adj=0, cex=1.5)
  #Plot mean peak day across early vs late time periods
  #gests<-get.gests.chin(albgam,"cpue.est")
  gests<-get.gests.chin(chinab,"cpue.est")#brms mod estimates
  
  lci=.125
  uci=0.875
  
  #old time period
  gests$peakcpue.doy<-as.numeric(gests$peakcpue.doy)
  pkdoy<-mean(gests$peakcpue.doy[gests$year<brkyr & gests$year>1993])+shift
  pkdoy.lci<-quantile(gests$peakcpue.doy[gests$year<brkyr & gests$year>1993],lci)+shift
  pkdoy.uci<-quantile(gests$peakcpue.doy[gests$year<brkyr & gests$year>1993],uci)+shift
  arrows(pkdoy.lci,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)+.2,pkdoy.uci,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)+.2,code = 0, col = "salmon", lty=2,lwd = 3)
  points(pkdoy,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)+.2, pch = 21, bg="salmon", cex = 2)
  
  #recent time period
  pkdoy<-mean(gests$peakcpue.doy[gests$year>=brkyr])+shift
  pkdoy.lci<-quantile(gests$peakcpue.doy[gests$year>=brkyr],lci)+shift
  pkdoy.uci<-quantile(gests$peakcpue.doy[gests$year>=brkyr],uci)+shift
  arrows(pkdoy.lci,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE),pkdoy.uci,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE),code = 0, col = "salmon", lty=1,lwd = 3)
  points(pkdoy,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE), pch = 21, bg="salmon", cex = 2)
  
}

dev.off()

# if(pod== "SR"){load("analyses/output/sr.brmslog.Rda")}
# if(pod== "J"){load("analyses/output/j.brms.Rda")}
# if(pod== "K"){load("analyses/output/k.brms.Rda")}
# if(pod== "L"){load("analyses/output/l.brms.Rda")}
#load("analyses/output/albionchibrmslog.Rda")

podnames<-c("J","K","L")
pdf(paste("analyses/orcaphen/figures/orcachinphenoverlap_allpods",brkyr,".pdf",sep=""),height=12, width=5)
#png(paste("analyses/orcaphen/figures/orcachinphenoverlap",pod,brkyr,".png",sep=""),height=960, width=960)

# to figure out how much to shift the salmon curve earlier- because they are measured at ft. langely on the frasier river, but we are interested in when they are at lime kiln
# lime  kiln is ~160 km from lime kiln "as the fish swims" and fish swim about 70 km per day! only need to shift by 2-3 days?
par(oma=c(1,1.5,1,2), mar=c(4,4.5,4,6),mfrow=c(3,1))

for(i in 1:length(podnames)){
  brkyr<-2006
  pod = podnames[i]
    if(pod== "J"){limegests$prob.occ<-limegests$Jprob.Estimate
  podname = "J pod"
  let<-"A)"
  }
  if(pod== "K"){limegests$prob.occ<-limegests$Kprob.Estimate
  podname = "K pod"
  let<-"B)"
  
  }
  if(pod== "L"){limegests$prob.occ<-limegests$Lprob.Estimate
  podname = "L pod"
  let<-"C)"
  
  }
  orcasum.days.lime1<-limegests[limegests$year>1993 & limegests$year<brkyr,]
  orcasum.days.lime2<-limegests[limegests$year>=brkyr & limegests$year<2018,]
  
  wdays.old<-cbind(aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),mean),aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),sd)$x,aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),length)$x)
  wdays.rec<-cbind(aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),mean),aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),sd)$x,aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),length)$x)
  colnames(wdays.old)<-colnames(wdays.rec)<-c("doy","meanocc","sdocc","n")
  wdays.old$seocc<-wdays.old$sdocc/sqrt(wdays.old$n)
  
  
  plot(wdays.old$doy,wdays.old$meanocc, type="l",lty=2, lwd=2,col="darkblue", xlim=c(140,200), ylim=c(0,1.15),  ylab="Probability of Occurrence",xlab="Day of Year", bty="l", cex.axis = 1.5, cex.lab=1.5)
  polygon(c(rev(wdays.old$doy),wdays.old$doy),c(rev(wdays.old$meanocc+wdays.old$sdocc),wdays.old$meanocc-wdays.old$sdocc),col=alpha("darkblue",0.05),lty=0)
  polygon(c(rev(wdays.rec$doy),wdays.rec$doy),c(rev(wdays.rec$meanocc+wdays.rec$sdocc),wdays.rec$meanocc-wdays.rec$sdocc),col=alpha("darkblue",0.05),lty=0)
  
  lines(wdays.rec$doy,wdays.rec$mean, lwd=2,col="darkblue")
  mtext(paste(let,podname, sep = " "), side=3, line=1, adj=0, cex=1.5)
  #axis(side = 4)
  
  #mtext(paste(podname,"presence",sep=" "),side=4, adj=.5, line=2)
  legend("topleft",legend=c(paste("1994",(brkyr-1),sep="-"),paste(brkyr,"2017",sep="-")),lty=c(2,1),lwd=2,col="darkblue", cex= 1.5)
  gests<-get.gests(limegests,"prob.occ")
  lci=.125
  uci=0.875
  
  #old time period
  pkocdoy<-mean(gests$peakoc.doy[gests$year<brkyr & gests$year>1993])
  pkocdoy.lci<-quantile(gests$peakoc.doy[gests$year<brkyr & gests$year>1993],lci)
  pkocdoy.uci<-quantile(gests$peakoc.doy[gests$year<brkyr & gests$year>1993],uci)
  arrows(pkocdoy.lci,1.05,pkocdoy.uci,1.05,code = 0, col = "darkblue", lty=2,lwd = 3)
  points(pkocdoy,1.05, pch = 21, bg="darkblue", cex = 2)
  
  #recent time period
  pkocdoy<-mean(gests$peakoc.doy[gests$year>=brkyr])
  pkocdoy.lci<-quantile(gests$peakoc.doy[gests$year>=brkyr],lci)
  pkocdoy.uci<-quantile(gests$peakoc.doy[gests$year>=brkyr],uci)
  arrows(pkocdoy.lci,.95,pkocdoy.uci,.95,code = 0, col = cols[2], lty=1,lwd = 3)
  points(pkocdoy,.95, pch = 21, bg=cols[2], cex = 2)
  
  
  }
dev.off()

#Plot of SRKW vs chin

#Now just make plots of chinook
brkyr<-2006
chinab<-read.csv("analyses/output/albionbrmsests.csv", header = TRUE)
#restrict days
#chinab<-chinab[chinab$doy<268,]
chinab<-chinab[chinab$doy>90,]

chinab.old<-chinab[chinab$year>1993 & chinab$year<brkyr,] 
chinab.rec<-chinab[chinab$year>=brkyr & chinab$year<2018,] 
cpue.old<-cbind(aggregate(chinab.old$cpue,by=list(chinab.old$doy),mean), aggregate(chinab.old$cpue,by=list(chinab.old$doy),sd)$x)
cpue.rec<-cbind(aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),mean),aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),sd)$x)
colnames(cpue.old)<-colnames(cpue.rec)<-c("doy","cpue.mean","cpue.sd")

pdf("analyses/orcaphen/figures/chinphenbrmsfig.pdf",height=6, width=12)
#png("analyses/orcaphen/figures/chinphenfig.png",height=6, width=12)

# to figure out how much to shift the salmon curve earlier- because they are measured at ft. langely on the frasier river, but we are interested in when they are at lime kiln
# lime  kiln is ~160 km from lime kiln "as the fish swims" and fish swim about 70 km per day! only need to shift by 2-3 days?
par(mfrow=c(1,3),mar=c(7, 5, 4, 2) + 0.1)
shift<--14
plot(cpue.old$doy[7:length(cpue.old$doy)]+shift,running_mean(cpue.old$cpue.mean,7),xlim=c(120,250),ylim=c(0,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)+.2),type="l", bty="u",col="salmon", lwd=2, lty=2,
     ylab="Chinook Abundance (mean cpue)", xlab="Day of Year", cex.axis=1.5,cex.lab=1.6)
polygon(c(rev(cpue.old$doy[7:length(cpue.old$doy)]+shift),cpue.old$doy[7:length(cpue.old$doy)]+shift),c(rev(running_mean(cpue.old$cpue.mean,7)+cpue.old$cpue.sd[7:length(cpue.old$doy)]),running_mean(cpue.old$cpue.mean,7)-cpue.old$cpue.sd[7:length(cpue.old$doy)]),col=alpha("salmon",0.1),lty=0)
polygon(c(rev(cpue.rec$doy[7:length(cpue.rec$doy)]+shift),cpue.rec$doy[7:length(cpue.rec$doy)]+shift),c(rev(running_mean(cpue.rec$cpue.mean,7)+cpue.rec$cpue.sd[7:length(cpue.rec$doy)]),running_mean(cpue.rec$cpue.mean,7)-cpue.rec$cpue.sd[7:length(cpue.rec$doy)]),col=alpha("salmon",0.1),lty=0)

lines(cpue.rec$doy[7:length(cpue.rec$doy)]+shift,running_mean(cpue.rec$cpue.mean,7), lwd=2,col="salmon")
mtext("A)", side=3, line=2, adj=-.2, cex=1.5)

alph=0.75

legend(120,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)+.2,legend=c(paste("1994",(brkyr-1),sep="-"),paste(brkyr,"2017",sep="-")),lty=c(2,1),lwd=2,col="salmon", cex=1.3)

#Plot mean peak day across early vs late time periods
gests<-get.gests.chin(chinab,"cpue.est")
lci=.125
uci=0.875

#old time period
pkdoy<-mean(gests$peakcpue.doy[gests$year<brkyr & gests$year>1993])+shift
#pkdoy<-which(cpue.old$cpue.mean==max(cpue.old$cpue.mean))#this changes things...talk to JAmeal about whether we should show the mean day of year of peak abundance or the day of year of mean peak abundance....
pkdoy.lci<-quantile(gests$peakcpue.doy[gests$year<brkyr & gests$year>1993],lci)+shift
pkdoy.uci<-quantile(gests$peakcpue.doy[gests$year<brkyr & gests$year>1993],uci)+shift
arrows(pkdoy.lci,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)+.1,pkdoy.uci,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)+.1,code = 0, col = "salmon", lty=2,lwd = 3)
points(pkdoy,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)+.1, pch = 21, bg="salmon", cex = 2)

#recent time period
pkdoy<-mean(gests$peakcpue.doy[gests$year>=brkyr])+shift
pkdoy.lci<-quantile(gests$peakcpue.doy[gests$year>=brkyr],lci)+shift
pkdoy.uci<-quantile(gests$peakcpue.doy[gests$year>=brkyr],uci)+shift
arrows(pkdoy.lci,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)-.05,pkdoy.uci,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)-.05,code = 0, col = "salmon", lty=1,lwd = 3)
points(pkdoy,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)-.05, pch = 21, bg="salmon", cex = 2)

gests<-gests[gests$years>1993,]
#Peak cpue doy over time
plot(gests$years,gests$peakcpue.doy, xlab= "Year", ylab= "Day of Peak Abundance Index", pch=21, bty="l", cex.axis=1.5,cex.lab=1.6,cex=1.8,bg="salmon")
peakmod<-summary(lm(gests$peakcpue.doy~gests$years))
if(peakmod$coef[2,4]<(1-alph)){abline(peakmod$coef[,1], lty=1, lwd=2)}
print(paste("r2=",round(peakmod$r.squared, digits=2),",p=",round(peakmod$coeff[2,4], digits=3)), side=3, adj=1, cex=0.7)
print(paste("coef=",round(peakmod$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
mtext("B)", side=3, line=2, adj=-.2, cex=1.5)

#CPUE over time
plot(gests$year,gests$totalcpues,type = "l", col="salmon",cex.axis=1.5,cex.lab=1.6,
     xlab="Year",ylab="Annual Abundance Index", cex=1.8, bty="l", lwd=2)
#polygon(c(rev(as.numeric(gests$year)),as.numeric(yearsum.uc$year)), c(rev(yearsum.uc$wdays), yearsum.lc$wdays), col = alpha("salmon", 0.2), border = NA)


#if(summary(mod)$coef[2,4]<.15 & summary(mod)$coef[2,4]>.05){abline(mod, lty=3, lwd=2)}
mtext("C)", side=3, line=2, adj=-.2, cex=1.5)
dev.off() 

