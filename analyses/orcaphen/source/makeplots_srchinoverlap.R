# if(pod== "SR"){load("analyses/output/sr.brmslog.Rda")}
# if(pod== "J"){load("analyses/output/j.brms.Rda")}
# if(pod== "K"){load("analyses/output/k.brms.Rda")}
# if(pod== "L"){load("analyses/output/l.brms.Rda")}
#load("analyses/output/albionchibrmslog.Rda")

pod = "SR"
limegests<-read.csv("analyses/output/lime_prob.occ.75.csv", header=TRUE)
dim(limegests)
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
chinab<-read.csv("analyses/output/albiongamests.csv", header = TRUE)
#chinab<-read.csv("analyses/output/albionchiphenbrmslog.csv", header = TRUE)
#restrict days
chinab<-chinab[chinab$doy<268,]
chinab<-chinab[chinab$doy>90,]

chinab.old<-chinab[chinab$year>1993 & chinab$year<brkyr,] 
chinab.rec<-chinab[chinab$year>=brkyr & chinab$year<2018,] 
cpue.old<-cbind(aggregate(chinab.old$cpue,by=list(chinab.old$doy),mean), aggregate(chinab.old$cpue,by=list(chinab.old$doy),sd)$x)
cpue.rec<-cbind(aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),mean),aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),sd)$x)
colnames(cpue.old)<-colnames(cpue.rec)<-c("doy","cpue.mean","cpue.sd")


pdf(paste("analyses/orcaphen/figures/orcachinphenoverlap",pod,brkyr,".pdf",sep=""),height=5, width=12)
png(paste("analyses/orcaphen/figures/orcachinphenoverlap",pod,brkyr,".png",sep=""),height=400, width=1200)

# to figure out how much to shift the salmon curve earlier- because they are measured at ft. langely on the frasier river, but we are interested in when they are at lime kiln
# lime  kiln is ~160 km from lime kiln "as the fish swims" and fish swim about 70 km per day! only need to shift by 2-3 days?
#windows(height=6, width=12)
par(oma=c(1,1.5,1,3), mar=c(4,4.5,4,6),mfrow=c(1,2))

plot(wdays.old$doy,wdays.old$meanocc, type="l",lty=2, lwd=2,col="darkblue", xlim=c(140,200), ylim=c(0,1.1),  ylab="Probability of Occurrence",xlab="Day of Year", bty="l", cex.axis = 1.5, cex.lab=1.5)
polygon(c(rev(wdays.old$doy),wdays.old$doy),c(rev(wdays.old$meanocc+wdays.old$sdocc),wdays.old$meanocc-wdays.old$sdocc),col=alpha("darkblue",0.05),lty=0)
polygon(c(rev(wdays.rec$doy),wdays.rec$doy),c(rev(wdays.rec$meanocc+wdays.rec$sdocc),wdays.rec$meanocc-wdays.rec$sdocc),col=alpha("darkblue",0.05),lty=0)

lines(wdays.rec$doy,wdays.rec$mean, lwd=2,col="darkblue")
mtext("A)", side=3, line=1, adj=0, cex=1.5)
#axis(side = 4)

#mtext(paste(podname,"presence",sep=" "),side=4, adj=.5, line=2)
#legend(135,1.2,legend=c(paste(brkyr,"2017",sep="-"),paste("1994",(brkyr-1),sep="-")),lty=c(1,2),lwd=2,col="darkblue", bty="n", cex=1.3)
legend("bottomright",legend=c(paste("1994",(brkyr-1),sep="-"),paste(brkyr,"2017",sep="-")),lty=c(2,1),lwd=2,col="darkblue", bty="n", cex=1.3)

#Plot mean peak day across early vs late time periods
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
arrows(pkocdoy.lci,1.1,pkocdoy.uci,1.1,code = 0, col = cols[2], lty=1,lwd = 3)
points(pkocdoy,1.1, pch = 21, bg=cols[2], cex = 2)


shift<--14
plot(cpue.old$doy[7:dim(cpue.old)[1]]+shift,running_mean(cpue.old$cpue.mean,7),xlim=c(140,240),ylim=c(0,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)),type="l", bty="u",col="salmon", lwd=2, lty=2,ylab="Chinook Abundance Index", xlab="Day of Year", bty= "l", cex.axis = 1.5, cex.lab=1.5)
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
arrows(pkdoy.lci,3.9,pkdoy.uci,3.9,code = 0, col = "salmon", lty=2,lwd = 3)
points(pkdoy,3.9, pch = 21, bg="salmon", cex = 2)

#recent time period
pkdoy<-mean(gests$peakcpue.doy[gests$year>=brkyr])+shift
pkdoy.lci<-quantile(gests$peakcpue.doy[gests$year>=brkyr],lci)+shift
pkdoy.uci<-quantile(gests$peakcpue.doy[gests$year>=brkyr],uci)+shift
arrows(pkdoy.lci,4.1,pkdoy.uci,4.1,code = 0, col = "salmon", lty=1,lwd = 3)
points(pkdoy,4.1, pch = 21, bg="salmon", cex = 2)





mtext("B)", side=3, line=1, adj=0, cex=1.5)


dev.off()
}

#Same figure but with all breakpoints in one figures
bryears<-c(2005,2006,2007)
lets<-c("A)","C)","E)")
lets2<-c("B)","D)","F)")
pdf(paste("analyses/orcaphen/figures/orcachinphenoverlap",pod,"allbrkyears.pdf",sep=""),height=10, width=12)
#png(paste("analyses/orcaphen/figures/orcachinphenoverlap",pod,brkyr,".png",sep=""),height=960, width=960)

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
  chinab<-read.csv("analyses/output/albiongamests.csv", header = TRUE)
  #chinab<-read.csv("analyses/output/albionchiphenbrmslog.csv", header = TRUE)
  #restrict days
  chinab<-chinab[chinab$doy<268,]
  chinab<-chinab[chinab$doy>90,]
  
  chinab.old<-chinab[chinab$year>1993 & chinab$year<brkyr,] 
  chinab.rec<-chinab[chinab$year>=brkyr & chinab$year<2018,] 
  cpue.old<-cbind(aggregate(chinab.old$cpue,by=list(chinab.old$doy),mean), aggregate(chinab.old$cpue,by=list(chinab.old$doy),sd)$x)
  cpue.rec<-cbind(aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),mean),aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),sd)$x)
  colnames(cpue.old)<-colnames(cpue.rec)<-c("doy","cpue.mean","cpue.sd")
  
  plot(wdays.old$doy,wdays.old$meanocc, type="l",lty=2, lwd=2,col="darkblue", xlim=c(140,200), ylim=c(0,1.1),  ylab="Probability of Occurrence",xlab="Day of Year", bty="l", cex.axis = 1.5, cex.lab=1.5)
  polygon(c(rev(wdays.old$doy),wdays.old$doy),c(rev(wdays.old$meanocc+wdays.old$sdocc),wdays.old$meanocc-wdays.old$sdocc),col=alpha("darkblue",0.05),lty=0)
  polygon(c(rev(wdays.rec$doy),wdays.rec$doy),c(rev(wdays.rec$meanocc+wdays.rec$sdocc),wdays.rec$meanocc-wdays.rec$sdocc),col=alpha("darkblue",0.05),lty=0)
  
  lines(wdays.rec$doy,wdays.rec$mean, lwd=2,col="darkblue")
  mtext(paste(lets[i]), side=3, line=1, adj=0, cex=1.5)
  #axis(side = 4)
  
  legend("topleft",legend=c(paste("1994",(brkyr-1),sep="-"),paste(brkyr,"2017",sep="-")),lty=c(2,1),lwd=2,col="darkblue", bty="n", cex=1.3)
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
arrows(pkocdoy.lci,1.1,pkocdoy.uci,1.1,code = 0, col = cols[2], lty=1,lwd = 3)
points(pkocdoy,1.1, pch = 21, bg=cols[2], cex = 2)




shift<--14
  plot(cpue.old$doy[7:dim(cpue.old)[1]]+shift,running_mean(cpue.old$cpue.mean,7),xlim=c(140,240),ylim=c(0,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)),type="l", bty="u",col="salmon", lwd=2, lty=2,ylab="Chinook Abundance Index", xlab="Day of Year", bty= "l", cex.axis = 1.5, cex.lab=1.5)
  polygon(c(rev(cpue.old$doy[7:dim(cpue.old)[1]]+shift),cpue.old$doy[7:dim(cpue.old)[1]]+shift),c(rev(running_mean(cpue.old$cpue.mean,7)+cpue.old$cpue.sd[7:dim(cpue.old)[1]]),running_mean(cpue.old$cpue.mean,7)-cpue.old$cpue.sd[7:dim(cpue.old)[1]]),col=alpha("salmon",0.1),lty=0)
  polygon(c(rev(cpue.rec$doy[7:dim(cpue.rec)[1]]+shift),cpue.rec$doy[7:dim(cpue.rec)[1]]+shift),c(rev(running_mean(cpue.rec$cpue.mean,7)+cpue.rec$cpue.sd[7:dim(cpue.rec)[1]]),running_mean(cpue.rec$cpue.mean,7)-cpue.rec$cpue.sd[7:dim(cpue.rec)[1]]),col=alpha("salmon",0.1),lty=0)
  
  lines(cpue.rec$doy[7:dim(cpue.rec)[1]]+shift,running_mean(cpue.rec$cpue.mean,7), lwd=2,col="salmon")
  #par(new = TRUE)
  mtext(paste(lets2[i]), side=3, line=1, adj=0, cex=1.5)
  #Plot mean peak day across early vs late time periods
  gests<-get.gests.chin(chinab,"cpue.est")
  lci=.125
  uci=0.875
  
  #old time period
  pkdoy<-mean(gests$peakcpue.doy[gests$year<brkyr & gests$year>1993])+shift
  pkdoy.lci<-quantile(gests$peakcpue.doy[gests$year<brkyr & gests$year>1993],lci)+shift
  pkdoy.uci<-quantile(gests$peakcpue.doy[gests$year<brkyr & gests$year>1993],uci)+shift
  arrows(pkdoy.lci,3.9,pkdoy.uci,3.9,code = 0, col = "salmon", lty=2,lwd = 3)
  points(pkdoy,3.9, pch = 21, bg="salmon", cex = 2)
  
  #recent time period
  pkdoy<-mean(gests$peakcpue.doy[gests$year>=brkyr])+shift
  pkdoy.lci<-quantile(gests$peakcpue.doy[gests$year>=brkyr],lci)+shift
  pkdoy.uci<-quantile(gests$peakcpue.doy[gests$year>=brkyr],uci)+shift
  arrows(pkdoy.lci,4.1,pkdoy.uci,4.1,code = 0, col = "salmon", lty=1,lwd = 3)
  points(pkdoy,4.1, pch = 21, bg="salmon", cex = 2)
  
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
  
  
  plot(wdays.old$doy,wdays.old$meanocc, type="l",lty=2, lwd=2,col="darkblue", xlim=c(140,200), ylim=c(0,1.1),  ylab="Probability of Occurrence",xlab="Day of Year", bty="l", cex.axis = 1.5, cex.lab=1.5)
  polygon(c(rev(wdays.old$doy),wdays.old$doy),c(rev(wdays.old$meanocc+wdays.old$sdocc),wdays.old$meanocc-wdays.old$sdocc),col=alpha("darkblue",0.05),lty=0)
  polygon(c(rev(wdays.rec$doy),wdays.rec$doy),c(rev(wdays.rec$meanocc+wdays.rec$sdocc),wdays.rec$meanocc-wdays.rec$sdocc),col=alpha("darkblue",0.05),lty=0)
  
  lines(wdays.rec$doy,wdays.rec$mean, lwd=2,col="darkblue")
  mtext(paste(let,podname, sep = " "), side=3, line=1, adj=0, cex=1.5)
  #axis(side = 4)
  
  #mtext(paste(podname,"presence",sep=" "),side=4, adj=.5, line=2)
  legend("topleft",legend=c(paste("1994",(brkyr-1),sep="-"),paste(brkyr,"2017",sep="-")),lty=c(2,1),lwd=2,col="darkblue", bty="n", cex= 1.5)
  gests<-get.gests(limegests,"prob.occ")
  lci=.125
  uci=0.875
  
  #old time period
  pkocdoy<-mean(gests$peakoc.doy[gests$year<brkyr & gests$year>1993])
  pkocdoy.lci<-quantile(gests$peakoc.doy[gests$year<brkyr & gests$year>1993],lci)
  pkocdoy.uci<-quantile(gests$peakoc.doy[gests$year<brkyr & gests$year>1993],uci)
  arrows(pkocdoy.lci,.95,pkocdoy.uci,.95,code = 0, col = "darkblue", lty=2,lwd = 3)
  points(pkocdoy,.95, pch = 21, bg="darkblue", cex = 2)
  
  #recent time period
  pkocdoy<-mean(gests$peakoc.doy[gests$year>=brkyr])
  pkocdoy.lci<-quantile(gests$peakoc.doy[gests$year>=brkyr],lci)
  pkocdoy.uci<-quantile(gests$peakoc.doy[gests$year>=brkyr],uci)
  arrows(pkocdoy.lci,1,pkocdoy.uci,1,code = 0, col = cols[2], lty=1,lwd = 3)
  points(pkocdoy,1, pch = 21, bg=cols[2], cex = 2)
  
  
  }
dev.off()
