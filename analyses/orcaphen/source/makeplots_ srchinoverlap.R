if(pod== "SR"){load("analyses/output/sr.brms.Rda")}
if(pod== "J"){load("analyses/output/j.brms.Rda")}
if(pod== "K"){load("analyses/output/k.brms.Rda")}
if(pod== "L"){load("analyses/output/l.brms.Rda")}


limegests<-read.csv("analyses/output/lime_prob.occ.50.csv", header=TRUE)
limegests$year<-as.numeric(limewdaysabs$year)
if(pod== "SR"){limegests$prob.occ<-limegests$SRprob.Estimate}
if(pod== "J"){limegests$prob.occ<-limegestsJprob.Estimate}
if(pod== "K"){limegests$prob.occ<-limegests$Kprob.Estimate}
if(pod== "L"){limegests$prob.occ<-limegests$Lprob.Estimate}

orcasum.days.lime1<-limegests[limegests$year>1993 & limegests$year<brkyr,]
orcasum.days.lime2<-limegests[limegests$year>=brkyr & limegests$year<2018,]

wdays.old<-cbind(aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),mean),aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),sd)$x,aggregate(orcasum.days.lime1$prob.occ,by=list(orcasum.days.lime1$doy),length)$x)
wdays.rec<-cbind(aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),mean),aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),sd)$x,aggregate(orcasum.days.lime2$prob.occ,by=list(orcasum.days.lime2$doy),length)$x)

#chinabold<-read.csv("analyses/output/albiongamests.csv", header = TRUE)
chinab<-read.csv("analyses/output/albionchiphenbrms.csv", header = TRUE)

chinab.old<-chinab[chinab$year>1993 & chinab$year<brkyr,] 
chinab.rec<-chinab[chinab$year>=brkyr & chinab$year<2018,] 
cpue.old<-cbind(aggregate(chinab.old$cpue,by=list(chinab.old$doy),mean), aggregate(chinab.old$cpue,by=list(chinab.old$doy),sd)$x)
cpue.rec<-cbind(aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),mean),aggregate(chinab.rec$cpue,by=list(chinab.rec$doy),sd)$x)
colnames(cpue.old)<-colnames(cpue.rec)<-c("doy","cpue.mean","cpue.sd")


pdf("analyses/orcaphen/figures/orcachinphenoverlap.pdf",height=6, width=12)
#png("analyses/orcaphen/figures/orcachinphenoverlap.png",height=6, width=12)

# to figure out how much to shift the salmon curve earlier- because they are measured at ft. langely on the frasier river, but we are interested in when they are at lime kiln
# lime  kiln is ~160 km from lime kiln "as the fish swims" and fish swim about 70 km per day! only need to shift by 2-3 days?
par(oma=c(1,1,1,3), mar=c(4,4,4,6))
shift<--14
plot(cpue.old$doy[7:220]+shift,cpue.old$cpue.mean,xlim=c(120,214),ylim=c(0,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)),type="l", bty="u",col="salmon", lwd=2, lty=2,ylab="Chinook Abundance (mean cpue)", xlab="Day of Year")
polygon(c(rev(cpue.old$doy[7:220]+shift),cpue.old$doy[7:220]+shift),c(rev(running_mean(cpue.old$cpue.mean,7)+cpue.old$cpue.sd[7:220]),running_mean(cpue.old$cpue.mean,7)-cpue.old$cpue.sd[7:220]),col=alpha("salmon",0.1),lty=0)
polygon(c(rev(cpue.rec$doy[7:205]+shift),cpue.rec$doy[7:205]+shift),c(rev(running_mean(cpue.rec$cpue.mean,7)+cpue.rec$cpue.sd[7:205]),running_mean(cpue.rec$cpue.mean,7)-cpue.rec$cpue.sd[7:205]),col=alpha("salmon",0.1),lty=0)

#lines(cpue.rec$doy[7:205]+shift,running_mean(cpue.rec$cpue.mean,7), lwd=2,col="salmon")
par(new = TRUE)

plot(wdays.old$doy[7:129],running_mean(wdays.old$meanocc,7), type="l",lty=2, lwd=2,col="black", xlim=c(120,250), ylim=c(0,1), yaxt="n", ylab="",xaxt="n", xlab="", bty="l")
polygon(c(rev(wdays.old$doy[7:129]),wdays.old$doy[7:129]),c(rev(running_mean(wdays.old$meanocc,7)+wdays.old$sdocc[7:129]),running_mean(wdays.old$meanocc,7)-wdays.old$sdocc[7:129]),col=alpha("black",0.05),lty=0)
polygon(c(rev(wdays.rec$doy[7:129]),wdays.rec$doy[7:129]),c(rev(running_mean(wdays.rec$meanocc,7)+wdays.rec$sdocc[7:129]),running_mean(wdays.rec$meanocc,7)-wdays.rec$sdocc[7:129]),col=alpha("black",0.05),lty=0)

lines(wdays.rec$doy[7:129],running_mean(wdays.rec$mean,7), lwd=2,col="black")
axis(side = 4)

mtext("SRKW presence",side=4, adj=.5, line=2)
legend(115,1,legend=c("1994-2005","2006-2017"),lty=c(2,1),lwd=2,col="black", bty="n")
legend(115,.85,legend=c("SRKW","salmon"),lty=1,lwd=2,col=c("black","salmon"), bty="n")

dev.off()

pdf("analyses/orcaphen/figures/orcachinphenoverlap.pdf",height=6, width=12)
#png("analyses/orcaphen/figures/orcachinphenoverlap.png",height=6, width=12)

# to figure out how much to shift the salmon curve earlier- because they are measured at ft. langely on the frasier river, but we are interested in when they are at lime kiln
# lime  kiln is ~160 km from lime kiln "as the fish swims" and fish swim about 70 km per day! only need to shift by 2-3 days?
par(oma=c(1,1,1,3), mar=c(4,4,4,6))
shift<--14
plot(cpue.old$doy[7:220]+shift,running_mean(cpue.old$cpue.mean,7),xlim=c(120,250),ylim=c(0,max(cpue.old$cpue.mean)+max(cpue.old$cpue.sd,na.rm=TRUE)),type="l", bty="u",col="salmon", lwd=2, lty=2,ylab="Chinook Abundance (mean cpue)", xlab="Day of Year")
polygon(c(rev(cpue.old$doy[7:220]+shift),cpue.old$doy[7:220]+shift),c(rev(running_mean(cpue.old$cpue.mean,7)+cpue.old$cpue.sd[7:220]),running_mean(cpue.old$cpue.mean,7)-cpue.old$cpue.sd[7:220]),col=alpha("salmon",0.1),lty=0)
polygon(c(rev(cpue.rec$doy[7:205]+shift),cpue.rec$doy[7:205]+shift),c(rev(running_mean(cpue.rec$cpue.mean,7)+cpue.rec$cpue.sd[7:205]),running_mean(cpue.rec$cpue.mean,7)-cpue.rec$cpue.sd[7:205]),col=alpha("salmon",0.1),lty=0)

lines(cpue.rec$doy[7:205]+shift,running_mean(cpue.rec$cpue.mean,7), lwd=2,col="salmon")
par(new = TRUE)

plot(wdays.old$doy[7:129],running_mean(wdays.old$meanocc,7), type="l",lty=2, lwd=2,col="black", xlim=c(120,250), ylim=c(0,1), yaxt="n", ylab="",xaxt="n", xlab="", bty="l")
polygon(c(rev(wdays.old$doy[7:129]),wdays.old$doy[7:129]),c(rev(running_mean(wdays.old$meanocc,7)+wdays.old$sdocc[7:129]),running_mean(wdays.old$meanocc,7)-wdays.old$sdocc[7:129]),col=alpha("black",0.05),lty=0)
polygon(c(rev(wdays.rec$doy[7:129]),wdays.rec$doy[7:129]),c(rev(running_mean(wdays.rec$meanocc,7)+wdays.rec$sdocc[7:129]),running_mean(wdays.rec$meanocc,7)-wdays.rec$sdocc[7:129]),col=alpha("black",0.05),lty=0)

lines(wdays.rec$doy[7:129],running_mean(wdays.rec$mean,7), lwd=2,col="black")
axis(side = 4)

mtext("SRKW presence",side=4, adj=.5, line=2)
legend(115,1,legend=c("1994-2005","2006-2017"),lty=c(2,1),lwd=2,col="black", bty="n")
legend(115,.85,legend=c("SRKW","salmon"),lty=1,lwd=2,col=c("black","salmon"), bty="n")

dev.off()
