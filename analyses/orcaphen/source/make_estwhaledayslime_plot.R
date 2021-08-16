#Make Estimated Whale days Fig

#Make figures of model estimated whale days with 75 percent
limedays<-read.csv("analyses/output/lime_prob.occ.75.csv", header=TRUE)
limedays$year<-as.numeric(as.character(limedays$year))
limedays<-limedays[limedays$year>1991,]

yearsum<-aggregate(limedays$SRprob.Estimate,by=list(limedays$year),sum)

jsum<-aggregate(limedays$Jprob.Estimate,by=list(limedays$year),sum)
ksum<-aggregate(limedays$Kprob.Estimate,by=list(limedays$year),sum)
lsum<-aggregate(limedays$Lprob.Estimate,by=list(limedays$year),sum)
yearsum.lc<-aggregate(limedays$SRprob.Q12.5,by=list(limedays$year),sum)
jsum.lc<-aggregate(limedays$Jprob.Q12.5,by=list(limedays$year),sum)
ksum.lc<-aggregate(limedays$Kprob.Q12.5,by=list(limedays$year),sum)
lsum.lc<-aggregate(limedays$Lprob.Q12.5,by=list(limedays$year),sum)
yearsum.uc<-aggregate(limedays$SRprob.Q87.5,by=list(limedays$year),sum)
jsum.uc<-aggregate(limedays$Jprob.Q87.5,by=list(limedays$year),sum)
ksum.uc<-aggregate(limedays$Kprob.Q87.5,by=list(limedays$year),sum)
lsum.uc<-aggregate(limedays$Lprob.Q87.5,by=list(limedays$year),sum)

colnames(yearsum)<-colnames(jsum)<-colnames(ksum)<-colnames(lsum)<-
  colnames(yearsum.lc)<-colnames(jsum.lc)<-colnames(ksum.lc)<-colnames(lsum.lc)<-
  colnames(yearsum.uc)<-colnames(jsum.uc)<-colnames(ksum.uc)<-colnames(lsum.uc)<-c("year","wdays")

#sum actual wale days for comparison
limewdaysabs<-limewdaysabs[as.numeric(as.character(limewdaysabs$year))>1991,]
jsumobs<-aggregate(limewdaysabs$Jpres,by=list(limewdaysabs$year),sum)
ksumobs<-aggregate(limewdaysabs$Kpres,by=list(limewdaysabs$year),sum)
lsumobs<-aggregate(limewdaysabs$Lpres,by=list(limewdaysabs$year),sum)
allsumobs<-aggregate(limewdaysabs$AllSRpres,by=list(limewdaysabs$year),sum)
colnames(jsumobs)<-colnames(ksumobs)<-colnames(lsumobs)<-colnames(allsumobs)<-c("year","wdays")
png(filename="analyses/orcaphen/figures/modwhaledays_lime.png",height=480,width=960)
#windows(height=6,width=12)
par(mfrow=c(2,3))
plot(as.numeric(yearsum$year),yearsum$wdays,ylab= "Year", xlab= "Number of Modeled Whale Days", bty="l", type="l", col=alpha("darkblue",0.8),lwd=2,main = "All Pods")
lines(as.numeric(allsumobs$year),allsumobs$wdays, type="l", col=alpha("darkblue",0.8),, lty=2,lwd=2)
polygon(c(rev(as.numeric(yearsum$year)),as.numeric(yearsum$year)), c(rev(yearsum.uc$wdays), yearsum.lc$wdays), col = alpha("darkblue", 0.2), border = NA)

mtext("A)", side = 3, line = 1, adj=0)
legend("bottomleft",legend=c("Observed", "Model estimate"), lty=c(1,2), col="darkblue", lwd=2, bty="n")
plot(as.numeric(jsum$year),jsum$wdays,ylab= "Year", xlab= "Number of Modeled Whale Days", bty="l", type="l", col="darkblue",lwd=2, main = "J Pod")
lines(as.numeric(jsumobs$year),jsumobs$wdays, type="l", col=alpha("darkblue",0.8),, lty=2,lwd=2)
polygon(c(rev(as.numeric(jsum$year)),as.numeric(jsum$year)), c(rev(jsum.uc$wdays), jsum.lc$wdays), col = alpha("darkblue", 0.2), border = NA)

mtext("B)", side = 3, line = 1, adj=0)

plot(as.numeric(ksum$year),ksum$wdays,ylab= "Year", xlab= "Number of Modeled Whale Days", bty="l", type="l", col="darkblue",lwd=2, main = "K Pod")
lines(as.numeric(ksumobs$year),ksumobs$wdays, type="l", col=alpha("darkblue",0.8),, lty=2,lwd=2)
polygon(c(rev(as.numeric(ksum$year)),as.numeric(ksum$year)), c(rev(ksum.uc$wdays), ksum.lc$wdays), col = alpha("darkblue", 0.2), border = NA)

mtext("C)", side = 3, line = 1, adj=0)

plot(as.numeric(lsum$year),lsum$wdays,ylab= "Year", xlab= "Number of Modeled Whale Days", bty="l", type="l", col="darkblue",lwd=2, main = "L Pod")
lines(as.numeric(lsumobs$year),lsumobs$wdays, type="l", col=alpha("darkblue",0.8), lty=2,lwd=2)
polygon(c(rev(as.numeric(lsum$year)),as.numeric(lsum$year)), c(rev(lsum.uc$wdays), lsum.lc$wdays), col = alpha("darkblue", 0.2), border = NA)

mtext("D)", side = 3, line = 1, adj=0)


#Add modeled salmon cpue and data cpue
chindat<-read.csv("analyses/output/albionchiphen_allyear.csv", header = TRUE)
chindat<-chindat[chindat$year>1993,]
chindat<-chindat[chindat$year<2018,]

chinab<-read.csv("analyses/output/albiongamests.csv", header = TRUE)
#restrict days
chinab<-chinab[chinab$doy<230,]
chinab<-chinab[chinab$doy>90,]
chinab<-chinab[chinab$year>1993,]
chinab<-chinab[chinab$year<2018,]

chinsum<-aggregate(chinab$cpue.est,by=list(chinab$year),sum)
colnames(chinsum)<-c("year","totalcpue")
plot(as.numeric(chindat$year),chindat$alltotal,ylim=c(0,360),xlab= "Year", ylab= "Total Estimated Chinook Abundance", bty="l", lty=1,type="l", col="salmon",lwd=2)
lines(as.numeric(chinsum$year),chinsum$totalcpue,lty=2, col="salmon",lwd=2)

mtext("E)", side = 3, line = 1, adj=0)

dev.off()

