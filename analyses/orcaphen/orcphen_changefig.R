#Figure showing phenological change by phenophase, pod/species, and region
#Goal is to compare rates of change among pods and regions, and between orcas and salmon
#Started August 2, 2019 by Ailene Ettinger

#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)

#library(lme4)
# Set working directory: 
setwd("~/GitHub/fishphen")
#or from work laptop:
#setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

#Read in summaries of rates of change from orca occupancy model
l2<-read.csv("analyses/output/L_2uss_doy92-3031978-2017_ci0.75_occprobmin0.5_wrecent.csv")
l1<-read.csv("analyses/output/L_1ps_doy183-3661978-2017_ci0.75_occprobmin0.5_wrecent.csv")
k2<-read.csv("analyses/output/K_2uss_doy92-3031978-2017_ci0.75_occprobmin0.5_wrecent.csv")
k1<-read.csv("analyses/output/K_1ps_doy183-3661978-2017_ci0.75_occprobmin0.5_wrecent.csv")

j1<-read.csv("analyses/output/J_1ps_doy183-3661978-2017_ci0.75_occprobmin0.5_wrecent.csv")

all2<-rbind(j2,k2,l2)
all1<-rbind(j1,k1,l1)
all2<-all2[order(all2$pod),]

#read in salmon data

wildshifts<-read.csv("analyses/output/salmonreturntrends_wild.csv",header=TRUE)
hatchshifts<-read.csv( "analyses/output/salmonreturntrends_hatch.csv",header=TRUE)

#hatchshifts<-hatchshifts[hatchshifts$type=="hatch",]
albion<-read.csv("analyses/output/albionreturntrends_linmodyrs.csv",header=TRUE)
#albionsp<-read.csv("analyses/output/albionreturntrends_springsum.csv",header=TRUE)
#albionfa<-read.csv("analyses/output/albionreturntrends_fall.csv",header=TRUE)

#colnames(albion)<-colnames(albionsp)<-colnames(albionfa)<-c("name","value")
#albionshifts<-as.data.frame(t(as.numeric(albion[4:28,2])))
colnames(albion)[1]<-"coef"
psshifts<-read.csv("analyses/output/salmonreturntrends_pslmm.csv", header=TRUE)
psshifts<-psshifts[,-2]

#colnames(albionshifts)<-albion[4:28,1]
albionshifts<-albion
# save plotted results as pdf
pdf(file="analyses/figures/srkw_salmon_shifts_lm_1978_2017.pdf",width=16,height=6)
#quartz()
### plot estimates of peak occurrence prob over all years
#quartz(width=16, height=6)
#par(mfcol=c(2,3),mai=c(.5,1,.5,0.5))
par(mfcol=c(2,1),mai=c(.5,1,.5,0.5))
x<-rep(1,times=3)
#Central salish sea first...
plot(x,all2$slope.mn[all2$phase=="first"],pch=c(21,22,24),bg="darkblue", ylab= "Change in timing (days/year)",xaxt="n", xlab="",xlim=c(0,4),ylim=c(-3,3), bty="l")
abline(h=0,lty=2)
arrows(x,all2$slope.lci[all2$phase=="first"],x,all2$slope.uci[all2$phase=="first"], code=3, length=0)
arrows(x+1,all2$slope.lci[all2$phase=="peak"],x+1,all2$slope.uci[all2$phase=="peak"], code=3, length=0)
arrows(x+2,all2$slope.lci[all2$phase=="last"],x+2,all2$slope.uci[all2$phase=="last"], code=3, length=0)

points(x,all2$slope.mn[all2$phase=="first"],pch=c(21,22,24),bg="darkblue")
points(x+1,all2$slope.mn[all2$phase=="peak"],pch=c(21,22,24),bg="darkblue")
points(x+2,all2$slope.mn[all2$phase=="last"],pch=c(21,22,24),bg="darkblue")

axis(side=1,labels=c("First","Peak","Last"), at = c(1,2,3))
mtext("SRKWs 1978-2017",side=3,line=0)
legend("topleft",legend=c("J pod","K pod","L pod"),pch=c(21,22,24), bty="n",pt.bg="darkblue")
#Puget sound proper
plot(x,all1$slope.mn[all1$phase=="first"],pch=c(21,22,24),bg="salmon", ylab= "Change in timing (days/year)",xaxt="n", xlab="",xlim=c(0,4),ylim=c(-10,7), bty="l")
abline(h=0,lty=2)
arrows(x,all1$slope.lci[all1$phase=="first"],x,all1$slope.uci[all1$phase=="first"], code=3, length=0)
arrows(x+1,all1$slope.lci[all1$phase=="peak"],x+1,all1$slope.uci[all1$phase=="peak"], code=3, length=0)
arrows(x+2,all1$slope.lci[all1$phase=="last"],x+2,all1$slope.uci[all1$phase=="last"], code=3, length=0)

points(x,all1$slope.mn[all1$phase=="first"],pch=c(21,22,24),bg="yellow")
points(x+1,all1$slope.mn[all1$phase=="peak"],pch=c(21,22,24),bg="salmon")
points(x+2,all1$slope.mn[all1$phase=="last"],pch=c(21,22,24),bg="salmon")
axis(side=1,labels=c("First","Peak","Last"), at = c(1,2,3))
dev.off()

#add shifts in most recent (1990-2016)
#pdf(file="analyses/figures/srkw_salmon_recentshifts_lm.pdf",width=12,height=10)
png(file="analyses/figures/srkw_salmon_recentshifts_lm.png",width=600,height=500)

###dev.off() plot estimates of peak occurrence prob over all years
#quartz(width=12, height=10)
#par(mfcol=c(2,3),mai=c(.5,1,.5,0.5))
par(mfcol=c(2,2),mai=c(.5,1,.5,0.5))
x<-rep(1,times=3)+c(0,.1,.2)
#Central salish sea first...
plot(x,all2$slope.mn[all2$phase=="first.20012016"],pch=c(21,22,24),bg="darkblue", ylab= "Change in timing (days/year)",xaxt="n", xlab="",xlim=c(0,4),ylim=c(-3,7), bty="l", cex=2)
abline(h=0,lty=2)
arrows(x,all2$slope.lci[all2$phase=="first.20012016"],x,all2$slope.uci[all2$phase=="first.20012016"], code=3, length=0)
arrows(x+1,all2$slope.lci[all2$phase=="peak.20012016"],x+1,all2$slope.uci[all2$phase=="peak.20012016"], code=3, length=0)
arrows(x+2,all2$slope.lci[all2$phase=="last.20012016"],x+2,all2$slope.uci[all2$phase=="last.20012016"], code=3, length=0)

points(x,all2$slope.mn[all2$phase=="first.20012016"],pch=c(21,22,24),bg="darkblue", cex=2)
points(x+1,all2$slope.mn[all2$phase=="peak.20012016"],pch=c(21,22,24),bg="darkblue", cex=2)
points(x+2,all2$slope.mn[all2$phase=="last.20012016"],pch=c(21,22,24),bg="darkblue", cex=2)

axis(side=1,labels=c("First","Peak","Last"), at = c(1,2,3))
#mtext("SRKWs 2001-2016",side=3,line=0)
legend("topleft",legend=c("J pod","K pod","L pod"),pch=c(21,22,24), bty="n",pt.bg="darkblue")
mtext("A)",side=3,line=0, adj=0)

#Puget sound proper
plot(x,all1$slope.mn[all1$phase=="first.20012016"],pch=c(21,22,24),bg="goldenrod", ylab= "Change in timing (days/year)",xaxt="n", xlab="",xlim=c(0,4),ylim=c(-3,7), bty="l", cex=2)
abline(h=0,lty=2)
arrows(x,all1$slope.lci[all1$phase=="first.20012016"],x,all1$slope.uci[all1$phase=="first.20012016"], code=3, length=0)
arrows(x+1,all1$slope.lci[all1$phase=="peak.20012016"],x+1,all1$slope.uci[all1$phase=="peak.20012016"], code=3, length=0)
arrows(x+2,all1$slope.lci[all1$phase=="last.20012016"],x+2,all1$slope.uci[all1$phase=="last.20012016"], code=3, length=0)

points(x,all1$slope.mn[all1$phase=="first.20012016"],pch=c(21,22,24),bg="goldenrod", cex=2)
points(x+1,all1$slope.mn[all1$phase=="peak.20012016"],pch=c(21,22,24),bg="goldenrod", cex=2)
points(x+2,all1$slope.mn[all1$phase=="last.20012016"],pch=c(21,22,24),bg="goldenrod", cex=2)
axis(side=1,labels=c("First","Peak","Last"), at = c(1,2,3))
#dev.off()
x<-c(1,2,3)
y<-c(albionshifts$est[2],albionshifts$est[6],albionshifts$est[4])
ylci<-c(albionshifts$ci25[2],albionshifts$ci25[6],albionshifts$ci25[4])
yuci<-c(albionshifts$ci75[2],albionshifts$ci75[6],albionshifts$ci75[4])
mtext("B)",side=3,line=0, adj=0)

plot(x,y,pch=23,bg="salmon",ylab= "Change in timing (days/year)",xaxt="n", xlab="",xlim=c(0,5), ylim=c(-3,7),bty="l", cex=2)
abline(h=0,lty=2)

arrows(x,ylci,x,yuci, code=3, length=0)
points(x,y,cex=2,bg="salmon",pch=23)
mtext("C)",side=3,line=0, adj=0)
legend("topleft",legend=c("Fraser River Chinook","Multiple Puget Sound Runs"),pch=c(23,25), bty="n",pt.bg="salmon")

#y<-c(albionfa$first.yr,albionfa$mid.yr,albionfa$pk.yr,albionfa$last.yr)
#points(x,y,cex=2,bg="darkblue",pch=23)

y<-c(psshifts[1,2],psshifts[1,4],psshifts[1,6])
ylci<-c(psshifts[2,2],psshifts[2,4],psshifts[2,6])
yuci<-c(psshifts[3,2],psshifts[3,4],psshifts[3,6])
plot(x,y,pch=25,bg="salmon",ylab= "Change in timing (days/year)",xaxt="n", xlab="",xlim=c(0,5), ylim=c(-3,7),bty="l", cex=2)
abline(h=0,lty=2)

arrows(x,ylci,x,yuci, code=3, length=0)
points(x,y,cex=2,bg="salmon",pch=25)
mtext("D)",side=3,line=0, adj=0)

axis(side=1,labels=c("First","Peak","Last"), at = c(1,2,3))

dev.off()
##Separate salmon figure
pdf(file="analyses/figures/salmon_shifts_lm.pdf",width=16,height=6)
par(mfcol=c(2,2),mai=c(.5,1,.5,0.5))

#Central salish sea = albion test fishery from fraser
x<-c(1,2,3,4)
y<-c(albionshifts$first.yr,albionshifts$pk.yr,albionshifts$mid.yr,albionshifts$last.yr)
ylci<-c(albionshifts$first.yrlci,albionshifts$pk.yrlci,albionshifts$mid.yrlci,albionshifts$last.yrlci)
yuci<-c(albionshifts$first.yruci,albionshifts$pk.yruci,albionshifts$mid.yruci,albionshifts$last.yruci)

plot(x,y,pch=23,bg="blue4",ylab= "",xaxt="n", xlab="",xlim=c(0,5), bty="l", cex=log(albionshifts$mn.total)/6)
abline(h=0,lty=2)

mtext("wild salmon",side=3,line=0)
arrows(x,ylci,x,yuci, code=3, length=0,col="blue2")
points(x,y,cex=log(albionshifts$mn.total)/6,bg="blue1",pch=23)
legend("topleft",legend=c("Chum","Chinook","Coho"),pch=c(21,23,25), bty="n",pt.bg="darkblue")

#Hachery shifts 
#Puget sound proper
dd<-wildshifts
x<-rep(1,times=6)
shapes<-c(21,23,25)

cols<-c("lightgray","lightsalmon","salmon2","salmon4","darksalmon")
plot(x,dd$first.yr,pch=shapes[factor(dd$sp)],cex=log(dd$mn.total.runsize)/6,bg=cols[factor(dd$site)], ylab= "",xaxt="n", xlab="",xlim=c(0,5),ylim=c(-10,7), bty="l")
abline(h=0,lty=2)
arrows(x,dd$first.yrlci,x,dd$first.yruci, code=3, length=0,col=cols[factor(dd$site)])
arrows(x+1,dd$pk.yrlci,x+1,dd$pk.yruci, code=3, length=0,col=cols[factor(dd$site)])
arrows(x+2,dd$mid.yrlci,x+2,dd$mid.yruci, code=3, length=0,col=cols[factor(dd$site)])
arrows(x+3,dd$last.yrlci,x+3,dd$last.yruci, code=3, length=0,col=cols[factor(dd$site)])

points(x,dd$first.yr,pch=shapes[factor(dd$sp)],cex=log(dd$mn.total.runsize)/6,bg=cols[factor(dd$site)])
points(x+1,dd$pk.yr,pch=shapes[factor(dd$sp)],cex=log(dd$mn.total.runsize)/6,bg=cols[factor(dd$site)])
points(x+2,dd$mid.yr,pch=shapes[factor(dd$sp)],cex=log(dd$mn.total.runsize)/6,bg=cols[factor(dd$site)])
points(x+3,dd$last.yr,pch=shapes[factor(dd$sp)],cex=log(dd$mn.total.runsize)/6,bg=cols[factor(dd$site)])

axis(side=1,labels=c("First","Peak","Med","Last"), at = c(1,2,3,4))
#Central salish sea first...
cols<-c("skyblue")

dd<-hatchshifts[hatchshifts$site=="WHATCOM CR HATCHERY",]#only one hatchery site in central salish sea, i think
x<-1
plot(x,dd$first.yr,pch=shapes[factor(dd$sp)],cex=log(dd$mn.total.runsize)/6,bg=cols[factor(dd$site)], ylab= "",xaxt="n", xlab="",xlim=c(0,5),ylim=c(-3,3), bty="l")
abline(h=0,lty=2)
arrows(x,dd$first.yrlci,x,dd$first.yruci, code=3, length=0,col=cols[factor(dd$site)])
arrows(x+1,dd$pk.yrlci,x+1,dd$pk.yruci, code=3, length=0,col=cols[factor(dd$site)])
arrows(x+2,dd$mid.yrlci,x+2,dd$mid.yruci, code=3, length=0,col=cols[factor(dd$site)])
arrows(x+3,dd$last.yrlci,x+3,dd$last.yruci, code=3, length=0,col=cols[factor(dd$site)])

points(x,dd$first.yr,pch=shapes[factor(dd$sp)],cex=log(dd$mn.total.runsize)/6,bg=cols[factor(dd$site)])
points(x+1,dd$pk.yr,pch=shapes[factor(dd$sp)],cex=log(dd$mn.total.runsize)/6,bg=cols[factor(dd$site)])
points(x+2,dd$mid.yr,pch=shapes[factor(dd$sp)],cex=log(dd$mn.total.runsize)/6,bg=cols[factor(dd$site)])
points(x+3,dd$last.yr,pch=shapes[factor(dd$sp)],cex=log(dd$mn.total.runsize)/6,bg=cols[factor(dd$site)])

axis(side=1,labels=c("First","Peak","Med","Last"), at = c(1,2,3,4))
mtext("hatchery salmon",side=3,line=0)

#Hachery shifts 
#Puget sound proper
dd<-hatchshifts[hatchshifts$site!="WHATCOM CR HATCHERY",]#only one hatchery site in central salish sea, i think
cols<-c("lightgray","lightsalmon","salmon2","salmon4","darksalmon","darkred")
x<-rep(1, times=length(dd$first.yr))
plot(x,dd$first.yr,pch=shapes[factor(dd$sp)],cex=log(dd$mn.total.runsize)/6,bg=cols[factor(dd$site)], ylab= "",xaxt="n", xlab="",xlim=c(0,5),ylim=c(-10,7), bty="l")
abline(h=0,lty=2)

arrows(x,dd$first.yrlci,x,dd$first.yruci, code=3, length=0,col=cols[factor(dd$site)])
arrows(x+1,dd$pk.yrlci,x+1,dd$pk.yruci, code=3, length=0,col=cols[factor(dd$site)])
arrows(x+2,dd$mid.yrlci,x+2,dd$mid.yruci, code=3, length=0,col=cols[factor(dd$site)])
arrows(x+3,dd$last.yrlci,x+3,dd$last.yruci, code=3, length=0,col=cols[factor(dd$site)])

points(x,dd$first.yr,pch=shapes[factor(dd$sp)],cex=log(dd$mn.total.runsize)/6,bg=cols[factor(dd$site)])
points(x+1,dd$pk.yr,pch=shapes[factor(dd$sp)],cex=log(dd$mn.total.runsize)/6,bg=cols[factor(dd$site)])
points(x+2,dd$mid.yr,pch=shapes[factor(dd$sp)],cex=log(dd$mn.total.runsize)/6,bg=cols[factor(dd$site)])
points(x+3,dd$last.yr,pch=shapes[factor(dd$sp)],cex=log(dd$mn.total.runsize)/6,bg=cols[factor(dd$site)])

axis(side=1,labels=c("First","Peak","Med","Last"), at = c(1,2,3,4))
dev.off()
