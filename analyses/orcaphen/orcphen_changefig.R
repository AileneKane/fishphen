#Figure showing phenological change by phenophase, pod/species, and region
#Goal is to compare rates of change among pods and regions, and between orcas and salmon
#Started August 2, 2019 by Ailene Ettinger

#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Set working directory: 
setwd("~/Documents/GitHub/fishphen")
#or from laptop:
#setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

#Read in summaries of rates of change from orca occupancy model
l2<-read.csv("analyses/output/L_2_uss.csv")
#l1<-read.csv("analyses/output/L_1_ps.csv")
l1<-read.csv("analyses/output/L_1sept1_ps.csv")
k2<-read.csv("analyses/output/K_2_uss.csv")
#k1<-read.csv("analyses/output/K_1_ps.csv")
k1<-read.csv("analyses/output/K_1sept1_ps.csv")
j2<-read.csv("analyses/output/J_2_uss.csv")
#j1<-read.csv("analyses/output/J_1_ps.csv")
j1<-read.csv("analyses/output/J_1sept1_ps.csv")

all2<-rbind(j2,k2,l2)
all1<-rbind(j1,k1,l1)
all2<-all2[order(all2$pod),]
# save plotted results as pdf
pdf(file=paste("analyses/figures/",pod,"/orcaphen_1976_2017","_",region,"_",season,"_",pod,"_",prob,".pdf", sep=""),width=7,height=6)

### plot estimates of peak detectability over all years
#quartz(width=7, height=6)
par(mfcol=c(2,2),mai=c(1,1,1,0.5))
x<-rep(1,times=3)
plot(x,all2$slope.mn[all2$phase=="first"],pch=c(21,22,24),bg="darkblue", ylab= "Change in timing (days)",xaxt="n", xlab="",xlim=c(0,4),ylim=c(-2,1), bty="l")
abline(h=0,lty=2)
arrows(x,all2$slope.lci[all2$phase=="first"],x,all2$slope.uci[all2$phase=="first"], code=3, length=0)
arrows(x+1,all2$slope.lci[all2$phase=="peak"],x+1,all2$slope.uci[all2$phase=="peak"], code=3, length=0)
arrows(x+2,all2$slope.lci[all2$phase=="last"],x+2,all2$slope.uci[all2$phase=="last"], code=3, length=0)

points(x,all2$slope.mn[all2$phase=="first"],pch=c(21,22,24),bg="darkblue")
points(x+1,all2$slope.mn[all2$phase=="peak"],pch=c(21,22,24),bg="darkblue")
points(x+2,all2$slope.mn[all2$phase=="last"],pch=c(21,22,24),bg="darkblue")

axis(side=1,labels=c("First","Peak","Last"), at = c(1,2,3))


plot(x,all1$slope.mn[all1$phase=="first"],pch=c(21,22,24),bg="salmon", ylab= "Change in timing (days)",xaxt="n", xlab="",xlim=c(0,4),ylim=c(-2,1), bty="l")
abline(h=0,lty=2)
arrows(x,all1$slope.lci[all1$phase=="first"],x,all1$slope.uci[all1$phase=="first"], code=3, length=0)
arrows(x+1,all1$slope.lci[all1$phase=="peak"],x+1,all1$slope.uci[all1$phase=="peak"], code=3, length=0)
arrows(x+2,all1$slope.lci[all1$phase=="last"],x+2,all1$slope.uci[all1$phase=="last"], code=3, length=0)

points(x,all1$slope.mn[all1$phase=="first"],pch=c(21,22,24),bg="salmon")
points(x+1,all1$slope.mn[all1$phase=="peak"],pch=c(21,22,24),bg="salmon")
points(x+2,all1$slope.mn[all1$phase=="last"],pch=c(21,22,24),bg="salmon")

axis(side=1,labels=c("First","Peak","Last"), at = c(1,2,3))
##Add salmon
shifts<-read.csv( "analyses/output/salmonreturntrends.csv", header=TRUE)
#how to decide which salmon returns to use?
#use the following criteria:
#LArge run size
#close to puget sound
#have wild and hatchery fish
