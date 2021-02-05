#Script to make schematic figure of hypotheses tested with the SRKW work
#started October 8, 2019
#Create phenology curve for SRKWs:
meannum<-30
phenshift<-10
numshift<--5
doy<-seq(1,365,by=1)
whaledays<-dnorm(doy,mean=200,sd=50)
preydays<-dnorm(doy,mean=200,sd=80)

doy_shift<-doy+50
pdf("/Users/aileneettinger/Documents/GitHub/fishphen/analyses/figures/srkw_schematic.pdf",height=8, width=8)
#quartz(height=8,width=8)

par(mfrow=c(2,2))
plot(doy,whaledays, xlab= "day of year", ylab= "occurrence probability",xlim=c(1,365),ylim=c(0,max(whaledays)+.001),yaxt="n", type="l", col = "darkblue",lty=2, lwd=2, cex.axis=1.5, cex.lab=1.5, bty="l")

lines(doy_shift,whaledays,lty=1, lwd=2,col="darkblue")
lines(doy,preydays,lty=2, lwd=2,col="salmon")
lines(doy_shift,preydays,lty=1, lwd=2,col="salmon")

points(doy[which(whaledays==max(whaledays))],max(whaledays)+.0003,pch=21,bg="darkblue", cex=1.2)
points(doy_shift[which(whaledays==max(whaledays))],max(whaledays)+.0003,pch=21,bg="darkblue", cex=1.2)
points(doy[which(preydays==max(preydays))],max(preydays)+.0003,pch=21,bg="salmon", cex=1.2)
points(doy_shift[which(preydays==max(preydays))],max(preydays)+.0003,pch=21,bg="salmon", cex=1.2)

mtext("Matched phenological shifts:", side=3, line=3, adj=0, cex=1.2)
mtext("A)", side=3, line=1, adj=0, cex=1.2)

legend("topleft",legend=c("past","present"),lty=c(2,1), lwd=2, bty="n", col= "darkblue")
arrows(doy[which(whaledays==max(whaledays))],max(whaledays)+.0007,doy_shift[which(whaledays==max(whaledays))],max(whaledays)+.0007, code=2, angle=45, length=.05, lwd=2, col="darkblue")
arrows(doy[which(preydays==max(preydays))],max(preydays)+.0007,doy_shift[which(preydays==max(preydays))],max(preydays)+.0007, code=2, angle=45, length=.05, lwd=2, col="salmon")
text(doy_shift[which(whaledays==max(whaledays))]+60,max(whaledays)+.0007,"predator shift")
text(doy_shift[which(preydays==max(preydays))]+50,max(preydays)+.0007,"prey shift")

plot(doy,doy, xlab= "peak prey day", ylab= "peak predator day", type="l", lty=1, lwd=2, cex.axis=1.5, cex.lab=1.5, bty="l")
mtext("B)", side=3, line=1, adj=0, cex=1.2)

plot(doy,whaledays, xlab= "day of year", ylab= "occurrence probability",xlim=c(1,365),ylim=c(0,max(whaledays)+.001),yaxt="n", type="l", col = "darkblue",lty=2, lwd=2, cex.axis=1.5, cex.lab=1.5, bty="l")
doy_shift_mis<-doy+10
lines(doy_shift_mis,whaledays,lty=1, lwd=2,col="darkblue")
lines(doy,preydays,lty=2, lwd=2,col="salmon")
lines(doy_shift,preydays,lty=1, lwd=2,col="salmon")
points(doy[which(whaledays==max(whaledays))],max(whaledays)+.0003,pch=21,bg="darkblue", cex=1.2)
points(doy_shift_mis[which(whaledays==max(whaledays))],max(whaledays)+.0003,pch=21,bg="darkblue", cex=1.2)
points(doy[which(preydays==max(preydays))],max(preydays)+.0003,pch=21,bg="salmon", cex=1.2)
points(doy_shift[which(preydays==max(preydays))],max(preydays)+.0003,pch=21,bg="salmon", cex=1.2)


mtext("Mismatched phenological shifts:", side=3, line=3, adj=0, cex=1.2)
arrows(doy[which(whaledays==max(whaledays))],max(whaledays)+.0007,doy_shift_mis[which(whaledays==max(whaledays))],max(whaledays)+.0007, code=2, angle=45, length=.05, lwd=2,col="darkblue")
arrows(doy[which(preydays==max(preydays))],max(preydays)+.0007,doy_shift[which(preydays==max(preydays))],max(preydays)+.0007, code=2, angle=45, length=.05, lwd=2, col="salmon")
text(doy_shift_mis[which(whaledays==max(whaledays))]+50,max(whaledays)+.0007,"predator shift")
text(doy_shift[which(preydays==max(preydays))]+60,max(preydays)+.0007,"prey shift")
mtext("C)", side=3, line=1, adj=0, cex=1.2)

plot(doy,rep(mean(doy),times=length(doy)), xlab= "peak prey day", ylab= "peak predator day", type="l", lty=3, lwd=2, cex.axis=1.5, cex.lab=1.5, bty="l")
mtext("D)", side=3, line=1, adj=0, cex=1.2)

dev.off()

#make a schematic of multiple salmon stocks

pdf("/Users/aileneettinger/Documents/GitHub/fishphen/analyses/figures/salmonportfolio_schematic.pdf",height=8, width=8)
#quartz(height=8,width=8)



par(mfrow=c(1,1))
plot(doy,preydays*100, xlab= "day of year", ylab= "abundance",xlim=c(1,365),ylim=c(0,100*max(whaledays)+.001),yaxt="n", type="l", col = "salmon",lty=1, lwd=3, cex.axis=1.5, cex.lab=1.5, bty="l")
stocknum<-6
stockmns<-as.integer(rnorm(stocknum,mean = 200,sd =80))
sds<-as.integer(rnorm(stocknum,mean = 40, sd = 10))
scale<-c(30,40,30,10,15,40)
cols<-c("purple","salmon", "darksalmon","gray","maroon","darkgray")
for (i in 1:stocknum){
  stockphen<-scale[i]*dnorm(doy,stockmns[i],sds[i])
  lines(doy,stockphen,lty=1, lwd=3,col=alpha(cols[i],.4))
}
dev.off()
