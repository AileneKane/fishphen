#plot time series of whale days in central salish sea and ouget sound proper 

if(assumeSRKW==FALSE & use3regions==FALSE){pdf(paste("analyses/figures/OrcaPhenPlots/whaledays",firstyear,"2regs.pdf", sep="_"),height= 6, width = 10)}
if(assumeSRKW==FALSE & use3regions==TRUE){pdf(paste("analyses/figures/OrcaPhenPlots/whaledays",firstyear,"3regs.pdf", sep="_"),height= 6, width = 10)}
if(assumeSRKW==TRUE & use3regions==FALSE){pdf(paste("analyses/figures/OrcaPhenPlots/whaledays",firstyear,"assumeSRKW2regs.pdf", sep="_"),height= 6, width = 10)}
if(assumeSRKW==TRUE & use3regions==TRUE){pdf(paste("analyses/figures/OrcaPhenPlots/whaledays",firstyear,"assumeSRKW3regs.pdf", sep="_"),height= 6, width = 10)}

#quartz(height= 6, width = 10)
plot(rownames(wdays),wdays$uss,type = "l", ylab= "Number of whale days", xlab= "Year", col = "darkblue", lwd=2,ylim= c(0,250),cex.axis=1.2,cex.lab=1.2)
lines(rownames(wdays),wdays$ps,lwd=2,col = "salmon")
legend("topleft",legend=c("Central Salish Sea","Puget Sound Proper"), lty= 1, col=c("darkblue","salmon"), bty="n", lwd=2)
dev.off()

if(assumeSRKW==FALSE & use3regions==FALSE){pdf(paste("analyses/figures/OrcaPhenPlots/whaledays_ps_v_uss",firstyear,"2regs.pdf", sep="_"),height= 6, width = 10)}
if(assumeSRKW==FALSE & use3regions==TRUE){pdf(paste("analyses/figures/OrcaPhenPlots/whaledays_ps_v_uss",firstyear,"3regs.pdf", sep="_"),height= 6, width = 10)}
if(assumeSRKW==TRUE & use3regions==FALSE){pdf(paste("analyses/figures/OrcaPhenPlots/whaledays_ps_v_uss",firstyear,"assumeSRKW2regs.pdf", sep="_"),height= 6, width = 10)}
if(assumeSRKW==TRUE & use3regions==TRUE){pdf(paste("analyses/figures/OrcaPhenPlots/whaledays_ps_v_uss",firstyear,"assumeSRKW3regs.pdf", sep="_"),height= 6, width = 10)}
#plot time series against eachother:
quartz(height= 6, width = 10)
plot(as.numeric(wdays$uss),as.numeric(wdays$ps),type = "p", pch=16,cex.axis=1.2,cex.lab=1.2,ylab= "Number of whale days in Puget Sound", xlab= "Number of whale days in the Central Salish Sea",ylim=c(0,100))
#dev.off()

mod<-lm(as.numeric(wdays$ps)~as.numeric(wdays$uss))
mod.ci<-confint(mod,level=.8)
if (summary(mod)$coeff[2,4]<0.05){abline(mod, lty= 1, lwd=2)}
if (summary(mod)$coeff[2,4]>0.05 & summary(mod)$coeff[2,4]<0.1){abline(mod, lty= 3, lwd=2)}
mtext(paste("r2 = ",round(summary(mod)$r.squared, digits =2)), side=3, adj=.9,line =-2, cex=1.2)
#abline(a=mod.ci[1,1],b=mod.ci[2,1], lty=2)
#abline(a=mod.ci[1,2],b=mod.ci[2,2], lty=2)
dev.off()

#pdf("analyses/figures/OrcaPhenPlots/whaledays_ps_v_uss_2000_2017.pdf",height= 6, width = 10)


numsightings<-as.data.frame(tapply(d$SRKW[d$SRKW==1],list(d$Year[d$SRKW==1],d$region[d$SRKW==1]),length))
pdf("analyses/figures/OrcaPhenPlots/numsighs_2regs.pdf",height= 6, width = 10)
plot(rownames(numsightings),numsightings$uss,type = "l", ylab= "Number of Sightings", xlab= "Year", col = "darkblue", lwd=2,ylim= c(0,7000),cex.axis=1.2,cex.lab=1.2)
lines(rownames(numsightings),numsightings$ps,lwd=2,col = "salmon")
legend("topleft",legend=c("Central Salish Sea","Puget Sound Proper"), lty= 1, col=c("darkblue","salmon"), bty="n", lwd=2)
dev.off()

#plot time series against eachother:
#plot(as.numeric(wdays$uss)[24:41],as.numeric(wdays$ps)[24:41],type = "p", pch=16,ylab= "Number of whale days in Puget Sound", xlab= "Number of whale days in the Central Salish Sea",ylim=c(0,100))
#mod<-lm(as.numeric(wdays$ps)[24:41]~as.numeric(wdays$uss)[24:41])

#mod.ci<-confint(mod,level=.8)
#abline(mod, lty= 1, lwd=1)
#abline(a=mod.ci[1,1],b=mod.ci[2,1], lty=2)
#abline(a=mod.ci[1,2],b=mod.ci[2,2], lty=2)
#dev.off()
#colMeans(wdays[1:20,], na.rm=TRUE)
#with assumeSRKW=TRUE:
#oc         ps        uss 
#1.846154  35.550000 144.450000 

##with assumeSRKW=FALSE:
#       oc         ps        uss 
#0.2307692 13.7500000 94.9000000 

#plot mean numnber of whale days by month for each region
wdays.bymonth.ps<-as.data.frame(tapply(orcasum.days$AllSRpres[orcasum.days$region=="ps"],list(orcasum.days$year[orcasum.days$region=="ps"],orcasum.days$mon[orcasum.days$region=="ps"]),sum))
wdays.monthmean.ps<-colMeans(wdays.bymonth.ps, na.rm=TRUE)
wdays.monthsd.ps<-colSds(as.matrix(wdays.bymonth.ps), na.rm=TRUE)

wdays.bymonth.uss<-tapply(orcasum.days$AllSRpres[orcasum.days$region=="uss"],list(orcasum.days$year[orcasum.days$region=="uss"],orcasum.days$mon[orcasum.days$region=="uss"]),sum)
wdays.monthmean.uss<-colMeans(wdays.bymonth.uss, na.rm=TRUE)
wdays.monthsd.uss<-colSds(as.matrix(wdays.bymonth.uss), na.rm=TRUE)

wdays.bymonth.ps.stan<-wdays.monthmean.ps-mean(wdays.monthmean.ps)
wdays.bymonth.uss.stan<-wdays.monthmean.uss-mean(wdays.monthmean.uss)

pdf("analyses/figures/OrcaPhenPlots/wdays_bymonth.pdf",height= 6, width = 10)
m<-c(seq(1:12))
plot(m,wdays.monthmean.ps,ylab= "Number of Whale Days", xlab= "Month", pch=21,bg = "salmon",ylim= c(0,30),cex.axis=1.2,cex.lab=1.2)

for(i in 1:12){
  arrows(m[i],wdays.monthmean.ps[i]- wdays.monthsd.ps[i],m[i],wdays.monthmean.ps[i]+ wdays.monthsd.ps[i], length=0,code =3, col = "salmon") 
}

for(i in 1:12){
  arrows(m[i],wdays.monthmean.uss[i]- wdays.monthsd.uss[i],m[i],wdays.monthmean.uss[i]+ wdays.monthsd.uss[i], length=0,code =3, col = "darkblue") 
}
points(m,wdays.monthmean.uss, pch=21,bg="darkblue")

legend("topleft",legend=c("Central Salish Sea","Puget Sound Proper"), pch=21,pt.bg=c("darkblue","salmon"), bty="n")
dev.off()

pdf("analyses/figures/OrcaPhenPlots/wdays_bymonth_stan.pdf",height= 6, width = 10)
plot(m,wdays.bymonth.ps.stan,ylab= "Number of Whale Days (standardized)", xlab= "Month", pch=21,bg = "salmon",ylim= c(-15,15),cex.axis=1.2,cex.lab=1.2)

for(i in 1:12){
  arrows(m[i],wdays.bymonth.ps.stan[i]- wdays.monthsd.ps[i],m[i],wdays.bymonth.ps.stan[i]+ wdays.monthsd.ps[i], length=0,code =3, col = "salmon") 
}

for(i in 1:12){
  arrows(m[i],wdays.bymonth.uss.stan[i]- wdays.monthsd.uss[i],m[i],wdays.bymonth.uss.stan[i]+ wdays.monthsd.uss[i], length=0,code =3, col = "darkblue") 
}
points(m,wdays.bymonth.uss.stan,, pch=21,bg="darkblue")

legend("topleft",legend=c("Central Salish Sea","Puget Sound Proper"), pch=21,pt.bg=c("darkblue","salmon"), bty="n")
dev.off()

