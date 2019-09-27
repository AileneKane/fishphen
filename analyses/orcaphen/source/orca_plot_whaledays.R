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
#quartz(height= 6, width = 10)
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
if(assumeSRKW==FALSE & use3regions==FALSE){pdf(paste("analyses/figures/OrcaPhenPlots/numsights",firstyear,"2regs.pdf", sep="_"),height= 6, width = 10)}
if(assumeSRKW==FALSE & use3regions==TRUE){pdf(paste("analyses/figures/OrcaPhenPlots/numsights",firstyear,"3regs.pdf", sep="_"),height= 6, width = 10)}
if(assumeSRKW==TRUE & use3regions==FALSE){pdf(paste("analyses/figures/OrcaPhenPlots/numsights",firstyear,"assumeSRKW2regs.pdf", sep="_"),height= 6, width = 10)}
if(assumeSRKW==TRUE & use3regions==TRUE){pdf(paste("analyses/figures/OrcaPhenPlots/numsights",firstyear,"assumeSRKW3regs.pdf", sep="_"),height= 6, width = 10)}
#quartz(height= 6, width = 10)

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
wdays.bymonth.uss<-tapply(orcasum.days$AllSRpres[orcasum.days$region=="uss"],list(orcasum.days$year[orcasum.days$region=="uss"],orcasum.days$mon[orcasum.days$region=="uss"]),sum)
wdays.monthmean.uss<-colMeans(wdays.bymonth.uss, na.rm=TRUE)
wdays.monthsd.uss<-colSds(as.matrix(wdays.bymonth.uss), na.rm=TRUE)

wdays.bymonth.ps.stan<-wdays.monthmean.ps-mean(wdays.monthmean.ps)
wdays.bymonth.uss.stan<-wdays.monthmean.uss-mean(wdays.monthmean.uss)
#most recent 8 years only
wdays.monthmean.uss.recent<-colMeans(wdays.bymonth.uss[(dim(wdays.bymonth.uss)[1]-8):(dim(wdays.bymonth.uss)[1]),], na.rm=TRUE)
wdays.monthsd.uss.recent<-colSds(as.matrix(wdays.bymonth.uss[(dim(wdays.bymonth.uss)[1]-8):(dim(wdays.bymonth.uss)[1]),], na.rm=TRUE))
wdays.monthmean.ps.recent<-colMeans(wdays.bymonth.ps[(dim(wdays.bymonth.ps)[1]-7):(dim(wdays.bymonth.ps)[1]),], na.rm=TRUE)
wdays.monthsd.ps.recent<-colSds(as.matrix(wdays.bymonth.ps[(dim(wdays.bymonth.ps)[1]-8):(dim(wdays.bymonth.ps)[1]),]), na.rm=TRUE)

#8 years prior to most recent 8 years (everything EXCEPT the most recent 10 years)
wdays.monthmean.uss.old<-colMeans(wdays.bymonth.uss[(dim(wdays.bymonth.uss)[1]-16):(dim(wdays.bymonth.uss)[1]-9),], na.rm=TRUE)
wdays.monthsd.uss.old<-colSds(as.matrix(wdays.bymonth.uss[(dim(wdays.bymonth.uss)[1]-16):(dim(wdays.bymonth.uss)[1]-9),]), na.rm=TRUE)
wdays.monthmean.ps.old<-colMeans(wdays.bymonth.ps[(dim(wdays.bymonth.ps)[1]-16):(dim(wdays.bymonth.ps)[1]-9),], na.rm=TRUE)
wdays.monthsd.ps.old<-colSds(as.matrix(wdays.bymonth.ps[(dim(wdays.bymonth.ps)[1]-16):(dim(wdays.bymonth.ps)[1]-9),]), na.rm=TRUE)


if(assumeSRKW==FALSE & use3regions==FALSE){pdf(paste("analyses/figures/OrcaPhenPlots/wdays_bymonth",firstyear,"2regs.pdf", sep="_"),height= 12, width = 6)}
if(assumeSRKW==FALSE & use3regions==TRUE){pdf(paste("analyses/figures/OrcaPhenPlots/wdays_bymonth",firstyear,"3regs.pdf", sep="_"),height= 12, width = 6)}
if(assumeSRKW==TRUE & use3regions==FALSE){pdf(paste("analyses/figures/OrcaPhenPlots/wdays_bymonth",firstyear,"assumeSRKW2regs.pdf", sep="_"),height= 12, width = 6)}
if(assumeSRKW==TRUE & use3regions==TRUE){pdf(paste("analyses/figures/OrcaPhenPlots/wdays_bymonth",firstyear,"assumeSRKW3regs.pdf", sep="_"),height= 12, width = 6)}

#quartz(height=10, width=5)
par(mfrow=c(2,1))
m<-c(seq(1:12))
plot(m,wdays.monthmean.uss,ylab= "Number of Whale Days", xlab= "Month", pch=21,bg = "darkblue",ylim= c(0,30),cex.axis=1.2,cex.lab=1.2, main= "Central Salish Sea")

for(i in 1:12){
  arrows(m[i],wdays.monthmean.uss[i]- wdays.monthsd.uss[i],m[i],wdays.monthmean.uss[i]+ wdays.monthsd.uss[i], length=0,code =3, col = "darkblue") 
}
lines(m,wdays.monthmean.uss, col="darkblue")

plot(m,wdays.monthmean.ps,ylab= "Number of Whale Days", xlab= "Month", pch=21,bg = "salmon",ylim= c(0,12),cex.axis=1.2,cex.lab=1.2, main= "Puget Sound Proper")

for(i in 1:12){
  arrows(m[i],wdays.monthmean.ps[i]- wdays.monthsd.ps[i],m[i],wdays.monthmean.ps[i]+ wdays.monthsd.ps[i], length=0,code =3, col = "salmon") 
}


#legend("topleft",legend=c("","Puget Sound Proper"), pch=21,pt.bg=c("darkblue","salmon"), bty="n")
dev.off()

if(assumeSRKW==FALSE & use3regions==FALSE){pdf(paste("analyses/figures/OrcaPhenPlots/wdays_bymonth_earlylate",firstyear,"2regs.pdf", sep="_"),height= 12, width = 8)}
if(assumeSRKW==FALSE & use3regions==TRUE){pdf(paste("analyses/figures/OrcaPhenPlots/wdays_bymonth_earlylate",firstyear,"3regs.pdf", sep="_"),height= 12, width = 8)}
if(assumeSRKW==TRUE & use3regions==FALSE){pdf(paste("analyses/figures/OrcaPhenPlots/wdays_bymonth_earlylate",firstyear,"assumeSRKW2regs.pdf", sep="_"),height= 12, width = 8)}
if(assumeSRKW==TRUE & use3regions==TRUE){pdf(paste("analyses/figures/OrcaPhenPlots/wdays_bymonth_earlylate",firstyear,"assumeSRKW3regs.pdf", sep="_"),height= 12, width = 8)}

#quartz(height=10, width=5)
par(mfrow=c(2,1))
m<-c(seq(1:12))
plot(m,wdays.monthmean.uss.old,ylab= "Number of Whale Days", xlab= "Month", type="l",col= "royalblue",ylim= c(0,35),cex.axis=1.2,cex.lab=1.2, main= "Central Salish Sea", lwd=2, lty=2)
polygon(c(rev(m),m),c(rev(wdays.monthmean.uss.old+ wdays.monthsd.uss.old),wdays.monthmean.uss.old- wdays.monthsd.uss.old),col=alpha("royalblue",0.1),lty=0)
lines(m,wdays.monthmean.uss.recent, col="darkblue", lty=1, lwd=2)
polygon(c(rev(m),m),c(rev(wdays.monthmean.uss.recent+ wdays.monthsd.uss.recent),wdays.monthmean.uss.recent- wdays.monthsd.uss.recent),col=alpha("darkblue",0.1),lty=0)
legend("topleft",
       legend=c(paste(row.names(wdays.bymonth.uss)[dim(wdays.bymonth.uss)[1]-8],"-",row.names(wdays.bymonth.uss)[dim(wdays.bymonth.uss)[1]]),
                paste(row.names(wdays.bymonth.uss)[dim(wdays.bymonth.uss)[1]-16],"-",row.names(wdays.bymonth.uss)[dim(wdays.bymonth.uss)[1]-9])),
                 lty=c(1,2),col=c("darkblue","royalblue"), bty="n", lwd=2, cex=0.9)

plot(m,wdays.monthmean.ps.old,ylab= "Number of Whale Days", xlab= "Month", type="l",col= "lightsalmon4",ylim= c(0,15),cex.axis=1.2,cex.lab=1.2, main= "Puget Sound", lwd=2, lty=2)
polygon(c(rev(m),m),c(rev(wdays.monthmean.ps.old+ wdays.monthsd.ps.old),wdays.monthmean.ps.old- wdays.monthsd.ps.old),col=alpha("lightsalmon4",0.1),lty=0)
lines(m,wdays.monthmean.ps.recent, col="salmon", lty=1, lwd=2)
polygon(c(rev(m),m),c(rev(wdays.monthmean.ps.recent+ wdays.monthsd.ps.recent),wdays.monthmean.ps.recent- wdays.monthsd.ps.recent),col=alpha("salmon",0.1),lty=0)
legend("topleft",
       legend=c(paste(row.names(wdays.bymonth.ps)[dim(wdays.bymonth.uss)[1]-8],"-",row.names(wdays.bymonth.uss)[dim(wdays.bymonth.uss)[1]]),
                paste(row.names(wdays.bymonth.ps)[dim(wdays.bymonth.uss)[1]-16],"-",row.names(wdays.bymonth.uss)[dim(wdays.bymonth.uss)[1]-9])),
                , lty=c(1,2),col=c("salmon","lightsalmon4"), bty="n", lwd=2, cex=0.9)
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

#trends in whale days per month
Jmod.ps<-lm(wdays.J$ps~as.numeric(rownames(wdays.J)))
Jmod.uss<-lm(wdays.J$uss~as.numeric(rownames(wdays.J)))

Jmod.ps.recent<-lm(wdays.J$ps[(length(wdays.J$ps)-15):(length(wdays.J$ps))]~as.numeric(rownames(wdays.J)[(length(wdays.J$ps)-15):(length(wdays.J$ps))]))
Jmod.uss.recent<-lm(wdays.J$uss[(length(wdays.J$uss)-15):(length(wdays.J$uss))]~as.numeric(rownames(wdays.J)[(length(wdays.J$uss)-15):(length(wdays.J$uss))]))

summary(Jmod.ps);summary(Jmod.ps.recent)
summary(Jmod.uss);summary(Jmod.uss.recent)

#trends in whale days per month K
Kmod.ps<-lm(wdays.K$ps~as.numeric(rownames(wdays.K)))
Kmod.uss<-lm(wdays.K$uss~as.numeric(rownames(wdays.K)))

Kmod.ps.recent<-lm(wdays.K$ps[(length(wdays.K$ps)-15):(length(wdays.K$ps))]~as.numeric(rownames(wdays.K)[(length(wdays.K$ps)-15):(length(wdays.K$ps))]))
Kmod.uss.recent<-lm(wdays.K$uss[(length(wdays.K$uss)-15):(length(wdays.K$uss))]~as.numeric(rownames(wdays.K)[(length(wdays.K$uss)-15):(length(wdays.K$uss))]))

summary(Kmod.ps);summary(Kmod.ps.recent)
summary(Kmod.uss);summary(Kmod.uss.recent)

#trends in whale days per month L
Lmod.ps<-lm(wdays.L$ps~as.numeric(rownames(wdays.L)))
Lmod.uss<-lm(wdays.L$uss~as.numeric(rownames(wdays.L)))

Lmod.ps.recent<-lm(wdays.L$ps[(length(wdays.L$ps)-15):(length(wdays.L$ps))]~as.numeric(rownames(wdays.L)[(length(wdays.L$ps)-15):(length(wdays.L$ps))]))
Lmod.uss.recent<-lm(wdays.L$uss[(length(wdays.L$uss)-15):(length(wdays.L$uss))]~as.numeric(rownames(wdays.L)[(length(wdays.L$uss)-15):(length(wdays.L$uss))]))

summary(Lmod.ps);summary(Lmod.ps.recent)
summary(Lmod.uss);summary(Lmod.uss.recent)

#trends in whale days per month ALL SRKW
mod.ps<-lm(wdays$ps~as.numeric(rownames(wdays)))
mod.uss<-lm(wdays$uss~as.numeric(rownames(wdays)))

mod.ps.recent<-lm(wdays$ps[(length(wdays$ps)-15):(length(wdays$ps))]~as.numeric(rownames(wdays)[(length(wdays$ps)-15):(length(wdays$ps))]))
mod.uss.recent<-lm(wdays$uss[(length(wdays$uss)-15):(length(wdays$uss))]~as.numeric(rownames(wdays)[(length(wdays$uss)-15):(length(wdays$uss))]))

summary(mod.ps);summary(mod.ps.recent)
summary(mod.uss);summary(mod.uss.recent)
