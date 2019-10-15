#file to look at trends in phenology using pearse corrections
pods.all.p<-c()
regions.all.p<-c()
years.all.p<-c()
nobs.all.p<-c()
firstest.all.p<-c()
lastest.all.p<-c()
mean.all.p<-c()
meandiffs.all<-c()
firstdiffs.all<-c()
years<-unique(orcasum.days.lime$orcayear)
p=1
for(p in 1:length(podcols)){
  colnum<-which(colnames(orcasum.days.lime)==podcols[p])
  
  regdat<-orcasum.days.lime
  for(y in 1:length(years)){
    yrdat<-regdat[regdat$orcayear==years[y],]
    pods.all.p<-c(pods.all.p,pods[p])
    years.all.p<-c(years.all.p,years[y])
    nobs.all.p<-c(nobs.all.p,length(yrdat$day[yrdat[,colnum]==1]))
    
    firstest.all.p<-rbind(firstest.all.p,est.limit(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]),alpha=alpha,k=k))
    lastest.all.p<-rbind(lastest.all.p,est.limit(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]), upper=TRUE,alpha=alpha,k=k))
    mean.all.p<-rbind(mean.all.p,
                      c(mean(yrdat$daysaftmar31[yrdat[,colnum]==1], na.rm=TRUE),#mean
                        quantile(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]),alpha),#lower ci
                        quantile(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]),1-alpha)))#upper ci
    meandiffs.all<-c(meandiffs.all,mean(diff(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]))))
    firstdiffs.all<-c(firstdiffs.all,diff(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]))[1])
    
  }
}

lime.df <- cbind(lime.df,pods.all.p,years.all.p,nobs.all.p,firstest.all.p,lastest.all.p,mean.all.p)

colnames(lime.df)[10:18]<-c("firstest.p","firstest.plcl","firstest.pucl","lastest.p","lastest.plcl","lastest.pucl","mean.p","mean.plcl","mean.pucl")
lime.df$year<-as.numeric(lime.df$year)+1
lime.df$years.all.p<-as.numeric(lime.df$years.all.p)+1

lime.df$firstest.all<-as.numeric(lime.df$firstest.all)
lime.df$lastest.all<-as.numeric(lime.df$lastest.all)
lime.df$nobs<-as.numeric(lime.df$nobs)
#plot to compare
lime.df<-lime.df[-1,]
meandiffs.all<-meandiffs.all[-1]
quartz(height=8,width=20)
par(mfrow=c(1,5))
plot(lime.df$year,lime.df$firstest.all,type="p",pch=16, col = "black",xlab="Year",ylab="Days after March 31", cex=1.2, bty="l", ylim=c(25,70), main = "Arrival")
arrows(lime.df$year,lime.df$firstest.plcl,lime.df$year,lime.df$firstest.pucl,col="blue",code=3,length=0)
points(lime.df$years.all.p,lime.df$firstest.p,pch=16, col = "blue",cex=1.2)
mod<-lm(lime.df$firstest.all~lime.df$year)
if(summary(mod)$coef[2,4]<alpha){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]>alpha){abline(mod, lty=3)}
mod<-lm(lime.df$firstest.p~lime.df$year)
if(summary(mod)$coef[2,4]<alpha){abline(mod, lty=1, col="blue")}
if(summary(mod)$coef[2,4]>alpha){abline(mod, lty=3, col="blue")}

plot(lime.df$year,lime.df$lastest.all,type="p",pch=16, col = "black",xlab="Year",ylab="Days after March 31", cex=1.2, bty="l", ylim=c(110,190), main = "Departure")
arrows(lime.df$year,lime.df$lastest.plcl,lime.df$year,lime.df$lastest.pucl,col="blue",code=3,length=0)
points(lime.df$year,lime.df$lastest.p,pch=16, col = "blue",cex=1.2)
mod<-lm(lime.df$lastest.all~lime.df$year)
if(summary(mod)$coef[2,4]<alpha){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]>alpha){abline(mod, lty=3)}
mod<-lm(lime.df$lastest.p~lime.df$year)
if(summary(mod)$coef[2,4]<alpha){abline(mod, lty=1, col="blue")}
if(summary(mod)$coef[2,4]>alpha){abline(mod, lty=3, col="blue")}

plot(lime.df$year,lime.df$mean.p,type="p",pch=16, col = "black",xlab="Year",ylab="Days after March 31", cex=1.2, bty="l", ylim=c(50,150), main = "Mean")
arrows(lime.df$year,lime.df$mean.plcl,lime.df$year,lime.df$mean.pucl,col="blue",code=3,length=0)
points(lime.df$year,lime.df$mean.p,pch=16, col = "blue",cex=1.2)
mod<-lm(lime.df$mean.p~lime.df$year)
if(summary(mod)$coef[2,4]<alpha){abline(mod, lty=1, col="blue")}
if(summary(mod)$coef[2,4]>alpha){abline(mod, lty=3, col="blue")}

plot(lime.df$year,meandiffs.all,type="p",pch=16, col = "red",xlab="Year",ylab="Mean Days Between Sightings", cex=1.2, bty="l", ylim=c(1,10), main = "Mean difference")
#plot(lime.df$year,firstdiffs.all,type="p",pch=16, col = "red",xlab="Year",ylab="Days Between First 2 Sightings", cex=1.2, bty="l", ylim=c(1,10), main = "First difference")
mod<-lm(meandiffs.all~lime.df$year)
if(summary(mod)$coef[2,4]<alpha){abline(mod, lty=1, col="red")}
if(summary(mod)$coef[2,4]>alpha){abline(mod, lty=3, col="red")}

plot(lime.df$year,lime.df$nobs,type="p",pch=16, col = "black",xlab="Year",ylab="# days", cex=1.2, bty="l", ylim=c(1,70), main = "Whale Days")
mod<-lm(lime.df$nobs~lime.df$year)
if(summary(mod)$coef[2,4]<alpha){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]>alpha){abline(mod, lty=3)}

#firstlastmod<-lm(lastest.all[2:dim(lime.df)[1]]~firstest.all[2:dim(lime.df)[1]], data=lime.df)
#firstlastestmod<-lm(lastest.p[2:dim(lime.df)[1]]~firstest.p[2:dim(lime.df)[1]], data=lime.df)
#removing first value doesn't have much effect
firstlastmod<-lm(lastest.all~firstest.all, data=lime.df)
firstlastestmod<-lm(lastest.p~firstest.p, data=lime.df)

summary(firstlastmod)
summary(firstlastestmod)

daysfirstmod<-lm(firstest.all~nobs, data=lime.df)
dayslastmod<-lm(lastest.all~nobs, data=lime.df)
daysfirstestmod<-lm(firstest.p~nobs, data=lime.df)
dayslastestmod<-lm(lastest.p~nobs, data=lime.df)

summary(daysfirstmod)#later first obs in years with few whale days
summary(dayslastmod)#earlier last obs in years with few whale days
summary(daysfirstestmod)#later first obs in years with few whale days
summary(dayslastestmod)#earlier last obs in years with few whale days
#estimates are more conservative...
