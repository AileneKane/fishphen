#Run simple linear models on srkw data
years<-seq(styr,2017, by=1)#restrict to these years for orcayears
#Look at trends in SRKW phenology across all years, for all pods
years.all<-c()
nobs.all<-c()
firstest.all<-c()
lastest.all<-c()

for(y in 1:length(years)){
  yrdat<-orcasum.days[orcasum.days$orcayear==years[y],]
  years.all<-c(years.all,years[y])
  nobs.all<-c(nobs.all,length(yrdat$day[yrdat$AllSRobs==1]))
  firstest.all<-c(firstest.all,min(yrdat$daysaftmar31[yrdat$AllSRobs==1], na.rm=TRUE))
  lastest.all<-c(lastest.all,max(yrdat$daysaftmar31[yrdat$AllSRobs==1], na.rm=TRUE))
}
df <- as.data.frame(cbind(years.all,nobs.all,firstest.all,lastest.all))
colnames(df)[1:2]<-c("year","nobs")
windows()
plot(df$year,df$firstest.all,xlab="year",ylab="first obs doy", main="", bty="l", pch=21, bg="gray")
mod<-lm(df$firstest.all~df$year)
abline(mod)
mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
windows()
plot(df$year,df$lastest.all,xlab="year",ylab="last obs doy", main="", bty="l", pch=21, bg="gray")
mod<-lm(df$lastest.all~df$year)
abline(mod)
mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)

#Create dataframe with first, last obs for each start date in each year
pods.all<-c()
regions.all<-c()
years.all<-c()
nobs.all<-c()
#firstest.1may.all<-c()#for uss
#firstest.1oct.all<-c()#for ps
#lastest.31oct.all<-c()#for uss
#lastest.31jan.all<-c()#for ps
firstest.all<-c()
lastest.all<-c()

#p=1
#r=1

#unique(orcasum.days$orcayear)#use may 1 as start of orca year, as this will encompass min start date window that i want to try
podcols<-c("Jobs","Kobs", "Lobs","AllSRobs")
for(p in 1:length(podcols)){
  colnum<-which(colnames(orcasum.days)==podcols[p])
  for(r in 1:2){
    regdat<-orcasum.days[orcasum.days$region==regions[r],]
    
    if (regions[r]=="uss"){
      regdat<-regdat[as.numeric(regdat$daysaftmar31)<185,]#may 1 through sept 30 for summer season
    }
    if (regions[r]=="ps"){
      regdat<-regdat[as.numeric(regdat$daysaftmar31)>=154 &as.numeric(regdat$daysaftmar31)<277,]#include data oct 1 through jan 31
    }
    for(y in 1:length(years)){
      yrdat<-regdat[regdat$orcayear==years[y],]
      pods.all<-c(pods.all,pods[p])
      regions.all<-c(regions.all,regions[r])
      years.all<-c(years.all,years[y])
      nobs.all<-c(nobs.all,length(yrdat$day[yrdat[,colnum]==1]))
      
      #      if (regions[r]=="uss"){
      #firstest.1may.all<-c(firstest.1may.all,min(yrdat$daysaftapr30[yrdat[,colnum]==1], na.rm=TRUE))#1may=1 day after apr30stdays
      #lastest.31oct.all<-c(lastest.31oct.all,max(yrdat$daysaftapr30[yrdat[,colnum]==1], na.rm=TRUE))#31oct= 184 days after apr30
      #firstest.1oct.all<-c(firstest.1oct.all,NA)
      #lastest.31jan.all<-c(lastest.31jan.all,NA)
      #       }
      #      if (regions[r]=="ps"){
      #       firstest.1oct.all<-c(firstest.1oct.all,min(yrdat$daysaftapr30[yrdat[,colnum]==1], na.rm=TRUE))}
      #        lastest.31jan.all<-c(lastest.31jan.all,max(yrdat$daysaftapr30[yrdat[,colnum]==1], na.rm=TRUE))
      #       firstest.1may.all<-c(firstest.1may.all,NA)
      #      lastest.31oct.all<-c(lastest.31oct.all,NA)
      #     }
      firstest.all<-c(firstest.all,min(yrdat$daysaftmar31[yrdat[,colnum]==1], na.rm=TRUE))
      lastest.all<-c(lastest.all,max(yrdat$daysaftmar31[yrdat[,colnum]==1], na.rm=TRUE))
      
    }
  }
}

df <- as.data.frame(cbind(pods.all,regions.all,years.all,nobs.all,firstest.all,lastest.all))
colnames(df)[1:4]<-c("pod","region","year","nobs")

#Plot trends using different start and end dates

pod.df=df[df$pod=="SRs",]
pod.df$firstest.all[which(pod.df$firstest.all=="Inf")]<-NA
pod.df$lastest.all[which(pod.df$lastest.all=="-Inf")]<-NA

#
pod.df$firstest.all<-as.numeric(pod.df$firstest.all)

pod.df$lastest.all<-as.numeric(pod.df$lastest.all)

pod.df$year<-as.numeric(pod.df$year)
windows(height=6,width=7)
par(mfrow=c(2,2))
for(r in 1:2){
  reg.df<-pod.df[pod.df$region==regions[r],]
  
  
  #first obs 
  plot(reg.df$year,reg.df$firstest.all,xlab="year",ylab="first obs doy", main=paste(regions[r]), bty="l", pch=21, bg="gray")
  mod<-lm(reg.df$firstest.all~reg.df$year)
  abline(mod)
  mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
  #last obs
  plot(reg.df$year,reg.df$lastest.all,xlab="year",ylab="last obs doy", main=paste(regions[r]), bty="l", pch=21, bg="gray")
  mod<-lm(reg.df$lastest.all~reg.df$year)
  mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  abline(mod)
  print(summary(mod))
  mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
  
}

#Linear models don't make sense given the data- look like nonlinear relationships
#Make boxplots similar to the way that i made them for test data
pod.df=df[df$pod=="SRs",]
pod.df$firstest[which(pod.df$firstest=="Inf")]<-NA
pod.df$lastest[which(pod.df$lastest=="-Inf")]<-NA
pod.df$firstest<-as.numeric(pod.df$firstest)
pod.df$lastest<-as.numeric(pod.df$lastest)

pod.df$decade<-"1977-1986"
pod.df$decade[pod.df$year>1986]<-"1987-1996"
pod.df$decade[pod.df$year>1996]<-"1997-2006"
pod.df$decade[pod.df$year>2006]<-"2007-2016"
if(styr==1978){
  pod.df$period<-"1978-1997"
  pod.df$period[pod.df$year>1997]<-"1998-2017"}

if(styr==2001){
  pod.df$period<-"2001-2009"
  pod.df$period[pod.df$year>2009]<-"2010-2017"
  }




windows()
par(mfrow=c(1,2))
#First obs
boxplot(as.numeric(pod.df$firstest[pod.df$region=="ps"])~as.factor(pod.df$period[pod.df$region=="ps"]), xlab="Period", ylab="Estimate of first obs (doy) in PS", main="First obs")
first.t.ps<-t.test(as.numeric(pod.df$firstest[pod.df$region=="ps"])~pod.df$period[pod.df$region=="ps"], paired = FALSE, var.equal = FALSE,conf.level=0.95)
#mtext(paste("Change=",-1*round(t$estimate[1]-t$estimate[2], digits=1),"(",-1*round(t$conf.int[1],digits=1),",",-1*round(t$conf.int[2],digits=1),")", sep=""),side=3,line=-3, adj=1)

#Last obs
boxplot(as.numeric(pod.df$lastest[pod.df$region=="ps"])~as.factor(pod.df$period[pod.df$region=="ps"]), xlab="Period", ylab="Estimate of last obs (doy) in PS", main="Last obs")
last.t.ps<-t.test(as.numeric(pod.df$lastest[pod.df$region=="ps"])~as.factor(pod.df$period[pod.df$region=="ps"]), conf.level=0.95)
#mtext(paste("Change=",-1*round(t$estimate[1]-t$estimate[2], digits=1),"(",-1*round(t$conf.int[1],digits=1),",",-1*round(t$conf.int[2],digits=1),")", sep=""),side=1,line=-3, adj=1)

#create a vector with the differences, like in the simulation code:
firstdif.ps<-first.t.ps$estimate[2]-first.t.ps$estimate[1]
firstdifp.ps<-first.t.ps$p.value
lastdif.ps<-last.t.ps$estimate[2]-last.t.ps$estimate[1]
lastdifp.ps<-last.t.ps$p.value

par(mfrow=c(1,2))
#First obs
boxplot(as.numeric(pod.df$firstest[pod.df$region=="uss"])~as.factor(pod.df$period[pod.df$region=="uss"]), xlab="Period", ylab="Estimate of first obs (doy) in USS", main="First obs")
first.t.uss<-t.test(as.numeric(pod.df$firstest[pod.df$region=="uss"])~pod.df$period[pod.df$region=="uss"], paired = FALSE, var.equal = FALSE,conf.level=0.5)
#mtext(paste("Change=",-1*round(t$estimate[1]-t$estimate[2], digits=1),"(",-1*round(t$conf.int[1],digits=1),",",-1*round(t$conf.int[2],digits=1),")", sep=""),side=3,line=-3, adj=1)
#Last obs
boxplot(as.numeric(pod.df$lastest[pod.df$region=="uss"])~as.factor(pod.df$period[pod.df$region=="uss"]), xlab="Period", ylab="Estimate of last obs (doy) in USS", main="Last obs")
last.t.uss<-t.test(as.numeric(pod.df$lastest[pod.df$region=="uss"])~as.factor(pod.df$period[pod.df$region=="uss"]), conf.level=0.5)
#mtext(paste("Change=",-1*round(t$estimate[1]-t$estimate[2], digits=1),"(",-1*round(t$conf.int[1],digits=1),",",-1*round(t$conf.int[2],digits=1),")", sep=""),side=1,line=-3, adj=1)

#create a vector with the differences, like in the simulation code:
firstdif.uss<-first.t.uss$estimate[2]-first.t.uss$estimate[1]
firstdifp.uss<-first.t.uss$p.value
lastdif.uss<-last.t.uss$estimate[2]-last.t.uss$estimate[1]
lastdifp.uss<-last.t.uss$p.value

change.df<-as.data.frame(rbind(c("SRs","ps",mean(as.numeric(pod.df$nobs[pod.df$region=="ps"], na.rm=TRUE)),NA,firstdif.ps, first.t.ps$conf.int[1], first.t.ps$conf.int[2],lastdif.ps,last.t.ps$conf.int[1], last.t.ps$conf.int[2]),
                               c("SRs","uss",mean(as.numeric(pod.df$nobs[pod.df$region=="uss"], na.rm=TRUE)),NA,firstdif.uss, first.t.uss$conf.int[1], first.t.uss$conf.int[2],lastdif.uss, last.t.uss$conf.int[1], last.t.uss$conf.int[2])))
colnames(change.df)<-c("pod","region","nobs","prob","first.dif","first.lci","first.uci","last.dif","last.lci","last.uci")

