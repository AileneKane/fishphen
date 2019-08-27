#Run simple linear models on srkw data
#Add a column for presences (1/0) for each pod, for Ts, and for SRKWs
searsd$J<-0
searsd$J[grep("J",searsd$Pod.cl)]<- 1
searsd$K<-0
searsd$K[grep("K",searsd$Pod.cl)]<- 1
searsd$L<-0
searsd$L[grep("L",searsd$Pod.cl)]<- 1
searsd$SRKW<-0
searsd$SRKW[grep("SR",searsd$Pod.cl)]<- 1
searsd$SRKW[searsd$J==1|searsd$K==1|searsd$L==1]<- 1
searsd$Orcas<-1
searsd$Trans<-0
searsd$Trans[grep("T",searsd$Pod.cl)]<- 1

#check some things
table(searsd$year,searsd$FishArea)
table(searsd$year,searsd$day)
#see if there is more than one observation per day per area 
tapply(searsd$day[searsd$FishArea=="10"],list(searsd$year[searsd$FishArea=="10"],searsd$day[searsd$FishArea=="10"]),length)
#get whale days
searsd$yrdayfa<-paste(searsd$year, searsd$day,searsd$FishArea,sep="_")
obs.days.sears = aggregate(Orcas ~yrdayfa, data = searsd,sum)
js.days.sears = aggregate(J ~yrdayfa, data = searsd,sum)
ks.days.sears = aggregate(K~yrdayfa, data = searsd,sum)
ls.days.sears = aggregate(L~yrdayfa, data = searsd,sum)
srs.days.sears = aggregate(SRKW~yrdayfa, data = searsd,sum)
orcasum.days.sears<-cbind(js.days.sears,ks.days.sears[,2],ls.days.sears[,2],srs.days.sears[,2],obs.days.sears[2])

colnames(orcasum.days.sears)[2:6]<-c("Jobs","Kobs","Lobs","AllSRobs","AllOrcas")
orcasum.days.sears$year<-substr(orcasum.days.sears$yrdayfa,1,4)
orcasum.days.sears$day<-substr(orcasum.days.sears$yrdayfa,6,8)
orcasum.days.sears$fa<-substr(orcasum.days.sears$yrdayfa,10,nchar(orcasum.days.sears$yrdayfa))
orcasum.days.sears$Jpres<-orcasum.days.sears$Jobs
orcasum.days.sears$Jpres[orcasum.days.sears$Jobs>0]<-1
orcasum.days.sears$Kpres<-orcasum.days.sears$Kobs
orcasum.days.sears$Kpres[orcasum.days.sears$Kobs>0]<-1
orcasum.days.sears$Lpres<-orcasum.days.sears$Lobs
orcasum.days.sears$Lpres[orcasum.days.sears$Lobs>0]<-1
orcasum.days.sears$AllSRpres<-orcasum.days.sears$AllSRobs
orcasum.days.sears$AllSRpres[orcasum.days.sears$AllSRobs>0]<-1

orcasum.days.sears$date<-as.Date(orcasum.days.sears$day, format="%j",origin = paste(as.numeric(orcasum.days.sears$year)-1,"12-31", sep="-"))
orcasum.days.sears$date[orcasum.days.sears$day==366]<-as.Date(paste(as.numeric(orcasum.days.sears$year[orcasum.days.sears$day==366]),"12-31", sep="-"))
#for some reason this gets the year wrong- replaces with current year
orcasum.days.sears$date<-paste(orcasum.days.sears$year,substr(orcasum.days.sears$date, 6,10), sep="-")

orcasum.days.sears$mon<-substr(orcasum.days.sears$date,6,7)

#Add days after March 31:
orcasum.days.sears$day<-as.numeric(orcasum.days.sears$day)

orcasum.days.sears$daysaftmar31<-difftime(as.Date(orcasum.days.sears$date), as.Date(paste(orcasum.days.sears$year,"03-31", sep="-")),units=c("days")) 
orcasum.days.sears$daysaftmar31[which(orcasum.days.sears$daysaftmar31<0)]<-difftime(as.Date(orcasum.days.sears$date[which(orcasum.days.sears$daysaftmar31<0)]), as.Date(paste(as.numeric(orcasum.days.sears$year[which(orcasum.days.sears$daysaftmar31<0)])-1,"03-31", sep="-")),units=c("days")) 

#add an "orca year" which runs apr1-mar 31
orcasum.days.sears$orcayear<-orcasum.days.sears$year
orcasum.days.sears$orcayear[which(orcasum.days.sears$day>90)]<-as.numeric(orcasum.days.sears$year[which(orcasum.days.sears$day>90)])-1
#leap years are 1976, 1980, 1984, 1992, 2000, 2004, 2008, 2012, 2016
#=120.6905

orcasum.days.sears$daysaftmar31[which(orcasum.days.sears$daysaftmar31=="0")]<-366


#check that daysaftermar31 is working
presapr1<-tapply(orcasum.days.sears$AllSRpres,list(orcasum.days.sears$fa, orcasum.days.sears$orcayear),sum)

#Check:
hist(as.numeric(orcasum.days.sears$daysaftmar31[orcasum.days.sears$fa=="10"]))
mean(as.numeric(orcasum.days.sears$daysaftmar31[orcasum.days.sears$fa=="10"]), na.rm=TRUE)

#Make plots
sears091011<-orcasum.days.sears[orcasum.days.sears$fa=="10"|orcasum.days.sears$fa=="11"|orcasum.days.sears$fa=="09",]
searsps<-orcasum.days.sears[orcasum.days.sears$fa!="07",]
unique(searsps$fa)

  #pdf(figname,height=6, width=6)
  quartz(height=6, width=15)
 par(mfrow=c(1,3))
podcols<-c("AllSRpres")
pods<-c("SRs")
for(p in 1:length(podcols)){
  colnum<-which(colnames(orcasum.days.sears)==podcols[p])
  #fas=sort(unique(orcasum.days.sears$fa),decreasing=TRUE)
  #fas = "10"
    regdat<-searsps
    years = sort(unique(orcasum.days.sears$orcayear))
    plot(0, type = 'n', las=1, xlim=c(1,366),ylim=c(min(as.numeric(years)),max(as.numeric(years))),ylab="",xlab="Month",xaxt='n', main="", cex.axis=1.1, cex.lab=1.3)
      col= "salmon"
      #using orcayears
      polygon(c(92,92,309,309),c(max(as.numeric(years))+1,min(as.numeric(years))-1,min(as.numeric(years))-1,max(as.numeric(years))+1),
              col=adjustcolor(col,alpha.f=0.2),
              border=NA)
    for(y in years){
      yrdat = regdat[regdat$orcayear==y,]
      days = yrdat$daysaftmar31[yrdat[,colnum]==1]
      
      points(x=days,y=rep(y,length=length(days)), pch=16,col= col, cex=1.3)
      #lines(x=days,y=rep(y,length=length(days)), lwd=2)
    }  
    
    axis(side=1,at = c(1,92,184,276,365), labels=c("1Apr","1Jul","1Oct","1Jan","31Mar"))
  }



quartz(height=6, width=15)
par(mfrow=c(1,3))

#Looks like continuous data from 2002 through 2018 (except for 2008 and 2014 (orcayear=2) which have very few sightings- why is this?) in FishAreas 10 and 11
years<-seq(2001,2017, by=1)#restrict to these years for orcayears
#years<-years[-which(years==2008)]
years<-years[-which(years==2013)]#only observed on 1 day inb
#Look at trends in SRKW phenology across all years, for all pods
years.all<-c()
nobs.all<-c()
firstest.all<-c()
mean.all<-c()
lastest.all<-c()
pods.all<-c()
lower.all<-c()
upper.all<-c()
for(y in 1:length(years)){
  yrdat<-searsps[searsps$orcayear==years[y],]
  years.all<-c(years.all,years[y])
  nobs.all<-c(nobs.all,length(yrdat$day[yrdat$AllSRobs==1]))
  firstest.all<-c(firstest.all,min(yrdat$daysaftmar31[yrdat$AllSRobs==1], na.rm=TRUE))
  lastest.all<-c(lastest.all,max(yrdat$daysaftmar31[yrdat$AllSRobs==1], na.rm=TRUE))
  mean.all<-c(mean.all,mean(yrdat$daysaftmar31[yrdat$AllSRobs==1], na.rm=TRUE))
  lower.all<-c(lower.all,quantile(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]),alpha))#lower ci
  upper.all<-c(upper.all,quantile(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]),1-alpha))#upper ci

  pods.all<-c(pods.all,pods[p])
  
}
sears.df <- as.data.frame(cbind(pods.all,years.all,nobs.all,firstest.all,lastest.all,mean.all))
colnames(sears.df)[1:3]<-c("pod","year","nobs")


#Now try for sears data
alpha=0.10
k=20
pods.all.p<-c()
regions.all.p<-c()
years.all.p<-c()
nobs.all.p<-c()
firstest.all.p<-c()
lastest.all.p<-c()
meandiffs.all<-c()
firstdiffs.all<-c()
years<-unique(sears.df$year)
p=1
for(p in 1:length(podcols)){
  colnum<-which(colnames(orcasum.days.lime)==podcols[p])
  
  regdat<-orcasum.days.sears
  for(y in 1:length(years)){
    yrdat<-regdat[regdat$orcayear==years[y],]
    pods.all.p<-c(pods.all.p,pods[p])
    years.all.p<-c(years.all.p,years[y])
    nobs.all.p<-c(nobs.all.p,length(yrdat$day[yrdat[,colnum]==1]))
    
    firstest.all.p<-rbind(firstest.all.p,est.limit(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]),alpha=alpha,k=k))
    lastest.all.p<-rbind(lastest.all.p,est.limit(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]), upper=TRUE,alpha=alpha,k=k))
    meandiffs.all<-c(meandiffs.all,mean(diff(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]))))
  }
}

sears.df <- cbind(sears.df,pods.all.p,years.all.p,nobs.all.p,firstest.all.p,lastest.all.p,mean.all.p)
colnames(sears.df)[10:18]<-c("firstest.p","firstest.plcl","firstest.pucl","lastest.p","lastest.plcl","lastest.pucl","mean.p","mean.plcl","mean.pucl")

sears.df$year<-as.numeric(sears.df$year)
sears.df$firstest.all<-as.numeric(sears.df$firstest.all)

quartz(height=8,width=25)
par(mfrow=c(1,6))

plot(sears.df$year,sears.df$firstest.all,xlab="year",ylab="first obs doy", main="", bty="l", pch=16, ylim=c(0,280))
mod<-lm(sears.df$firstest.all~sears.df$year)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]<.1){abline(mod, lty=3)}
mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
arrows(sears.df$year,sears.df$firstest.plcl,sears.df$year,sears.df$firstest.pucl,col="blue",code=3,length=0)
points(sears.df$year,sears.df$firstest.p,pch=16, col = "blue",cex=1.2)

points(sears.df$year,sears.df$firstest.p,pch=16, col= "blue") 
mod<-lm(sears.df$firstest.p~sears.df$year)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1, col="blue")}
if(summary(mod)$coef[2,4]<.1){abline(mod, lty=3, col="blue")}


plot(sears.df$year,sears.df$lastest.all,xlab="year",ylab="last obs doy", main="", bty="l", pch=16, ylim=c(140,365))
mod<-lm(sears.df$lastest.all~sears.df$year)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]<.1){abline(mod, lty=3)}
mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)

arrows(sears.df$year,sears.df$lastest.plcl,sears.df$year,sears.df$lastest.pucl,col="blue",code=3,length=0)
points(sears.df$year,sears.df$lastest.p,pch=16, col = "blue",cex=1.2)
mod<-lm(sears.df$lastest.p~sears.df$year)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1,col = "blue")}
if(summary(mod)$coef[2,4]<.1){abline(mod, lty=3,col = "blue")}


plot(sears.df$year,sears.df$mean.all,xlab="year",ylab="mean obs doy", main="", bty="l", pch=16)
mod<-lm(sears.df$mean.all~sears.df$year)
if(summary(mod)$coef[2,4]<.05){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]<.1){abline(mod, lty=3)}
arrows(sears.df$year,sears.df$mean.plcl,sears.df$year,sears.df$mean.pucl,code=3,length=0)

abline(mod)
mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
