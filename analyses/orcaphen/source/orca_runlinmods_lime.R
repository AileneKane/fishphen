obs.days.lime = aggregate(Orcas ~yrdayfa, data = limed,sum)
js.days.lime = aggregate(J ~yrdayfa, data = limed,sum)
ks.days.lime = aggregate(K~yrdayfa, data = limed,sum)
ls.days.lime = aggregate(L~yrdayfa, data = limed,sum)
srs.days.lime = aggregate(SRKW~yrdayfa, data = limed,sum)
orcasum.days.lime<-cbind(js.days.lime,ks.days.lime[,2],ls.days.lime[,2],srs.days.lime[,2],obs.days.lime[2])

colnames(orcasum.days.lime)[2:6]<-c("Jobs","Kobs","Lobs","AllSRobs","AllOrcas")
orcasum.days.lime$year<-substr(orcasum.days.lime$yrdayfa,1,4)
orcasum.days.lime$day<-substr(orcasum.days.lime$yrdayfa,6,8)
orcasum.days.lime$fa<-substr(orcasum.days.lime$yrdayfa,10,nchar(orcasum.days.lime$yrdayfa))
orcasum.days.lime$Jpres<-orcasum.days.lime$Jobs
orcasum.days.lime$Jpres[orcasum.days.lime$Jobs>0]<-1
orcasum.days.lime$Kpres<-orcasum.days.lime$Kobs
orcasum.days.lime$Kpres[orcasum.days.lime$Kobs>0]<-1
orcasum.days.lime$Lpres<-orcasum.days.lime$Lobs
orcasum.days.lime$Lpres[orcasum.days.lime$Lobs>0]<-1
orcasum.days.lime$AllSRpres<-orcasum.days.lime$AllSRobs
orcasum.days.lime$AllSRpres[orcasum.days.lime$AllSRobs>0]<-1

orcasum.days.lime$date<-as.Date(orcasum.days.lime$day, format="%j",origin = paste(as.numeric(orcasum.days.lime$year)-1,"12-31", sep="-"))
#orcasum.days.lime$date[orcasum.days.lime$day==366]<-as.Date(paste(as.numeric(orcasum.days.lime$year[orcasum.days.lime$day==366]),"12-31", sep="-"))
#for some reason this gets the year wrong- replaces with current year
orcasum.days.lime$date<-paste(orcasum.days.lime$year,substr(orcasum.days.lime$date, 6,10), sep="-")

orcasum.days.lime$mon<-substr(orcasum.days.lime$date,6,7)

#Add days after March 31:
orcasum.days.lime$day<-as.numeric(orcasum.days.lime$day)

orcasum.days.lime$daysaftmar31<-difftime(as.Date(orcasum.days.lime$date), as.Date(paste(orcasum.days.lime$year,"03-31", sep="-")),units=c("days")) 

#orcasum.days.lime$daysaftmar31[which(as.numeric(orcasum.days.lime$daysaftmar31)<0)]<-difftime(as.Date(orcasum.days.lime$date[which(as.numeric(orcasum.days.lime$daysaftmar31)<0)]), as.Date(paste(as.numeric(orcasum.days.lime$year[which(as.numeric(orcasum.days.lime$daysaftmar31)<0)])-1,"03-31", sep="-")),units=c("days")) 

#add an "orca year" which runs apr1-mar 31
orcasum.days.lime$orcayear<-orcasum.days.lime$year
orcasum.days.lime$orcayear[which(orcasum.days.lime$day>90)]<-as.numeric(orcasum.days.lime$year[which(orcasum.days.lime$day>90)])-1
#leap years are 1976, 1980, 1984, 1992, 2000, 2004, 2008, 2012, 2016
#=120.6905

#orcasum.days.lime$daysaftmar31[which(orcasum.days.lime$daysaftmar31=="0")]<-366


#check that daysaftermar31 is working
presapr1<-tapply(orcasum.days.lime$AllSRpres,list(orcasum.days.lime$fa, orcasum.days.lime$orcayear),sum)

#Check:
hist(as.numeric(orcasum.days.lime$daysaftmar31[orcasum.days.lime$fa=="07"]))
mean(as.numeric(orcasum.days.lime$daysaftmar31[orcasum.days.lime$fa=="07"]), na.rm=TRUE)

#Make plots
  #pdf(figname,height=6, width=6)
  quartz(height=6, width=15)
 par(mfrow(c(1,3)))
podcols<-c("AllSRpres")
pods<-c("SRs")
for(p in 1:length(podcols)){
  colnum<-which(colnames(orcasum.days.lime)==podcols[p])
  #fas=sort(unique(orcasum.days.lime$fa),decreasing=TRUE)
  #fas = "10"
    regdat<-orcasum.days.lime
    years = sort(unique(orcasum.days.lime$orcayear))
    plot(0, type = 'n', las=1, xlim=c(1,366),ylim=c(min(as.numeric(years)),max(as.numeric(years))),ylab="",xlab="Month",xaxt='n', main="", cex.axis=1.1, cex.lab=1.3)
    col= "darkblue"
    polygon(c(1,1,214,214),c(max(as.numeric(years))+1,min(as.numeric(years))-1,min(as.numeric(years))-1,max(as.numeric(years))+1),
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
years<-sort(unique(orcasum.days.lime$orcayear))#restrict to these years for orcayears
#Look at trends in SRKW phenology across all years, for all pods
years.all<-c()
nobs.all<-c()
firstest.all<-c()
mean.all<-c()
lastest.all<-c()

for(y in 1:length(years)){
  yrdat<-orcasum.days.lime[orcasum.days.lime$orcayear==years[y],]
  
  years.all<-c(years.all,years[y])
  nobs.all<-c(nobs.all,length(yrdat$day[yrdat$AllSRobs==1]))
  firstest.all<-c(firstest.all,min(yrdat$daysaftmar31[yrdat$AllSRobs==1], na.rm=TRUE))
  lastest.all<-c(lastest.all,max(yrdat$daysaftmar31[yrdat$AllSRobs==1], na.rm=TRUE))
  mean.all<-c(mean.all,mean(yrdat$daysaftmar31[yrdat$AllSRobs==1], na.rm=TRUE))
}
df <- as.data.frame(cbind(years.all,nobs.all,firstest.all,lastest.all,mean.all))
colnames(df)[1:2]<-c("year","nobs")
df$year<-as.numeric(df$year)
plot(df$year,df$firstest.all,xlab="year",ylab="first obs doy", main="", bty="l", pch=21, bg="gray")
mod<-lm(df$firstest.all~df$year)
abline(mod, lty=1)
mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)

plot(df$year,df$lastest.all,xlab="year",ylab="last obs doy", main="", bty="l", pch=21, bg="gray")
mod<-lm(df$lastest.all~df$year)
abline(mod, lty=1)
mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)

plot(df$year,df$mean.all,xlab="year",ylab="mean obs doy", main="", bty="l", pch=21, bg="gray")
mod<-lm(df$mean.all~df$year)
abline(mod, lty=2)
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
mean.all<-c()
#p=1
#r=1

#unique(orcasum.days$orcayear)#use may 1 as start of orca year, as this will encompass min start date window that i want to try
p=1
for(p in 1:length(podcols)){
  colnum<-which(colnames(orcasum.days.lime)==podcols[p])
 
    regdat<-orcasum.days.lime
    for(y in 1:length(years)){
      yrdat<-regdat[regdat$orcayear==years[y],]
      pods.all<-c(pods.all,pods[p])
      years.all<-c(years.all,years[y])
      nobs.all<-c(nobs.all,length(yrdat$day[yrdat[,colnum]==1]))
     
      firstest.all<-c(firstest.all,min(yrdat$daysaftmar31[yrdat[,colnum]==1], na.rm=TRUE))
      lastest.all<-c(lastest.all,max(yrdat$daysaftmar31[yrdat[,colnum]==1], na.rm=TRUE))
      mean.all<-c(mean.all,mean(yrdat$daysaftmar31[yrdat[,colnum]==1], na.rm=TRUE))
      
    }
  }

lime.df <- as.data.frame(cbind(pods.all,years.all,nobs.all,firstest.all,lastest.all,mean.all))
colnames(lime.df)[1:3]<-c("pod","year","nobs")
