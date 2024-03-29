limed<-d[d$Source=="TWM-Otis",]
limed<-limed[which(!is.na(limed$SightDate)),]

obs.days.lime = aggregate(Obs ~yrdayfa, data = limed,sum)
js.days.lime = aggregate(J ~yrdayfa, data = limed,sum)
ks.days.lime = aggregate(K~yrdayfa, data = limed,sum)
ls.days.lime = aggregate(L~yrdayfa, data = limed,sum)
srs.days.lime = aggregate(SRKW~yrdayfa, data = limed,sum)
orcasum.days.lime<-cbind(js.days.lime,ks.days.lime[,2],ls.days.lime[,2],srs.days.lime[,2],obs.days.lime[2])

colnames(orcasum.days.lime)[2:6]<-c("Jobs","Kobs","Lobs","AllSRobs","AllObs")
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
#hist(as.numeric(orcasum.days.lime$daysaftmar31[orcasum.days.lime$fa=="07"]))
#mean(as.numeric(orcasum.days.lime$daysaftmar31[orcasum.days.lime$fa=="07"]), na.rm=TRUE)

regions=unique(orcasum.days$region)
podcols<-c("Jpres","Kpres","Lpres","AllSRpres")
pods<-c("J", "K","L","SRs")

#aggregate whale days

#first add zeros

limeyears<-unique(orcasum.days.lime$year)

alllimeabs<-c()

for(y in limeyears){
  days<-seq(from=min(orcasum.days.lime$day), to =as.numeric(max(limed$day)), by=1)

  abs<-rep(0,times=length(days))
  years<-rep(y, times=length(days))
  limeabs<-cbind(years,days,abs)
  alllimeabs<-rbind(alllimeabs,limeabs)
}
alllimeabs<-as.data.frame(alllimeabs)
limewdayspres<-subset(orcasum.days.lime, select=c(year,day,AllSRpres,Jpres,Kpres,Lpres))
colnames(alllimeabs)[1:2]<-colnames(limewdayspres)[1:2]
alllimeabs$day<-as.numeric(alllimeabs$day)
alllimeabs$abs<-as.numeric(alllimeabs$abs)

limewdaysabs<-left_join(alllimeabs,limewdayspres)
#replace NAs with 0
limewdaysabs$AllSRpres[which(is.na(limewdaysabs$AllSRpres))]<-0
limewdaysabs$Jpres[which(is.na(limewdaysabs$Jpres))]<-0
limewdaysabs$Kpres[which(is.na(limewdaysabs$Kpres))]<-0
limewdaysabs$Lpres[which(is.na(limewdaysabs$Lpres))]<-0

write.csv(limewdaysabs,"analyses/output/limedat.csv", row.names = FALSE)
#Make a plot of lime kiln whale days by year
yearsum<-aggregate(limewdaysabs$AllSRpres,by=list(limewdaysabs$year),sum)
jsum<-aggregate(limewdaysabs$Jpres,by=list(limewdaysabs$year),sum)
ksum<-aggregate(limewdaysabs$Kpres,by=list(limewdaysabs$year),sum)
lsum<-aggregate(limewdaysabs$Lpres,by=list(limewdaysabs$year),sum)
colnames(yearsum)<-colnames(jsum)<-colnames(ksum)<-colnames(lsum)<-c("year","wdays")
if(plotPNG == TRUE){png(filename="analyses/orcaphen/figures/whaledays_lime.png",height=480,width=960)}
if(plotPNG == FALSE){pdf("analyses/orcaphen/figures/whaledays_lime.pdf",height=5,width=15)}

#windows(height=6,width=12)
par(mfrow=c(1,4))
plot(as.numeric(yearsum$year),yearsum$wdays,ylab= "Year", xlab= "Number of Whale Days", bty="l", type="l", col="darkblue",lwd=2,main = "All Pods")
mtext("A)", side = 3, line = 1, adj=0)
plot(as.numeric(jsum$year),jsum$wdays,ylab= "Year", xlab= "Number of Whale Days", bty="l", type="l", col="darkblue",lwd=2, main = "J Pod")
mtext("B)", side = 3, line = 1, adj=0)

plot(as.numeric(ksum$year),ksum$wdays,ylab= "Year", xlab= "Number of Whale Days", bty="l", type="l", col="darkblue",lwd=2, main = "K Pod")
mtext("C)", side = 3, line = 1, adj=0)

plot(as.numeric(lsum$year),lsum$wdays,ylab= "Year", xlab= "Number of Whale Days", bty="l", type="l", col="darkblue",lwd=2, main = "L Pod")
mtext("D)", side = 3, line = 1, adj=0)

dev.off()

