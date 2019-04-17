
#Add a column for presences (1/0) for each pod, for Ts, and for SRKWs
d$J<-0
d$J[grep("J",d$Pod.cl)]<- 1
d$K<-0
d$K[grep("K",d$Pod.cl)]<- 1
d$L<-0
d$L[grep("L",d$Pod.cl)]<- 1
d$SRKW<-0
d$SRKW[grep("SR",d$Pod.cl)]<- 1
d$SRKW[d$J==1|d$K==1|d$L==1]<- 1   
d$Orcas<-1
d$Trans<-0
d$Trans[grep("T",d$Pod.cl)]<- 1

# Add a column that combines :
#1) day, year, and region; 
#2) day, year, and fishing area 
#3) week, year, and region; 
#4) week, year, and fishing are; 
#to use to total up observations for later analyses 
d$yrdayregion<-paste(d$Year, d$day,d$region,sep="_")
d$yrwkregion<-paste(d$Year, d$week,d$region,sep="_")
d$yrdayfa<-paste(d$Year, d$day,d$FishArea,sep="_")
d$yrwkfa<-paste(d$Year, d$week,d$FishArea,sep="_")
d$yrregion<-paste(d$Year,d$region,sep="_")
d$yrfa<-paste(d$Year, d$FishArea,sep="_")

#Plot a bunch of different things:
#1. Plot the total number of orca observations per year in each region since 1978
#a. All pods together
#b .Each pod separately
obs = aggregate(Orcas ~yrregion, data = d,sum)
js = aggregate(J ~yrregion, data = d,sum)
ks = aggregate(K~yrregion, data = d,sum)
ls = aggregate(L~yrregion, data = d,sum)
srs = aggregate(SRKW~yrregion, data = d,sum)
orcasum<-cbind(js,ks[,2],ls[,2],srs[,2],obs[2])
colnames(orcasum)[2:6]<-c("Jobs","Kobs","Lobs","AllSRobs","AllOrcas")
orcasum$year<-substr(orcasum$yrregion,1,4)
orcasum$region<-substr(orcasum$yrregion,6,nchar(orcasum$yrregion))

# quartz()
# #par(mfrow)
# #start with uss, all SRKWs
# plot(orcasum$year[orcasum$region=="uss"],orcasum$AllSRobs[orcasum$region=="uss"],type="l",xlab="Year",ylab="Number of detections", lwd=2,bty="l", main="All SRKW sightings, 1978-2017")
# lines(orcasum$year[orcasum$region=="ps"],orcasum$AllSRobs[orcasum$region=="ps"], lwd=2,lty=2)
# lines(orcasum$year[orcasum$region=="oc"],orcasum$AllSRobs[orcasum$region=="oc"], lwd=2,lty=3)
# lines(orcasum$year[orcasum$region=="uss"],orcasum$Jobs[orcasum$region=="uss"],lwd=2,col="blue")
# lines(orcasum$year[orcasum$region=="ps"],orcasum$Jobs[orcasum$region=="ps"], lwd=2,lty=2, col="blue")
# lines(orcasum$year[orcasum$region=="oc"],orcasum$Jobs[orcasum$region=="oc"], lwd=2,lty=3, col="blue")
# lines(orcasum$year[orcasum$region=="uss"],orcasum$Kobs[orcasum$region=="uss"],lwd=2,col="purple")
# lines(orcasum$year[orcasum$region=="ps"],orcasum$Kobs[orcasum$region=="ps"], lwd=2,lty=2, col="purple")
# lines(orcasum$year[orcasum$region=="oc"],orcasum$Kobs[orcasum$region=="oc"], lwd=2,lty=3, col="purple")
# lines(orcasum$year[orcasum$region=="uss"],orcasum$Lobs[orcasum$region=="uss"],lwd=2,col="darkred")
# lines(orcasum$year[orcasum$region=="ps"],orcasum$Lobs[orcasum$region=="ps"], lwd=2,lty=2, col="darkred")
# lines(orcasum$year[orcasum$region=="oc"],orcasum$Lobs[orcasum$region=="oc"], lwd=2,lty=3, col="darkred")
# 
# legend("topleft",legend=c("Upper Salish Sea","Puget Sound","Outer Coast","Js","Ks","Ls"), lty=c(1,2,3,1,1,1),col=c("black","black","black","blue","purple","darkred"), bty="n")
# #legend("topleft",legend=c("Upper Salish Sea","Puget Sound","Outer Coast"), lty=c(1,2,3),col=c("black"), bty="n")
# 
# 
# quartz()
# par(mfrow=c(3,1))
# #start with uss, all SRKWs
# plot(orcasum$year[orcasum$region=="uss"],orcasum$Jobs[orcasum$region=="uss"],type="l",xlab="Year",ylab="Number of detections", lwd=2,bty="l", main="J pod sightings, 1978-2017", col="blue")
# lines(orcasum$year[orcasum$region=="ps"],orcasum$Jobs[orcasum$region=="ps"], lwd=2,lty=2, col="blue")
# lines(orcasum$year[orcasum$region=="oc"],orcasum$Jobs[orcasum$region=="oc"], lwd=2,lty=3, col="blue")
# 
# legend("topleft",legend=c("uss","ps","oc"), col="blue", lty=c(1,2,3), bty="n")
# #Kpod
# plot(orcasum$year[orcasum$region=="uss"],orcasum$Kobs[orcasum$region=="uss"],type="l",xlab="Year",ylab="Number of detections", lwd=2,bty="l", main="K pod sightings, 1978-2017", col="purple")
# lines(orcasum$year[orcasum$region=="ps"],orcasum$Kobs[orcasum$region=="ps"], lwd=2,lty=2, col="purple")
# lines(orcasum$year[orcasum$region=="oc"],orcasum$Kobs[orcasum$region=="oc"], lwd=2,lty=3, col="purple")
# #Lpod
# plot(orcasum$year[orcasum$region=="uss"],orcasum$Lobs[orcasum$region=="uss"],type="l",xlab="Year",ylab="Number of detections", lwd=2,bty="l", main="K pod sightings, 1978-2017", col="darkred")
# lines(orcasum$year[orcasum$region=="ps"],orcasum$Lobs[orcasum$region=="ps"], lwd=2,lty=2, col="darkred")
# lines(orcasum$year[orcasum$region=="oc"],orcasum$Lobs[orcasum$region=="oc"], lwd=2,lty=3, col="darkred")

#2. Plot the number of "whale days" (days on which whales were observed in each region)
#a. All pods together
#b .Each pod separately
obs.days = aggregate(Orcas ~yrdayregion, data = d,sum)
js.days = aggregate(J ~yrdayregion, data = d,sum)
ks.days = aggregate(K~yrdayregion, data = d,sum)
ls.days = aggregate(L~yrdayregion, data = d,sum)
srs.days = aggregate(SRKW~yrdayregion, data = d,sum)
orcasum.days<-cbind(js.days,ks.days[,2],ls.days[,2],srs.days[,2],obs.days[2])

colnames(orcasum.days)[2:6]<-c("Jobs","Kobs","Lobs","AllSRobs","AllOrcas")
orcasum.days$year<-substr(orcasum.days$yrdayregion,1,4)
orcasum.days$day<-substr(orcasum.days$yrdayregion,6,8)
orcasum.days$region<-substr(orcasum.days$yrdayregion,10,nchar(orcasum.days$yrdayregion))
orcasum.days$Jpres<-orcasum.days$Jobs
orcasum.days$Jpres[orcasum.days$Jobs>0]<-1
orcasum.days$Kpres<-orcasum.days$Kobs
orcasum.days$Kpres[orcasum.days$Kobs>0]<-1
orcasum.days$Lpres<-orcasum.days$Lobs
orcasum.days$Lpres[orcasum.days$Lobs>0]<-1
orcasum.days$AllSRpres<-orcasum.days$AllSRobs
orcasum.days$AllSRpres[orcasum.days$AllSRobs>0]<-1


#Add date information
orcasum.days$date<-as.Date(orcasum.days$day, format="%j",origin = paste(as.numeric(orcasum.days$year)-1,"12-31", sep="-"))
orcasum.days$date[orcasum.days$day==366]<-as.Date(paste(as.numeric(orcasum.days$year[orcasum.days$day==366]),"12-31", sep="-"))
#for some reason this gets the year wrong- replaces with current year
orcasum.days$date<-paste(orcasum.days$year,substr(orcasum.days$date, 6,10), sep="-")

orcasum.days$mon<-substr(orcasum.days$date,6,7)
#orcasum.days$date-as.Date(paste(orcasum.days$year,"06-30", sep="-"))
#head(cbind(orcasum.days$date,as.Date(paste(orcasum.days$year,"06-30", sep="-")),orcasum.days$date-as.Date(paste(orcasum.days$year,"06-30", sep="-"))))
#Add days after June 30:
orcasum.days$day<-as.numeric(orcasum.days$day)

orcasum.days$daysaftapr30<-difftime(as.Date(orcasum.days$date), as.Date(paste(orcasum.days$year,"04-30", sep="-")),units=c("days")) 
orcasum.days$daysaftapr30[which(orcasum.days$daysaftapr30<0)]<-difftime(as.Date(orcasum.days$date[which(orcasum.days$daysaftapr30<0)]), as.Date(paste(as.numeric(orcasum.days$year[which(orcasum.days$daysaftapr30<0)])-1,"04-30", sep="-")),units=c("days")) 

#add an "orca year" which runs may 1-april 30
orcasum.days$orcayear<-orcasum.days$year
orcasum.days$orcayear[which(orcasum.days$day>120)]<-as.numeric(orcasum.days$year[which(orcasum.days$day>120)])+1
#currently june 30 = 0 but we want it to be 365 or 366 (depending if leap year or not). correct this
#leap years are 1976, 1980, 1984, 1992, 200, 2004, 2008, 2012, 2016
#orcasum.days$daysaftapr30[which(orcasum.days$daysaftapr30=="0")]<-366
#rcasum.days$daysaftapr30[which(orcasum.days$daysaftapr30=="0" & orcasum.days$year=="1980")]<-366
#orcasum.days$daysaftapr30[which(orcasum.days$daysaftapr30=="0" & orcasum.days$year=="1984")]<-366
#orcasum.days$daysaftapr30[which(orcasum.days$daysaftapr30=="0" & orcasum.days$year=="1988")]<-366
#orcasum.days$daysaftapr30[which(orcasum.days$daysaftapr30=="0" & orcasum.days$year=="1992")]<-366
#orcasum.days$daysaftapr30[which(orcasum.days$daysaftapr30=="0" & orcasum.days$year=="1996")]<-366
#orcasum.days$daysaftapr30[which(orcasum.days$daysaftapr30=="0" & orcasum.days$year=="2000")]<-366
#orcasum.days$daysaftapr30[which(orcasum.days$daysaftapr30=="0" & orcasum.days$year=="2004")]<-366
#orcasum.days$daysaftapr30[which(orcasum.days$daysaftapr30=="0" & orcasum.days$year=="2008")]<-366
#orcasum.days$daysaftapr30[which(orcasum.days$daysaftapr30=="0" & orcasum.days$year=="2012")]<-366
#orcasum.days$daysaftapr30[which(orcasum.days$daysaftapr30=="0" & orcasum.days$year=="2016")]<-366
orcasum.days$daysaftapr30[which(orcasum.days$daysaftapr30=="0")]<-365
#the above needs to be fixed to deal with leap year

#check that daysafterapril 30 is working
presapr30<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region, orcasum.days$orcayear),sum)

#Ok, now look at proportion of whale days per week by season and region and by decade

orcasum.days$week<-strftime(strptime(orcasum.days$date,format= "%Y-%m-%d"), format = "%V")#new weeks start on mondays
orcasum.days$decade<-NA
orcasum.days$decade[orcasum.days$year<1987 & orcasum.days$year>1976]<-"1977-1986"
orcasum.days$decade[orcasum.days$year<1997 & orcasum.days$year>1986]<-"1987-1996"
orcasum.days$decade[orcasum.days$year<2007 & orcasum.days$year>1996]<-"1997-2006"
orcasum.days$decade[orcasum.days$year<2018 & orcasum.days$year>2006]<-"2007-2017"
orcasum.days<-orcasum.days[!is.na(orcasum.days$decade),]


