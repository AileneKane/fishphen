
#Make plots
  #pdf(figname,height=6, width=6)
#   quartz(height=6, width=15)
#  par(mfrow=c(1,3))
# podcols<-c("AllSRpres")
# pods<-c("SRs")
# for(p in 1:length(podcols)){
#   colnum<-which(colnames(orcasum.days.lime)==podcols[p])
#   #fas=sort(unique(orcasum.days.lime$fa),decreasing=TRUE)
#   #fas = "10"
#     regdat<-orcasum.days.lime
#     years = sort(unique(orcasum.days.lime$orcayear))
#     plot(0, type = 'n', las=1, xlim=c(1,366),ylim=c(min(as.numeric(years)),max(as.numeric(years))),ylab="",xlab="Month",xaxt='n', main="", cex.axis=1.1, cex.lab=1.3)
#     col= "darkblue"
#     polygon(c(1,1,214,214),c(max(as.numeric(years))+1,min(as.numeric(years))-1,min(as.numeric(years))-1,max(as.numeric(years))+1),
#                 col=adjustcolor(col,alpha.f=0.2),
#                 border=NA)
#       for(y in years){
#       yrdat = regdat[regdat$orcayear==y,]
#       days = yrdat$daysaftmar31[yrdat[,colnum]==1]
#       
#       points(x=days,y=rep(y,length=length(days)), pch=16,col= col, cex=1.3)
#       #lines(x=days,y=rep(y,length=length(days)), lwd=2)
#     }  
#     
#     axis(side=1,at = c(1,92,184,276,365), labels=c("1Apr","1Jul","1Oct","1Jan","31Mar"))
#   }
# 
# 

pdf("analyses/orcaphen/figures/limekilntrends_dat.pdf",height=6, width=15)
#quartz(height=6, width=15)
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
plot(df$year[!df$year==1990],df$firstest.all[!df$year==1990],xlab="year",ylab="Day of Year", bty="l", pch=21, bg="darkblue", cex=1.1, main = "First observation day")
mod<-lm(df$firstest.all[!df$year==1990]~df$year[!df$year==1990])
if(summary(mod)$coef[2,4]<alph){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]>alph){abline(mod, lty=3)}
mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)

plot(df$year[!df$year==1990],df$lastest.all[!df$year==1990],xlab="year",ylab="Day of year", bty="l", pch=21, bg="darkblue", cex=1.1, main = "Last observation")
mod<-lm(df$lastest.all[!df$year==1990]~df$year[!df$year==1990])
if(summary(mod)$coef[2,4]<alph){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]>alph){abline(mod, lty=3)}
mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)

# plot(df$year,df$mean.all,xlab="year",ylab="mean obs doy", main="", bty="l", pch=21, bg="gray")
# mod<-lm(df$mean.all~df$year)
# abline(mod, lty=2)
# mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
# mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
# 


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
#p=1
for(p in 1:length(podcols)){
  colnum<-which(colnames(orcasum.days.lime)==podcols[p])
 
    regdat<-orcasum.days.lime
    for(y in 1:length(years)){
      yrdat<-regdat[regdat$orcayear==years[y],]
      pods.all<-c(pods.all,pods[p])
      years.all<-c(years.all,years[y])
      nobs.all<-c(nobs.all,length(yrdat$day[yrdat[,colnum]==1]))
     
      firstest.all<-c(firstest.all,min(yrdat$day[yrdat[,colnum]==1], na.rm=TRUE))
      lastest.all<-c(lastest.all,max(yrdat$day[yrdat[,colnum]==1], na.rm=TRUE))
      mean.all<-c(mean.all,mean(yrdat$day[yrdat[,colnum]==1], na.rm=TRUE))
      
    }
  }

lime.df <- as.data.frame(cbind(pods.all,years.all,nobs.all,firstest.all,lastest.all,mean.all))
colnames(lime.df)[1:3]<-c("pod","year","nobs")


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
                        quantile(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]),alph),#lower ci
                        quantile(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]),1-alph)))#upper ci
    meandiffs.all<-c(meandiffs.all,mean(diff(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]))))
    firstdiffs.all<-c(firstdiffs.all,diff(as.integer(yrdat$daysaftmar31[yrdat[,colnum]==1]))[1])
    
  }
}

lime.df <- cbind(lime.df,pods.all.p,years.all.p,nobs.all.p,firstest.all.p,lastest.all.p,mean.all.p)

if(substr(getwd(),8,21)=="aileneettinger"){quartz()} else{windows()}
plot(lime.df$year[!lime.df$year==1990],lime.df$nobs[!lime.df$year==1990],type="p",pch=21, bg = "darkblue",xlab="Year",ylab="Number of days", cex=1.2, bty="l", ylim=c(1,70), main = "Whale Days")
lime.df$nobs<-as.numeric(lime.df$nobs)
lime.df$year<-as.numeric(lime.df$year)
mod<-lm(lime.df$nobs[!lime.df$year==1990]~lime.df$year[!lime.df$year==1990])
if(summary(mod)$coef[2,4]<alph){abline(mod, lty=1)}
if(summary(mod)$coef[2,4]>alph){abline(mod, lty=3)}
mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)

wdays.start<-lime.df$nobs[lime.df$year==1994]
wdays.end<-lime.df$nobs[lime.df$year==2017]
#calculate percent change:
(wdays.start-wdays.end)/wdays.start

#function to plot whale days
png(filename="analyses/orcaphen/figures/whaledays_lime.png",height=400,width=1200)
#pdf("analyses/orcaphen/figures/whaledays_lime.pdf",height=4,width=12)

#windows(height=6,width=12)
par(mfrow=c(1,4))
ps<-c("SRs","J","K","L")
lets<-c("A)","B)","C)","D)")
mains<-c("All Pods","J Pod","K Pod","L Pod")
for(i in 1:length(ps)){
  poddat<-lime.df[lime.df$pod==ps[i],]
  plot(poddat$year,poddat$nobs,xlab= "Year", ylab= "Number of Whale Days", bty="l", type="l", col="darkblue",xlim=c(1994,2017),lwd=2,main = paste(mains[i]))
  mtext(paste(lets[i]), side = 3, line = 1, adj=0)
}
dev.off()

#Plot trends in phenology over time at Lime Kiln
png(filename="analyses/orcaphen/figures/phentrends_lime.png",height=960,width=1280)
#windows(height=6,width=12)
par(mfcol=c(2,4))
ps<-c("SRs","J","K","L")
lets<-c("A)","B)","C)","D)","E)","F)","G)","H)")
mains<-c("All Pods","J Pod","K Pod","L Pod")

for(i in 1:length(ps)){
  poddat<-lime.df[lime.df$pod==ps[i],]
  plot(poddat$year,poddat$firstest.all,ylab= "Year", xlab= "Arrival Day Of Year", pch=21, bty="l",cex=1.5,bg="darkblue",main = paste(mains[i]))
  mtext(paste(lets[i]), side = 3, line = 1, adj=0)
  mod<-lm(poddat$firstest.all~poddat$year)
  if(summary(mod)$coef[2,4]<1-alph){abline(mod, lty=1, lwd=2)}
  mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
  
  plot(poddat$year,poddat$lastest.all,ylab= "Year", xlab= "Departure Day of Year", pch=21, bty="l",cex=1.5,bg="darkblue")
  mtext(paste(lets[i+3]), side = 3, line = 1, adj=0)
  mod<-lm(poddat$lastest.all~poddat$year)
  if(summary(mod)$coef[2,4]<1-alph){abline(mod, lty=1, lwd=2)}
  mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
  
}
dev.off()
png(filename="analyses/orcaphen/figures/phentrends_lime_2panels.png",height=480,width=720)
#windows(height=6,width=12)
par(mfcol=c(1,2))
ps<-c("SRs")
lets<-c("A)","B)")
mains<-c("All Pods","All Pod")
alph=0.95
for(i in 1:length(ps)){
  poddat<-lime.df[lime.df$pod==ps[i],]
  poddat$year<-as.numeric(poddat$year)
  poddat$firstest.all<-as.numeric(poddat$firstest.all)
  poddat$lastest.all<-as.numeric(poddat$lastest.all)
  plot(poddat$year,poddat$firstest.all,xlab= "Year", ylab= "Arrival Day Of Year", pch=21, bty="l",cex=1.5,bg="darkblue",main = paste(mains[i]))
  mtext(paste(lets[i]), side = 3, line = 1, adj=0)
  mod<-lm(poddat$firstest.all~poddat$year)
  if(summary(mod)$coef[2,4]<1-alph){abline(mod, lty=1, lwd=2)}
  mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
  
  plot(poddat$year,poddat$lastest.all,xlab= "Year", ylab= "Departure Day of Year", pch=21, bty="l",cex=1.5,bg="darkblue",main = paste(mains[i]))
  mtext(paste(lets[i+3]), side = 3, line = 1, adj=0)
  mod<-lm(poddat$lastest.all~poddat$year)
  if(summary(mod)$coef[2,4]<1-alph){abline(mod, lty=1, lwd=2)}
  mtext(paste("r2=",round(summary(mod)$r.squared, digits=2),",p=",round(summary(mod)$coeff[2,4], digits=2)), side=3, adj=1, cex=0.7)
  mtext(paste("coef=",round(summary(mod)$coeff[2,1], digits=2)), side=3,line=-1, adj=1, cex=0.7)
  
}
dev.off()
