
if(substr(getwd(),8,21)=="aileneettinger"){quartz()} else{windows()}
 par(mai=c(1,1,1,1.5))
 #start with uss, all SRKWs
 plot(rownames(wdays),wdays$uss,type="l",xlab="Year",ylab="Number of whale days", lwd=2,bty="l", main="All SRKW sightings, 1978-2017", ylim=c(0,300))
 lines(rownames(wdays),wdays$ps, lwd=2,lty=2)
 lines(rownames(wdays),wdays$oc, lwd=2,lty=3)
 legend("topleft",legend=c("Upper Salish Sea","Puget Sound","Outer Coast"), lty=c(1,2,3),col="black", bty="n")
 #Add dates of some significant events to the plot
 #"Dyes inlet event" 1997
 abline(v=1997, col="blue")
 text("Dyes inlet event",x=2000,y=200, col="blue",cex=0.8 )
# #Internet sightsing
 abline(v=2001, col="purple")
 text("Internet sightings added",x=2008,y=190, col="purple",cex=0.8 )

#Is there a trend in number of days on which whales observed since 1978?

 m.uss<-lm(wdays$uss~as.numeric(rownames(wdays)))
 m.ps<-lm(wdays$ps~as.numeric(rownames(wdays)))
 m.oc<-lm(wdays$oc~as.numeric(rownames(wdays)))
 abline(m.uss, col="darkred", lwd=2)
 abline(m.ps, col="darkred", lwd=2, lty=2)
 abline(m.oc, col="darkred", lwd=2, lty=3)
 mtext(paste((round(m.uss$coef[2], digits=2)*10),"days/dec"), line=-5.5, adj=1.3,cex=0.9)
 mtext(paste((round(m.ps$coef[2], digits=2)*10),"days/dec"), line=-20.5, adj=1.3,cex=0.9)
 mtext(paste((round(m.oc$coef[2], digits=2)*10),"days/dec"), line=-23.5, adj=1.3,cex=0.9)

 if(substr(getwd(),8,21)=="aileneettinger"){quartz()} else{windows()}
 par(mfrow=c(3,1))#par(mfrow)
 #J pod
 plot(rownames(wdays.J),wdays.J$uss,type="l",xlab="Year",ylab="Number of whale days", lwd=2,bty="l", main="J sightings, 1978-2017", ylim=c(0,200), col="blue")
 lines(rownames(wdays.J),wdays.J$ps, lwd=2,lty=2, col="blue")
 lines(rownames(wdays.J),wdays.J$oc, lwd=2,lty=3, col="blue")
 legend("topleft",legend=c("Upper Salish Sea","Puget Sound","Outer Coast"), lty=c(1,2,3),col="blue", bty="n")

 #K
 plot(rownames(wdays.K),wdays.K$uss,type="l",xlab="Year",ylab="Number of whale days", lwd=2,bty="l", main="K sightings, 1978-2017", ylim=c(0,200), col="purple")
 lines(rownames(wdays.K),wdays.K$ps, lwd=2,lty=2, col="purple")
 lines(rownames(wdays.K),wdays.K$oc, lwd=2,lty=3, col="purple")

 #L
 plot(rownames(wdays.L),wdays.L$uss,type="l",xlab="Year",ylab="Number of whale days", lwd=2,bty="l", main="L sightings, 1978-2017", ylim=c(0,200), col="darkred")
 lines(rownames(wdays.L),wdays.L$ps, lwd=2,lty=2, col="darkred")
 lines(rownames(wdays.L),wdays.L$oc, lwd=2,lty=3, col="darkred")


#Plots observed or not, by doy and year
#yaxis is year
#xaxis is doy
#do for 3 regions
podcols<-c("Jpres", "Kpres", "Lpres", "AllSRpres")
pods<-c("J","K","L","SRs")
for(p in 1:length(podcols)){
  if(substr(getwd(),8,21)=="aileneettinger"){quartz()} else{windows()}
  par(omi=c(.5,2,.5,.5), mfrow=c(1,3))
  colnum<-which(colnames(orcasum.days)==podcols[p])
  regions=unique(orcasum.days$region)
  for(r in regions){
    regdat<-orcasum.days[orcasum.days$region==r,]
    years = unique(orcasum.days$year)
    plot(0, type = 'n', las=1, xlim=c(1,366),ylim=c(min(as.numeric(years)),max(as.numeric(years))),ylab="",xlab="Day of year", main=paste(r), cex.axis=1.1, cex.lab=1.3)
    for(y in years){
      yrdat = regdat[regdat$year==y,]
      days = yrdat$day[yrdat[,colnum]==1]
      points(x=days,y=rep(y,length=length(days)), pch=21,bg="gray", cex=1.3)
      
      #lines(x=days,y=rep(y,length=length(days)), lwd=2)
    }  
    if(r=="uss"){mtext(paste(pods[p]), side=3,line=3, adj=0.5)}
    if(r=="ps"){mtext("Year", side=2,line=4, adj=0.5)}
    
  }
}



# #summary of the number of days whales were observed in each region, by year:
# pres<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region, orcasum.days$year),sum)
# #summary of the number of days whales with rows of data (observed or not) in each region, by year:
# totobs<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region, orcasum.days$year),length)
# prob<-pres/totobs
# #summary of the number of days whales were observed in each region, across all years:
# pres.doy<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region, orcasum.days$day),sum)
# #summary of the number of days whales with rows of data (observed or not) in each region, across all tears:
# totobs.doy<-tapply(orcasum.days$AllSRpres,list(orcasum.days$region, orcasum.days$day),length)
# prob.doy<-pres.doy/totobs.doy
# quartz()
# par(mfrow=c(2,1))
# plot(colnames(prob.doy),prob.doy[3,], type="l", ylab="Probability of obs.",xlab="DOY", main="USS")
# plot(colnames(prob.doy),prob.doy[2,], type="l", ylab="Probability of obs.",xlab="DOY", main="PS")
# 
# #Make box plots to see central tendencies:
# # podcols<-c("Jpres", "Kpres", "Lpres", "AllSRpres")
# # pods<-c("J","K","L","SRs")
# # for(p in 1:length(podcols)){
# #   quartz(width=15,height=6)
# #   par(mfrow=c(1,3))
# #   colnum<-which(colnames(orcasum.days)==podcols[p])
# #   regions=unique(orcasum.days$region)
# #   for(r in regions){
# #     regdat<-orcasum.days[orcasum.days$region==r,]
# #     if (r=="ps"){
# #      regdat<-regdat[as.numeric(regdat$day)>273,]#look at data only after Sept 30 for PS
# #     }
# #         boxplot(as.numeric(regdat$day)~as.factor(regdat$year),
# #             horizontal=TRUE,las=1,
# #             ylab="year",xlab="DOY",main=paste(r))
# #     
# #     if(r=="uss"){mtext(paste(pods[p]), side=3,line=2, adj=0.5)}
# #     
# #   }
# # }
# # 
# 
# #number of whale days per week
# wdaysperwk<-aggregate(orcasum.days$AllSRpres,list(orcasum.days$region, orcasum.days$year,orcasum.days$week),sum)
# colnames(wdaysperwk)<-c("region","year","week","wdays")
# wdaysperwk<-wdaysperwk[order(wdaysperwk$region,wdaysperwk$year,wdaysperwk$week),]
# wdaysperwk$decade<-NA
# wdaysperwk$decade[wdaysperwk$year<1987 & wdaysperwk$year>1976]<-"1977-1986"
# wdaysperwk$decade[wdaysperwk$year<1997 & wdaysperwk$year>1986]<-"1987-1996"
# wdaysperwk$decade[wdaysperwk$year<2007 & wdaysperwk$year>1996]<-"1997-2006"
# wdaysperwk$decade[wdaysperwk$year<2018 & wdaysperwk$year>2006]<-"2007-2017"
# #For this, remove all the 0s
# wdaysperwk$wdays[which(wdaysperwk$wdays==0)]<-NA
# wdaysperwk$prop<-wdaysperwk$wdays/7
# #plot lines by region and year
# yrs<-unique(wdaysperwk$year)
# 
# myPalette <- colorRampPalette(brewer.pal(9, "YlOrRd")) #### Gives us a heat map look
# cols = rev(myPalette(length(yrs)))
# mycols <- data.frame(cbind(sort(yrs),cols))
# colnames(mycols)[1]<-"yr"
# 
# # region<-unique(wdaysperwk$region)
# # for(r in 1:length(region)){
# #   regdat<-wdaysperwk[wdaysperwk$region==region[r],]
# #   quartz()
# #   par(mai=c(1,2,2,2))
# #   plot(regdat$week[regdat$year=="1977"],regdat$prop[regdat$year=="1977"],type="l",col=mycols$cols[mycols$yr==1977],xlab="week",ylab="Proportion whale days/week", main=paste(region[r]))
# #   for(i in 1:length(yrs)){
# #     lines(regdat$week[regdat$year==paste(yrs[i+1])],regdat$prop[regdat$year==paste(yrs[i+1])],col=mycols$cols[mycols$yr==yrs[i+1]])
# #   }
# # }
# # 
# # #now summarize proportions by decade
# # meanperweek<-aggregate(wdaysperwk$prop,list(wdaysperwk$region, wdaysperwk$decade,wdaysperwk$week),mean, na.rm=TRUE)
# # colnames(meanperweek)<-c("region","decade","week","prop.mn")
# # sdperweek<-aggregate(wdaysperwk$prop,list(wdaysperwk$region, wdaysperwk$decade,wdaysperwk$week),sd, na.rm=TRUE)
# # colnames(sdperweek)<-c("region","decade","week","prop.sd")
# # 
# # decs<-unique(meanperweek$decade)
# # 
# # cols = brewer.pal(4, "Set1")
# # mycols <- data.frame(cbind(sort(decs),cols))
# # colnames(mycols)[1]<-"dec"
# # quartz(height=6,width=12)
# # par(mai=c(.6,.6,1,.5),mfrow=c(1,3))
# # region<-unique(meanperweek$region)
# # reg.names=c("Puget Sound","Upper Salish Sea","Outer Coast")
# # for(r in 1:length(region)){
# #   regdat<-meanperweek[meanperweek$region==region[r],]
# #   
# #   plot(regdat$week[regdat$decade=="1977-1986"],cex.lab=1.2,regdat$prop.mn[regdat$decade=="1977-1986"],type="l",lwd=2,col=mycols$cols[mycols$dec=="1977-1986"],xlab="week",ylab="Proportion whale days/week", main=paste(reg.names[r]),ylim=c(0,1))
# #   for(i in 2:length(decs)){
# #     lines(regdat$week[regdat$decade==decs[i]],regdat$prop.mn[regdat$decade==decs[i]],col=mycols$cols[mycols$dec==decs[i]],lwd=2)
# #   }
# #   if(r==3){legend("topright", legend=mycols$dec, col=mycols$cols, lwd=2,lty=1)}
# # }
# # 
# # #now, across all decades
# # meanperweek2<-aggregate(wdaysperwk$prop,list(wdaysperwk$region, wdaysperwk$week),mean, na.rm=TRUE)
# # colnames(meanperweek2)<-c("region","week","prop.mn")
# # sdperweek2<-aggregate(wdaysperwk$prop,list(wdaysperwk$region, wdaysperwk$week),sd, na.rm=TRUE)
# # colnames(sdperweek2)<-c("region","week","prop.sd")
# # 
# # quartz(height=6,width=12)
# # par(mai=c(.6,.6,1,.5), mfrow=c(1,3))
# # region<-unique(meanperweek2$region)
# # reg.names=c("Outer Coast","Puget Sound","Upper Salish Sea")
# # for(r in 1:length(region)){
# #   regdat<-meanperweek2[meanperweek2$region==region[r],]
# #   
# #   plot(regdat$week,cex.lab=1.2,regdat$prop.mn,type="l",lwd=2,col="#E41A1C",xlab="week",ylab="Proportion whale days/week", main=paste(reg.names[r]),ylim=c(0,1))
# #   
# # }
# # 
# # 
# # #next thing to try: standardize the proportions- this will make seasonal patterns more clear i think
# # #now, across all decades
# # #mean proportion by region
# # reg.means<-tapply(meanperweek2$prop.mn, meanperweek2$region, mean, na.rm=TRUE)
# # reg.sds<-tapply(meanperweek2$prop.mn, meanperweek2$region, sd,na.rm=TRUE)
# # 
# # reg.means<-as.data.frame(cbind(names(reg.means),reg.means))
# # reg.sds<-as.data.frame(cbind(names(reg.sds),reg.sds))
# # colnames(reg.sds)[1]<-colnames(reg.means)[1]<-"region"
# # meanperweek3<-left_join(meanperweek2,reg.means, by="region", copy=TRUE)
# # meanperweek4<-left_join(meanperweek3,reg.sds, by="region", copy=TRUE)
# # 
# # meanperweek4$prop.mn.std<-(as.numeric(meanperweek4$prop.mn)-as.numeric(meanperweek4$reg.means))/as.numeric(meanperweek4$reg.sds)
# # 
# # quartz(height=6,width=12)
# # par(mai=c(.6,.6,1,.5), mfrow=c(1,3))
# # region<-unique(meanperweek4$region)
# # reg.names=c("Outer Coast","Puget Sound","Upper Salish Sea")
# # for(r in 1:length(region)){
# #   regdat<-meanperweek4[meanperweek3$region==region[r],]
# #   
# #   plot(regdat$week,cex.lab=1.2,regdat$prop.mn.std,type="l",lwd=2,col="#E41A1C",xlab="week",ylab="Standardized proportion whale days/week", main=paste(reg.names[r]))
# #   abline(h=0, lty=2, lwd=2)
# # }
# # 
