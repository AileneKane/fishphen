#Make a map of observations of SRKW sightings in different regions

#Same figure but from April 1- MAr 31 and try boxplots
if(assumeSRKW==TRUE & use3regions==FALSE){figname<-paste("analyses/figures/OrcaPhenPlots/srkw_phenmap_assumeSRKW_April1.pdf")}
if(assumeSRKW==FALSE & use3regions==FALSE){figname<-paste("analyses/figures/OrcaPhenPlots/srkw_phenmap_April1.pdf")}
if(assumeSRKW==TRUE & use3regions==TRUE){figname<-paste("analyses/figures/OrcaPhenPlots/srkw_phenmap_assumeSRKW_3regs_April1.pdf")}
if(assumeSRKW==FALSE & use3regions==TRUE){figname<-paste("analyses/figures/OrcaPhenPlots/srkw_phenmap_3regs_April1.pdf")}

if(use3regions==FALSE){
  pdf(figname,height=12, width=6)
  #quartz(height=6, width=15)
  par(omi=c(.5,.5,.5,.5), mfrow=c(2,1))
}
if(use3regions==TRUE){
  pdf(figname,height=6, width=20)
  #quartz(height=6, width=20)
  par(omi=c(.5,.5,.5,.5), mfrow=c(1,4))
}

podcols<-c("AllSRpres")
pods<-c("SRs")
for(p in 1:length(podcols)){
  colnum<-which(colnames(orcasum.days)==podcols[p])
  regions=sort(unique(orcasum.days$region[-which(orcasum.days$region=="oc")]),decreasing=TRUE)
  for(r in regions){
    regdat<-orcasum.days[orcasum.days$region==r,]
    years = unique(orcasum.days$orcayear)
    plot(0, type = 'n', las=1, xlim=c(1,366),ylim=c(min(as.numeric(years)),max(as.numeric(years))),ylab="",xlab="Month",xaxt='n', main="", cex.axis=1.1, cex.lab=1.3)
    if(r=="uss"){
      col="darkblue"
      #using orcayears
      polygon(c(1,1,214,214),c(max(as.numeric(years))+1,min(as.numeric(years))-1,min(as.numeric(years))-1,max(as.numeric(years))+1),
              col=adjustcolor(col,alpha.f=0.2),
              border=NA)
    }
    if(r=="ps"){
      col="salmon"
      #using orcayears
      polygon(c(92,92,309,309),c(max(as.numeric(years))+1,min(as.numeric(years))-1,min(as.numeric(years))-1,max(as.numeric(years))+1),
              col=adjustcolor(col,alpha.f=0.2),
              border=NA)
    }
    if(r=="jf"){
      col="darkgreen"
      #using orcayears
      #polygon(c(1,1,214,214),c(max(as.numeric(years))+1,min(as.numeric(years))-1,min(as.numeric(years))-1,max(as.numeric(years))+1),
       #       col=adjustcolor(col,alpha.f=0.2),
        #      border=NA)
    } 
    for(y in years){
      yrdat = regdat[regdat$orcayear==y,]
      days = yrdat$daysaftmar31[yrdat[,colnum]==1]
      
      points(x=days,y=rep(y,length=length(days)), pch=16,col= col, cex=1.3)
      #lines(x=days,y=rep(y,length=length(days)), lwd=2)
    }  
    
    axis(side=1,at = c(1,92,184,276,365), labels=c("1Apr","1Jul","1Oct","1Jan","31Mar"))
    #if(r=="uss"){mtext(paste(pods[p]), side=3,line=3, adj=0.5)}
    if(r=="uss"){mtext("Year", side=2,line=4, adj=0.5)}
    
  }
}

if(use3regions==FALSE){dev.off()}
#summarize the days on which atleast one sighting occured for each lat/long in ps and uss
srlatlon = aggregate(d$SRKW, list(d$Lat,d$Long,d$region,d$Year,d$day),sum)
colnames(srlatlon)<-c("lat","lon","region","year","doy","numsights")
latlons<-srlatlon[srlatlon$region!="oc",]#remove outercoast\
latlons<-latlons[latlons$lat!="",]#remove blank lats
latlons<-latlons[latlons$lon!="0.0000",] #remove weird lon
latlons$pres<-0#add column for 0/1 abs/pres on a given day at the lat/long. 
latlons$pres[latlons$numsights>0]<-1
numdays<-aggregate(latlons$pres, list(latlons$lat,latlons$lon,latlons$region,latlons$year),sum)
colnames(numdays)<-c("lat","lon","region","year","numdays")
numdays<-numdays[numdays$numdays>0,]#remove zeros
meannumdays<-aggregate(numdays$numdays, list(numdays$lat,numdays$lon,numdays$region),mean)
colnames(meannumdays)<-c("lat","lon","region","mnnumdays")
#make a map that shows mean number of whale days by lat long in each region

newmap <- getMap(resolution = "low")

#quartz(height=12, width=6)
if(assumeSRKW==TRUE & use3regions==FALSE){mapname<-paste("analyses/figures/OrcaPhenPlots/srkw_justmap_assumeSRKW_April1.pdf")
    pdf(mapname,height=12, width=6)
}
plot(newmap, xlim = c(-126, -120), ylim = c(49.2, 51), asp = 1)
points(meannumdays$lon[meannumdays$region=="ps"],meannumdays$lat[meannumdays$region=="ps"],type="p",pch=16, col="salmon",cex=log(meannumdays$mnnumdays, base=10))
points(meannumdays$lon[meannumdays$region=="uss"],meannumdays$lat[meannumdays$region=="uss"],type="p",pch=16, col="darkblue",cex=log(meannumdays$mnnumdays, base=10))
points(meannumdays$lon[meannumdays$region=="jf"],meannumdays$lat[meannumdays$region=="jf"],type="p",pch=16, col="darkgreen",cex=log(meannumdays$mnnumdays, base=10))

legend(-122.3,47,legend=c("# days per year= 2","# days per year= 80"),
       pch=21, pt.bg=c("darkblue","darkblue"), pt.cex=c(log(2, base=10), log(80, base=10)))

dev.off()


#Try boxplots
if(assumeSRKW==TRUE & use3regions==FALSE){figname<-paste("analyses/figures/OrcaPhenPlots/srkw_phenmap_assumeSRKW_April1box.pdf")}
if(assumeSRKW==FALSE & use3regions==FALSE){figname<-paste("analyses/figures/OrcaPhenPlots/srkw_phenmap_April1box.pdf")}
if(assumeSRKW==TRUE & use3regions==TRUE){figname<-paste("analyses/figures/OrcaPhenPlots/srkw_phenmap_assumeSRKW_3regs_April1box.pdf")}
if(assumeSRKW==FALSE & use3regions==TRUE){figname<-paste("analyses/figures/OrcaPhenPlots/srkw_phenmap_3regs_April1box.pdf")}

if(use3regions==FALSE){
  pdf(figname,height=12, width=6)
  #quartz(height=12, width=6)
  par(omi=c(.5,.5,.5,.5), mfrow=c(2,1))
}
if(use3regions==TRUE){
  pdf(figname,height=6, width=20)
  #quartz(height=6, width=20)
  par(omi=c(.5,.5,.5,.5), mfrow=c(1,4))
}


for(p in 1:length(podcols)){
  colnum<-which(colnames(orcasum.days)==podcols[p])
  regions=sort(unique(orcasum.days$region[-which(orcasum.days$region=="oc")]),decreasing=TRUE)
  for(r in regions){
    regdat<-orcasum.days[orcasum.days$region==r,]
    regdat<-regdat[regdat$orcayear>1976,]
    regdat<-regdat[regdat$orcayear<2017,]
    
    years = sort(unique(regdat$orcayear))
    if(r=="uss"){
      col="darkblue"
      #using orcayears
      boxplot(as.numeric(regdat$daysaftmar31[regdat[,colnum]==1])~as.factor(regdat$orcayear[regdat[,colnum]==1]),horizontal=TRUE,las=1,col=col,xaxt='n')    
      points(c(1,1,214,214),c(max(as.numeric(years))+1,min(as.numeric(years))-1,min(as.numeric(years))-1,max(as.numeric(years))+1))
      polygon(c(1,1,214,214),c(max(as.numeric(years))+1,min(as.numeric(years))-1,min(as.numeric(years))-1,max(as.numeric(years))+1),
              col=adjustcolor(col,alpha.f=0.2),
              border=NA)
    }
    if(r=="ps"){
      col="salmon"
      #using orcayears
      polygon(c(92,92,309,309),c(max(as.numeric(years))+1,min(as.numeric(years))-1,min(as.numeric(years))-1,max(as.numeric(years))+1),
              col=adjustcolor(col,alpha.f=0.2),
              border=NA)
      boxplot(as.numeric(regdat$daysaftmar31[regdat[,colnum]==1])~as.factor(regdat$orcayear[regdat[,colnum]==1]),horizontal=TRUE,las=1,col=col,xaxt='n')    
    }
      if(r=="jf"){
        col="darkgreen"
        #using orcayears
        boxplot(as.numeric(regdat$daysaftmar31[regdat[,colnum]==1])~as.factor(regdat$orcayear[regdat[,colnum]==1]),horizontal=TRUE,las=1,col=col,xaxt='n')    
        
        polygon(c(1,1,214,214),c(max(as.numeric(years))+1,min(as.numeric(years))-1,min(as.numeric(years))-1,max(as.numeric(years))+1),
                col=adjustcolor(col,alpha.f=0.2),
                border=NA)
      } 
    
    for(y in years){
      yrdat = regdat[regdat$orcayear==y,]
      days = yrdat$daysaftmar31[yrdat[,colnum]==1]
      
      points(x=days,y=rep(y,length=length(days)), pch=16,col= col, cex=1.3)
      #lines(x=days,y=rep(y,length=length(days)), lwd=2)
    }  
    
    axis(side=1,at = c(1,92,184,276,365), labels=c("1Apr","1Jul","1Oct","1Jan","31Mar"))
    #if(r=="uss"){mtext(paste(pods[p]), side=3,line=3, adj=0.5)}
    if(r=="uss"){mtext("Year", side=2,line=4, adj=0.5)}
    
  }
}

if(use3regions==FALSE){dev.off()}

#summarize the days on which atleast one sighting occured for each lat/long in ps and uss
srlatlon = aggregate(d$SRKW, list(d$Lat,d$Long,d$region,d$Year,d$day),sum)
colnames(srlatlon)<-c("lat","lon","region","year","doy","numsights")
latlons<-srlatlon[srlatlon$region!="oc",]#remove outercoast\
latlons<-latlons[latlons$lat!="",]#remove blank lats
latlons<-latlons[latlons$lon!="0.0000",] #remove weird lon
latlons$pres<-0#add column for 0/1 abs/pres on a given day at the lat/long. 
latlons$pres[latlons$numsights>0]<-1
numdays<-aggregate(latlons$pres, list(latlons$lat,latlons$lon,latlons$region,latlons$year),sum)
colnames(numdays)<-c("lat","lon","region","year","numdays")
numdays<-numdays[numdays$numdays>0,]#remove zeros
meannumdays<-aggregate(numdays$numdays, list(numdays$lat,numdays$lon,numdays$region),mean)
colnames(meannumdays)<-c("lat","lon","region","mnnumdays")
#make a map that shows mean number of whale days by lat long in each region
newmap <- getMap(resolution = "low")

#quartz(height=12, width=6)
if(assumeSRKW==TRUE & use3regions==FALSE){mapname<-paste("analyses/figures/OrcaPhenPlots/srkw_justmap_assumeSRKW_April1.pdf")
pdf(mapname,height=12, width=6)
}
plot(newmap, xlim = c(-126, -120), ylim = c(49.2, 51), asp = 1)
points(meannumdays$lon[meannumdays$region=="ps"],meannumdays$lat[meannumdays$region=="ps"],type="p",pch=16, col="salmon",cex=log(meannumdays$mnnumdays, base=10))
points(meannumdays$lon[meannumdays$region=="uss"],meannumdays$lat[meannumdays$region=="uss"],type="p",pch=16, col="darkblue",cex=log(meannumdays$mnnumdays, base=10))
legend(-123.5,46,legend=c("# days per year= 2","# days per year= 80"),
       pch=21, pt.bg=c("darkblue","darkblue"), pt.cex=c(log(2, base=10), log(80, base=10)))

dev.off()


#Make box plots of data across all years

  pdf("analyses/figures/OrcaPhenPlots/meanphenboxplot.pdf",height=6, width=10)
  #quartz(height=6, width=6)
  par(omi=c(.5,1.5,.5,.5), mfrow=c(1,1))
  ps.ussdat<-orcasum.days[orcasum.days$region!="oc",]
      cols=c("salmon","darkblue")
      #using orcayears
      boxplot(as.numeric(ps.ussdat$daysaftmar31)~as.factor(ps.ussdat$region),horizontal=TRUE,las=1,col=cols,xaxt='n', bty="l",boxwex=.75, at = c(1,5), names=c("Puget Sound","Central Salish Sea"), cex.label=1.2)    
    axis(side=1,at = c(1,92,184,276,365), labels=c("1Apr","1Jul","1Oct","1Jan","31Mar"))
dev.off()
