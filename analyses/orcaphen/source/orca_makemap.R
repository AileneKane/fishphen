#Make a map of observations of SRKW sightings in different regions
#make same plot, but prettier and just for ALL SRs in PS and upper salish sea

figname<-paste("analyses/figures/OrcaPhenPlots/srkw_phenmap.pdf")
pdf(figname,height=6, width=15)
#quartz(height=6, width=15)
par(omi=c(.5,2,.5,.5), mfrow=c(1,3))

podcols<-"AllSRpres"
pods<-"SRs"

#regnames<- c("Winter Area","Summer Area")
for(p in 1:length(podcols)){
   colnum<-which(colnames(orcasum.days)==podcols[p])
  regions=unique(orcasum.days$region)
  regions<- c("ps","uss")#winter = dark blue, summer = salmon
  for(r in regions){
    regdat<-orcasum.days[orcasum.days$region==r,]
    years = unique(orcasum.days$year)
    plot(0, type = 'n', las=1, xlim=c(1,366),ylim=c(min(as.numeric(years)),max(as.numeric(years))),ylab="",xlab="Day of year", main="", cex.axis=1.1, cex.lab=1.3)
    if(r=="uss"){
      col="darkblue"
      polygon(c(121,121,304,304),c(max(as.numeric(years))+1,min(as.numeric(years))-1,min(as.numeric(years))-1,max(as.numeric(years))+1),
              col=adjustcolor(col,alpha.f=0.2),
              border=NA)
    }
    if(r=="ps"){
      col="salmon"
      polygon(c(244,244,368,368),c(max(as.numeric(years))+1,min(as.numeric(years))-1,min(as.numeric(years))-1,max(as.numeric(years))+1),
              col=adjustcolor(col,alpha.f=0.2),
              border=NA)
    }
    
    for(y in years){
      yrdat = regdat[regdat$year==y,]
      days = yrdat$day[yrdat[,colnum]==1]
          
      points(x=days,y=rep(y,length=length(days)), pch=16,col= col, cex=1.3)
      #lines(x=days,y=rep(y,length=length(days)), lwd=2)
    }  
    #if(r=="uss"){mtext(paste(pods[p]), side=3,line=3, adj=0.5)}
    if(r=="ps"){mtext("Year", side=2,line=4, adj=0.5)}
    
  }
}


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

#quartz(height=6, width=6)
 plot(newmap, xlim = c(-125, -120), ylim = c(47, 49), asp = 1)
  points(meannumdays$lon[meannumdays$region=="ps"],meannumdays$lat[meannumdays$region=="ps"],type="p",pch=16, col="salmon",cex=log(meannumdays$mnnumdays, base=10))
  points(meannumdays$lon[meannumdays$region=="uss"],meannumdays$lat[meannumdays$region=="uss"],type="p",pch=16, col="darkblue",cex=log(meannumdays$mnnumdays, base=10))
  legend(-122.2,48,legend=c("# days per year= 2","# days per year= 78"),
         pch=21, bty="n",pt.bg=c("darkblue","darkblue"), pt.cex=c(log(2, base=10), log(78, base=10)))
  
dev.off()
