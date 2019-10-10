#Select sites to use in return data analyses
# Choose streams that 
### a) have high total run sizes for wild coho, chum, chinook; 
### b) are close to puget sound
#The following hatcheries seem to have large amounts of continuous data and seem to be close to puget sound:
#dps<-d[d$Facility_Short_Name=="HOODSPORT HATCHERY"|d$Facility_Short_Name=="ISSAQUAH HATCHERY"|d$Facility_Short_Name=="MINTER CR HATCHERY"|
#d$Facility_Short_Name=="SOOS CREEK HATCHERY"|d$Facility_Short_Name=="TUMWATER FALLS HATCHERY"|d$Facility_Short_Name=="VOIGHTS CR HATCHERY"|

co<-table(d$Facility_Short_Name[d$SPECIES_Code=="CO"],d$ORIGIN_TYPE_Code[d$SPECIES_Code=="CO"])
ch<-table(d$Facility_Short_Name[d$SPECIES_Code=="CH"],d$ORIGIN_TYPE_Code[d$SPECIES_Code=="CH"])
ck<-table(d$Facility_Short_Name[d$SPECIES_Code=="CK"],d$ORIGIN_TYPE_Code[d$SPECIES_Code=="CK"])
ch.df<-as.data.frame(cbind(rownames(ch),ch[,1],ch[,2],ch[,3],ch[,4]))
colnames(ch.df)<- c("site","H.ch","M.ch","U.ch","W.ch")
co.df<-as.data.frame(cbind(rownames(co),co[,1],co[,2],co[,3],co[,4]))
colnames(co.df)<- c("site","H.co","M.co","U.co","W.co")
ck.df<-as.data.frame(cbind(rownames(ck),ck[,1],ck[,2],ck[,3],ck[,4]))
colnames(ck.df)<- c("site","H.ck","M.ck","U.ck","W.ck")

#sites with most hatchery/wild ck data
ck.df<-ck.df[order(as.numeric(ck.df$H.ck)),]
ck.hatch<-ck.df$site[as.numeric(ck.df$H.ck)>500]
ck.wild<-ck.df$site[as.numeric(ck.df$W.ck)>200]
ck.sites<-unique(c(ck.hatch,ck.wild))

#sites with most hatchery ch data
ch.df<-ch.df[order(as.numeric(ch.df$H.ch)),]
ch.hatch<-ch.df$site[as.numeric(ch.df$H.ch)>100]
ch.wild<-ch.df$site[as.numeric(ch.df$W.ch)>60]
ch.sites<-unique(c(ch.hatch,ch.wild))

#sites with most hatchery ch data
co.df<-co.df[order(as.numeric(co.df$H.co)),]
co.hatch<-co.df$site[as.numeric(co.df$H.co)>400]
co.wild<-co.df$site[as.numeric(co.df$W.co)>100]
co.sites<-unique(c(co.hatch,co.wild))

#make dataframe for hatchery sites and one for wild sites
wild<-as.data.frame(rbind(
        cbind(ck.wild,rep("ck", times=length(ck.wild))),
        cbind(ch.wild,rep("ch", times=length(ch.wild))),
        cbind(co.wild,rep("co", times=length(co.wild)))
))

hatch<-as.data.frame(rbind(
  cbind(ck.hatch,rep("ck", times=length(ck.hatch))),
  cbind(ch.hatch,rep("ch", times=length(ch.hatch))),
  cbind(co.hatch,rep("co", times=length(co.hatch)))
))
colnames(wild)<-colnames(hatch)<-c("Facility_Short_Name","sp")
#add lat/longs for each site in each df
wild<-left_join(wild,latlon,rep=TRUE)
hatch<-left_join(hatch,latlon,rep=TRUE)

#Map the wild vs hatchery sites to see where they are

#Make a map with names by species, to help me decide which sites to use for looking at trends in return dates
#quartz(height=8, width=25)
par(mfrow=c(1,3), oma=c(1,1,1,1))
newmap <- getMap(resolution = "low")
sp<-unique(wild$sp)
col=c("darkred","darkblue","darkgreen")
for(s in 1:length(sp)){
  spdat<-wild[wild$sp==sp[s],]
  plot(newmap, xlim = c(-125, -119), ylim = c(47, 48), asp = 1,main=paste(sp[s],"-wild"))
  points(spdat$Lon,spdat$Lat,type="p",pch=21, cex=1.3, bg=col[s])
  text(spdat$Lon,spdat$Lat,labels=substr(spdat$Facility_Short_Name,1,5), cex=0.8)
}

#now hatchery salmon
#quartz(height=8, width=25)
par(mfrow=c(1,3), oma=c(1,1,1,1))
newmap <- getMap(resolution = "low")
sp<-unique(hatch$sp)
col=c("darkred","darkblue","darkgreen")
for(s in 1:length(sp)){
  spdat<-hatch[hatch$sp==sp[s],]
  plot(newmap, xlim = c(-125, -119), ylim = c(47, 48), asp = 1,main=paste(sp[s],"-hatch"))
  points(spdat$Lon,spdat$Lat,type="p",pch=21, cex=1.3, bg=col[s])
  text(spdat$Lon,spdat$Lat,labels=substr(spdat$Facility_Short_Name,1,5), cex=1.1)
}





#other hatcheries that seem to have continuous data: BINGHAM CR HATCHERY#COWLITZ SALMON HATCHERY#DUNGENESS HATCHERY#EASTBANK HATCHERY#ELOCHOMAN HATCHERY#ELWHA HATCHERY#FALLERT CR HATCHERY #FORKS CREEK HATCHERY#GARRISON HATCHERY 
#GRAYS RIVER HATCHERY #HUMPTULIPS HATCHERY#HUPP SPRINGS REARING#KALAMA FALLS HATCHERY #KENDALL CR HATCHERY #KLICKITAT HATCHERY#LEWIS RIVER HATCHERY#K ABERDEEN HATCHERY #LYONS FERRY HATCHERY #MARBLEMOUNT HATCHERY#MCKERNAN HATCHERY#METHOW HATCHERY 
#NASELLE HATCHERY #NEMAH HATCHERY#NORTH TOUTLE HATCHERY#PRIEST RAPIDS HATCHERY#RINGOLD SPRINGS HATCHERY #SAMISH HATCHERY#SOLDUC HATCHERY #WASHOUGAL HATCHERY#WELLS HATCHERY
#Plot the data and look at it and pull out first and last observation date


#HAtcheries that seem to be close to Puget sound and have high runs are:
#CEDAR RIVER HATCHERY- ck(wild), co(wild) 
#ELWHA HATCHERY- ch(wild), ck(hatch)
#GARRISON HATCHERY-ch(wild)
#SOOS CREEK HATCHERY-ch(wild)
#MINTER CR HATCHERY- co(wild,hatch), ck(hatch), ch(hatch)
#GEORGE ADAMS HATCHERY - ck(hatch),ch(hatch)
#HOODSPORT HATCHERY - ck(hatch), ch(hatch)
#GLENWOOD SPRINGS- ck(hatch)
#MCKERNAN HATCHERY- ch(hatch)
#WHATCOM CR HATCHERY- ch(hatch)
#COWLITZ SALMON HATCHERY-ck(wild,hatch)- too far frmo puget sound, not used
#BINGHAM CR , SATSPON - tied to outer coast not puget sound so not used
#restrict sites to those with large numbers of salmon and within XX km of puget sound/salish sea
ck.wild.subs<-"CEDAR RIVER HATCHERY"
co.wild.subs<-c("CEDAR RIVER HATCHERY","MINTER CR HATCHERY")
ch.wild.subs<-c("ELWHA HATCHERY","GARRISON HATCHERY","SOOS CREEK HATCHERY")
ck.hatch.subs<-c("ELWHA HATCHERY","MINTER CR HATCHERY","GEORGE ADAMS HATCHERY","HOODSPORT HATCHERY","GLENWOOD SPRINGS")
co.hatch.subs<-"MINTER CR HATCHERY"
ch.hatch.subs<-c("MINTER CR HATCHERY","GEORGE ADAMS HATCHERY","HOODSPORT HATCHERY","MCKERNAN HATCHERY","WHATCOM CR HATCHERY")

#make dataframe for hatchery sites and one for wild sites

wild<-as.data.frame(rbind(
  cbind(ck.wild.subs,rep("CK", times=length(ck.wild.subs)),rep("W", times=length(ck.wild.subs))),
  cbind(ch.wild.subs,rep("CH", times=length(ch.wild.subs)),rep("W", times=length(ch.wild.subs))),
  cbind(co.wild.subs,rep("CO", times=length(co.wild.subs)),rep("W", times=length(co.wild.subs)))
))

hatch<-as.data.frame(rbind(
  cbind(ck.hatch.subs,rep("CK", times=length(ck.hatch.subs)),rep("H", times=length(ck.hatch.subs))),
  cbind(ch.hatch.subs,rep("CH", times=length(ch.hatch.subs)),rep("H", times=length(ch.hatch.subs))),
  cbind(co.hatch.subs,rep("CO", times=length(co.hatch.subs)),rep("H", times=length(co.hatch.subs)))
))
colnames(wild)<-colnames(hatch)<-c("Facility_Short_Name","SPECIES_Code","ORIGIN_TYPE_Code")
#add lat/longs for each site in each df
wild<-left_join(wild,latlon,rep=TRUE)
hatch<-left_join(hatch,latlon,rep=TRUE)
