###Script to stitch together the 4 files f WDFW recreational fishing data sent by Karen Kloempken
###File started 5 April 2019
###By Ailene Ettinger


#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Set working directory: 
setwd("~/Documents/GitHub/fishphen")
#or from laptop:
#setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

# Load libraries

#Read in the files
#1987-1993 data
d1<-read.csv("data/WDFW_fromkk/PugetSound_1987_1989-1993_SalmonCatch-ReleaseData.csv", header=TRUE)
#Karen says 1987-1993 date format is MDDYY. 
#Some of the years have Location names and some do not. 
#The location code should be the same for all years. 
#Some of the species name are available but some years it was not. All years have species codes though.
head(d1)
#unique(d1$X.OtherSpCaught)
#unique(d1$SpeciesName)
#unique(d1$OtherSpCode)#NA 101 102 103 104 105 106 123 107 108
#Add 3 additional columns that other years have
d1$Rel.Sps.Code<-NA 
d1$X.Rel<-NA
d1$CohoCaughtAD<-NA

#add year
d1$year<-paste("19",substr(d1$SampleDate,nchar(d1$SampleDate)-1,nchar(d1$SampleDate)), sep="")
d1$year[d1$year=="1999"]<-"1989"
#add doy
d1$day<-substr(d1$SampleDate,nchar(d1$SampleDate)-3,nchar(d1$SampleDate)-2)
d1$mon<-substr(d1$SampleDate,nchar(d1$SampleDate)-(nchar(d1$SampleDate)-1),nchar(d1$SampleDate)-4)
d1$date<-as.Date(paste(d1$year,d1$mon,d1$day,sep="-"))
d1$doy<-strftime(d1$date, format = "%j")
#Try plotting some things just to see...
sort(unique(d1$CRCArea))
#9567 unique fishing areas!#need to convert these to fishing areas!
sort(unique(d1$LocationName))#only 105 unique location names
unique(d1$LoCode)#189 unique location codes
#dim(d1[substr(d1$CRCArea,1,2)=="WN",])#15000 rows
#unique(d1$LocationName[substr(d1$CRCArea,1,2)=="WN"])#these seem to be the CRC areas!""   "7"  "81" "82" "6"  "9"  "15" "5"  "12" "14" "4"  "13" "11" "10"
#add clean CRC AREa column that combines these two columsn
d1$CRCArea.cl<-d1$CRCArea
d1$CRCArea.cl[substr(d1$CRCArea,1,2)=="WN"]<-d1$LocationName[substr(d1$CRCArea,1,2)=="WN"]
d1$CRCArea.cl[d1$CRCArea=="UNKNOWN"|d1$CRCArea=="36000000"]<-d1$LocationName[d1$CRCArea=="UNKNOWN"|d1$CRCArea=="36000000"]
d1$CRCArea.cl[d1$LocationName=="Point Defiance Public Ramp"]<-"11"
d1$CRCArea.cl[d1$LocationName=="Point Defiance Boathouse"]<-"11"
#Question for Karen: do you think LoCode, LocationName or CRC area is more reliable? what about cases when they do not aligbn
#e.g. "Point Roberst Lighthouse County Park Ramp" should be CRC area 7, but is listed as 11
#sort(unique(d1$CRCArea.cl))#down to 542 areas!
#unique(d1$LoCode[d1$CRCArea.cl=="0"|d1$CRCArea.cl==""])
#see if you can use other rows where data do exist for these data to inform this
#unique(d1$CRCArea.cl[d1$LoCode==1367])#=8 for all but one blank, so it is safe to use 8 for the CRCArea for all assocated with this code?
d1$CRCArea.cl[d1$LoCode==1367]<-"8"
#tapply(d1$LocationName[d1$LoCode==1202],d1$CRCArea.cl[d1$LoCode==1202],length)#11 is by far most common
d1$CRCArea.cl[d1$LoCode==1202]<-"11"
#tapply(d1$LocationName[d1$LoCode==1069],d1$CRCArea.cl[d1$LoCode==1069],length)#9 is most common
d1$CRCArea.cl[d1$LoCode==1069]<-"9"
#tapply(d1$LocationName[d1$LoCode==1072],d1$CRCArea.cl[d1$LoCode==1072],length)#6 is by far most common
d1$CRCArea.cl[d1$LoCode==1072]<-"6"

#tapply(d1$LocationName[d1$LoCode==1067],d1$CRCArea.cl[d1$LoCode==1067],length)
d1$CRCArea.cl[d1$LoCode==1067]<-"9"

#tapply(d1$LocationName[d1$LoCode==1186],d1$CRCArea.cl[d1$LoCode==1186],length)
d1$CRCArea.cl[d1$LoCode==1186]<-"6"

#tapply(d1$LocationName[d1$LoCode==1187],d1$CRCArea.cl[d1$LoCode==1187],length)
d1$CRCArea.cl[d1$LoCode==1187]<-"6"

#tapply(d1$LocationName[d1$LoCode==1174],d1$CRCArea.cl[d1$LoCode==1174],length)
d1$CRCArea.cl[d1$LoCode==1174]<-"11"

#tapply(d1$LocationName[d1$LoCode==1192],d1$CRCArea.cl[d1$LoCode==1192],length)
d1$CRCArea.cl[d1$LoCode==1192]<-"9"

#tapply(d1$LocationName[d1$LoCode==1041],d1$CRCArea.cl[d1$LoCode==1041],length)#7,6,81 are most common
d1$CRCArea.cl[d1$LoCode==1041]<-"81"

#tapply(d1$LocationName[d1$LoCode==1177],d1$CRCArea.cl[d1$LoCode==1177],length)
d1$CRCArea.cl[d1$LoCode==1177]<-"9"

#tapply(d1$LocationName[d1$LoCode==1140],d1$CRCArea.cl[d1$LoCode==1140],length)
d1$CRCArea.cl[d1$LoCode==1140]<-"9"

#tapply(d1$LocationName[d1$LoCode== 1055],d1$CRCArea.cl[d1$LoCode== 1055],length)
d1$CRCArea.cl[d1$LoCode== 1055]<-"9"

#tapply(d1$LocationName[d1$LoCode==1126],d1$CRCArea.cl[d1$LoCode==1126],length)
d1$CRCArea.cl[d1$LoCode==1126]<-"13"

#tapply(d1$LocationName[d1$LoCode==1018],d1$CRCArea.cl[d1$LoCode==1018],length)
d1$CRCArea.cl[d1$LoCode==1018]<-"7"

#tapply(d1$LocationName[d1$LoCode==1144],d1$CRCArea.cl[d1$LoCode==1144],length)
d1$CRCArea.cl[d1$LoCode==1144]<-"9"

#tapply(d1$LocationName[d1$LoCode==1227],d1$CRCArea.cl[d1$LoCode==1227],length)
d1$CRCArea.cl[d1$LoCode==1227]<-"10"

#tapply(d1$LocationName[d1$LoCode==1151],d1$CRCArea.cl[d1$LoCode==1151],length)
d1$CRCArea.cl[d1$LoCode==1151]<-"82"

#tapply(d1$LocationName[d1$LoCode==1173],d1$CRCArea.cl[d1$LoCode==1173],length)
d1$CRCArea.cl[d1$LoCode==1173]<-"11"
#tapply(d1$LocationName[d1$LoCode==1013],d1$CRCArea.cl[d1$LoCode==1013],length)
d1$CRCArea.cl[d1$LoCode==1013]<-"7"
#unique(d1$LocationName[d1$LoCode==1149])#has 17 different ones 11 has the most- is this the correct one? sent email to KAren
#looking at a map, the narrows marina is in CRC area 13 (barely- just south of the route 16 bridge) so i'm called that CRC area 13
d1$CRCArea.cl[d1$LoCode==1149]<-"13"
d1$CRCArea.cl[d1$LoCode==1508]<-"12"
dim(d1[!is.na(as.numeric(d1$CRCArea.cl)),])#most
d1<-d1[as.numeric(d1$CRCArea.cl)<14|d1$CRCArea.cl=="81"|d1$CRCArea.cl=="82",]#most
dim(d1)

fas<-unique(d1$CRCArea.cl)
fas<-fas[-which(is.na(fas))]
for(j in 1:length(fas)){
fadat<-d1[which(d1$CRCArea.cl==fas[j]),]
years<-unique(fadat$year)
quartz(height=6,width=15)
par(mfrow=c(1,length(years)))
for(i in 1:length(years)){
yr<-fadat[which(fadat$year==years[i]),]
if(length(is.na(is.na(yr$X.ChinookCaught)))!=0){yr$X.ChinookCaught[which(is.na(yr$X.ChinookCaught))]<-0}
if(length(is.na(is.na(yr$X.CohoCaught)))!=0){yr$X.CohoCaught[which(is.na(yr$X.CohoCaught))]}
plot(as.integer(yr$doy),yr$X.ChinookCaught,type= "p", pch=21,bg="salmon",xlab="DOY", ylab="Fish Caught", main=paste(years[i],fas[j]))
points(as.integer(yr$doy),yr$X.CohoCaught)
points(as.integer(yr$doy),yr$X.OtherSpCaught, pch=21,bg="lightblue")
}
}
colnames(d1)[1:13]<-c("SampleDate","LoCode","LocName","CRCArea","BoatIntNum","Anglers","ChinCaught","CohoCaught","OtherSpCode","SpName", "OtherSpCaught","RelSpCode","NoRel")

#reorder dataset so that columns are in same order as other years
#d1a<-subset(d1,select=c())#put columns in same order as other datasets
#1994-1996
d2<-read.csv("data/WDFW_fromkk/PugetSound_1994-1996_SalmonCatch-ReleaseData.csv", header=TRUE)
head(d2)
unique(d2$X.OtherSpCaught)
unique(d2$SpeciesName)
unique(d2$OtherSpCode)#NA 101 102 103 104 105 106 123 107 108
unique(d2$CRCArea)
d2$CohoCaughtAD<-NA
#add year
d2$year<-paste("19",substr(d2$SampleDate,nchar(d2$SampleDate)-1,nchar(d2$SampleDate)), sep="")
#add doy
d2$day<-substr(d2$SampleDate,nchar(d2$SampleDate)-3,nchar(d2$SampleDate)-2)
d2$mon<-substr(d2$SampleDate,nchar(d2$SampleDate)-(nchar(d2$SampleDate)-1),nchar(d2$SampleDate)-4)
d2$date<-as.Date(paste(d2$year,d2$mon,d2$day,sep="-"))
d2$doy<-strftime(d2$date, format = "%j")
dim(d2)
dim(d2[!is.na(as.numeric(d2$CRCArea)),])#most
d2<-d2[as.numeric(d2$CRCArea)<14|d2$CRCArea=="81"|d2$CRCArea=="82",]#most
dim(d2)
colnames(d2)[1:14]<-c("SampleDate","LoCode","LocName","CRCArea","BoatIntNum","Anglers","ChinCaught","CohoCaught","OtherSpCode","SpName", "OtherSpCaught","RelSpCode","NoRel","CohoCaughtAD")


fas<-unique(d2$CRCArea)
fas<-fas[-which(is.na(fas))]
for(j in 1:length(fas)){
  fadat<-d2[which(d2$CRCArea==fas[j]),]
  years<-unique(fadat$year)
  quartz(height=6,width=15)
  par(mfrow=c(1,length(years)))
  for(i in 1:length(years)){
    yr<-fadat[which(fadat$year==years[i]),]
    if(length(is.na(is.na(yr$ChinCaught)))!=0){yr$ChinCaught[which(is.na(yr$ChinCaught))]<-0}
    if(length(is.na(is.na(yr$CohoCaught)))!=0){yr$CohoCaught[which(is.na(yr$CohoCaught))]}
    plot(as.integer(yr$doy),yr$ChinCaught,type= "p", pch=21,bg="salmon",xlab="DOY", ylab="Fish Caught", main=paste(years[i],fas[j]))
    points(as.integer(yr$doy),yr$CohoCaught)
    points(as.integer(yr$doy),yr$OtherSpCaught, pch=21,bg="lightblue")
  }
}
#reorder dataset so that columns are in same order as other years

years<-unique(d2$year)
for(i in 1:length(years)){
  yr<-d2[d2$year==years[i],]
  plot(as.integer(yr$doy),yr$X.ChinookCaught,type= "p", pch=21,bg="salmon",xlab="DOY", ylab="Fish Caught", main=paste(years[i]))
  points(as.integer(yr$doy),yr$X.CohoCaught)
}
colnames(d1)[1:13]<-c("SampleDate","LoCode","LocName","CRCArea","BoatIntNum","Anglers","ChinCaught","CohoCaught","OtherSpCode","SpName", "NoOtherSpp","RelSpCode","NoRel")



#1997-2000
d3<-read.csv("data/WDFW_fromkk/PugetSound_1997-2000_SalmonCatch-ReleaseData.csv", header=TRUE)
head(d3)
unique(d3$X.OtherSpCaught)
unique(d3$SpeciesName)
unique(d3$OtherSpCode)#NA 101 102 103 104 105 106 123 107 108
unique(d3$CRCArea)
d3$year<-substr(d3$SampleDate,nchar(d3$SampleDate)-3,nchar(d3$SampleDate))
#add doy
d3$day<-substr(d3$SampleDate,nchar(d3$SampleDate)-5,nchar(d3$SampleDate)-4)
d3$mon<-substr(d3$SampleDate,nchar(d3$SampleDate)-(nchar(d3$SampleDate)-1),nchar(d3$SampleDate)-6)

d3$date<-as.Date(paste(d3$year,d3$mon,d3$day,sep="-"))
d3$doy<-strftime(d3$date, format = "%j")
colnames(d3)[1:15]<-c("SampleDate","LoCode","LocName","CRCArea","BoatIntNum","Anglers","ChinCaught","CohoCaught","CohoCaughtAD","OtherSpCode","SpName", "NoOtherSpp","RelSpCode","RelSpName","NoRel")
#perhaps remove the following columns: CohoRelNotKnown"       "OthMarkCodeReleased"  "OthMarkCountReleased"  "X" 

#2001-2002
d4<-read.csv("data/WDFW_fromkk/PugetSound_2001-2002SalmonCatch-ReleaseData.csv", header=TRUE)
head(d4)
unique(d4$SpeciesName)
unique(nchar(d4$SampleDate))
d4$year<-substr(d4$SampleDate,nchar(d4$SampleDate)-3,nchar(d4$SampleDate))
#add doy
d4$day<-substr(d4$SampleDate,nchar(d4$SampleDate)-5,nchar(d4$SampleDate)-4)
d4$mon<-substr(d4$SampleDate,nchar(d4$SampleDate)-(nchar(d4$SampleDate)-1),nchar(d4$SampleDate)-6)

d4$date<-as.Date(paste(d4$year,d4$mon,d4$day,sep="-"))
d4$doy<-strftime(d4$date, format = "%j")
colnames(d4)[1:13]<-c("SampleDate","LoCode","LocName","CRCArea","BoatIntNum","Anglers","ChinCaught","ChinAd" ,"CohoCaught","CohoCaughtAD","OtherSpCode","SpName", "NoOtherSpp")


#2003-2005
d5<-read.csv("data/WDFW_fromkk/PugetSound_2003-2005SalmonCatch-ReleaseData.csv", header=TRUE)
head(d5)
unique(d5$SpeciesName)

#2006-2008
d6<-read.csv("data/WDFW_fromkk/PugetSound_2006-2008SalmonCatch-ReleaseData.csv", header=TRUE)
head(d6)
unique(d6$SpeciesName)

#2009-2013
d7<-read.csv("data/WDFW_fromkk/PugetSound_2009-2013SalmonCatch-ReleaseData.csv", header=TRUE)
head(d7)
unique(d6$SpeciesName)

#2014
d8<-read.csv("data/WDFW_fromkk/PugetSound_2014SalmonCatch-ReleaseData.csv", header=TRUE)

head(d8)

#2015-2017April
d9<-read.csv("data/WDFW_fromkk/PugetSound_2015-2017AprSalmonCatch-ReleaseData.csv", header=TRUE)
head(d9)
#2017May-2018dec
d10<-read.csv("data/WDFW_fromkk/PugetSound_2017May-2018SalmonCatch-ReleaseData.csv", header=TRUE)
head(d10)


#now stitch together all 10 files!