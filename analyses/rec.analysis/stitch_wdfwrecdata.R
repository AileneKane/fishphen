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
#Karen says 1987?1993 date format is MDDYY. 
#Some of the years have Location names and some do not. 
#The location code should be the same for all years. 
#Some of the species name are available but some years it was not. All years have species codes though.
head(d1)
unique(d1$X.OtherSpCaught)
unique(d1$SpeciesName)
unique(d1$OtherSpCode)#NA 101 102 103 104 105 106 123 107 108
#Add 3 additional columns that other years have
d1$Rel.Sps.Code<-NA 
d1$X.Rel<-NA
d1$CohoCaughtAD<-NA
#reorder dataset so that columns are in same order as other years

#add year
d1$year<-paste("19",substr(d1$SampleDate,nchar(d1$SampleDate)-1,nchar(d1$SampleDate)), sep="")
#add doy
d1$day<-substr(d1$SampleDate,nchar(d1$SampleDate)-3,nchar(d1$SampleDate)-2)
d1$mon<-substr(d1$SampleDate,nchar(d1$SampleDate)-(nchar(d1$SampleDate)-1),nchar(d1$SampleDate)-4)
d1$date<-as.Date(paste(d1$year,d1$mon,d1$day,sep="-"))
d1$doy<-strftime(d1$date, format = "%j")
#Try plotting some things just to see...
quartz(height=6,width=15)
par(mfrow=c(1,7))
years<-unique(d1$year)
#fas<-unique(d1$CRCArea)
#9567 unique fishing areas!
#need to convert these to fishing areas!
for(i in 1:length(years)){
yr<-d1[d1$year==years[i],]
  plot(as.integer(yr$doy),yr$X.ChinookCaught,type= "p", pch=21,bg="salmon",xlab="DOY", ylab="Fish Caught", main=paste(years[i]))
points(as.integer(yr$doy),yr$X.CohoCaught)
}

length(colnames(d1))

#1994-1996
d2<-read.csv("data/WDFW_fromkk/PugetSound_1994-1996_SalmonCatch-ReleaseData.csv", header=TRUE)
head(d2)
unique(d2$X.OtherSpCaught)
unique(d2$SpeciesName)
unique(d2$OtherSpCode)#NA 101 102 103 104 105 106 123 107 108
d2$CohoCaughtAD<-NA
#reorder dataset so that columns are in same order as other years


#1997-2000
d3<-read.csv("data/WDFW_fromkk/PugetSound_1997-2000_SalmonCatch-ReleaseData.csv", header=TRUE)
head(d3)
unique(d3$X.OtherSpCaught)
unique(d3$SpeciesName)
unique(d3$OtherSpCode)#NA 101 102 103 104 105 106 123 107 108

#2001-2013 data missing!

#2014
d5<-read.csv("data/WDFW_fromkk/PugetSound_2014SalmonCatch-ReleaseData.csv", header=TRUE)
#2015-2017April
head(d5)
d6<-read.csv("data/WDFW_fromkk/PugetSound_2015-2017AprSalmonCatch-ReleaseData.csv", header=TRUE)
head(d6)
#2017May-2018dec
d7<-read.csv("data/WDFW_fromkk/PugetSound_2017May-2018SalmonCatch-ReleaseData.csv", header=TRUE)
head(d7)
