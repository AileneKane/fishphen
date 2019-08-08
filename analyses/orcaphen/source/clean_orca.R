#Code for Cleaning OrcaMaster Database
#d = Appendix II
#1. Clean the Pod Column
d$Pod[d$Pod=="j"|d$Pod==" J"|d$Pod=="J "|d$Pod=="J  "|d$Pod=="J   "|d$Pod=="J "|d$Pod=="J  "]<-"J"
d$Pod[d$Pod=="Jp "|d$Pod=="Jp  "|d$Pod=="Js"]<-"Jp"
d$Pod[d$Pod=="J1 "]<-"J1"
d$Pod[d$Pod=="K "|d$Pod=="K  "]<-"K"

d$Pod[d$Pod=="Kp"|d$Pod=="KP"|d$Pod=="Kp  "|d$Pod=="Kp  "]<-"Kp"
d$Pod[d$Pod=="L "|d$Pod=="L  "|d$Pod=="Ls"]<-"L"
d$Pod[d$Pod=="LP"|d$Pod=="Lp  "|d$Pod=="Lp "|d$Pod=="Lp?"]<-"Lp"
d$Pod[d$Pod=="JK "|d$Pod=="JK  "|d$Pod=="JK   "]<-"JK"

d$Pod[d$Pod=="JL "|d$Pod=="JL  "|d$Pod=="JL   "]<-"JL"
d$Pod[d$Pod=="KL "|d$Pod=="KL  "|d$Pod=="LK"]<-"KL"
d$Pod[d$Pod=="KpL "|d$Pod=="KpL  "]<-"KpL"
d$Pod[d$Pod=="JL "|d$Pod=="JL  "|d$Pod=="JL   "]<-"JL"
d$Pod[d$Pod=="JKl"|d$Pod=="JKL  "]<-"JKL"

d$Pod[d$Pod=="T"|d$Pod=="Ts "]<-"Ts"
d$Pod[d$Pod=="Orca"|d$Pod=="orca"|d$Pod=="orcas"]<-"Orcas"
d$Pod[d$Pod=="SR"|d$Pod=="sRs"|d$Pod=="S"|d$Pod=="SWKW"|d$Pod=="SRS"]<-"SRs"
#dim(d)
# 2. Clean the Likely Pod column
d$LikelyPod[d$LikelyPod==" J"|d$LikelyPod=="J "|d$LikelyPod=="J  "|d$LikelyPod=="J   "]<-"J"
d$LikelyPod[d$LikelyPod=="J1 "]<-"J1"
d$LikelyPod[d$LikelyPod=="L "|d$LikelyPod=="L  "]<-"L"
d$LikelyPod[d$LikelyPod=="JK "|d$LikelyPod=="JK  "]<-"JK"
d$LikelyPod[d$LikelyPod=="JL "]<-"JL"
d$LikelyPod[d$LikelyPod=="JLP"]<-"JLp"
d$LikelyPod[d$LikelyPod=="JKLp "|d$LikelyPod=="JKLp  "]<-"JKLp"
d$LikelyPod[d$LikelyPod=="JKp  "|d$LikelyPod=="JKp "]<-"JKp"
d$LikelyPod[d$LikelyPod=="Jp  "|d$LikelyPod=="Jp "]<-"Jp"
d$LikelyPod[d$LikelyPod=="KL "|d$LikelyPod=="KL  "]<-"KL"

d$LikelyPod[d$LikelyPod=="SR"|d$LikelyPod=="sRs"|d$LikelyPod=="S"]<-"SRs"
d$LikelyPod[d$LikelyPod=="T"|d$LikelyPod=="Ts "]<-"Ts"

#dim(d)
#3. Clean source column
unique(d$Source)
d$Source[d$Source=="SPOT "|d$Source=="SPOT   "]<-"SPOT"

#4. Clean Year column
d$Year[d$Year==0]<-2001

#5. Clean the FishArea column and format them to match the WDFW rec data formatting
d$FishArea[d$FishArea=="1"]<-"01"
d$FishArea[d$FishArea=="2"]<-"02"
d$FishArea[d$FishArea=="3"]<-"03"
d$FishArea[d$FishArea=="4"]<-"04"
d$FishArea[d$FishArea=="5"]<-"05"
d$FishArea[d$FishArea=="6"]<-"06"
d$FishArea[d$FishArea=="7"]<-"07"
d$FishArea[d$FishArea=="8"]<-"08"
d$FishArea[d$FishArea=="9"]<-"09"
d$FishArea[d$FishArea=="8-1"|d$FishArea=="8.1"]<-"81"
d$FishArea[d$FishArea=="8-2"|d$FishArea=="8.2"]<-"82"
d$FishArea[d$FishArea=="17"]<-"17C"
d$FishArea[d$FishArea=="18"]<-"18C"
d$FishArea[d$FishArea=="20"]<-"20C"
d$FishArea[d$FishArea=="28"]<-"28C"
d$FishArea[d$FishArea=="29"]<-"29C"
d$FishArea[d$FishArea=="121A"]<-"121C"

#6. Convert quad data to FishArea and Lat Long
quads<-quads[,1:6]
#format "quads" to match orcamaster database above

quads$Fish.Area[quads$Fish.Area==5.0]<-"05"
quads$Fish.Area[quads$Fish.Area==6.0]<-"06"
quads$Fish.Area[quads$Fish.Area==7.0]<-"07"
quads$Fish.Area[quads$Fish.Area==8.1]<-"81"
quads$Fish.Area[quads$Fish.Area==8.2]<-"82"
quads$Fish.Area[quads$Fish.Area==9.0]<-"09"
quads$Fish.Area[quads$Fish.Area==10.0]<-"10"
quads$Fish.Area[quads$Fish.Area==11.0]<-"11"
quads$Fish.Area[quads$Fish.Area==12.0]<-"12"
quads$Fish.Area[quads$Fish.Area==13.0]<-"13"
quads$Fish.Area[quads$Fish.Area==14.0]<-"14C"
quads$Fish.Area[quads$Fish.Area==17.0]<-"17C"
quads$Fish.Area[quads$Fish.Area==18.0]<-"18C"
quads$Fish.Area[quads$Fish.Area==19.0]<-"19C"
quads$Fish.Area[quads$Fish.Area==20.0]<-"20C"
quads$Fish.Area[quads$Fish.Area==28.0]<-"28C"
quads$Fish.Area[quads$Fish.Area==29.0]<-"29C"

#for sightings with quad area but no fisharea, add in fisharea
#check which sites with no fisharea have a quadrant
#sort(unique(d$Quadrant[d$FishArea==""|d$FishArea=="<Null>"]))
#length(d$Quadrant[d$FishArea==""|d$FishArea=="<Null>"])#728
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="110"]<-quads$Fish.Area[quads$Quad==110]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="112"]<-quads$Fish.Area[quads$Quad==112]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="113"]<-quads$Fish.Area[quads$Quad==113]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="122"]<-quads$Fish.Area[quads$Quad==122]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="124"]<-quads$Fish.Area[quads$Quad==124]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="136"]<-quads$Fish.Area[quads$Quad==136]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="141"]<-quads$Fish.Area[quads$Quad==141]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="164"]<-quads$Fish.Area[quads$Quad==164]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="170"]<-quads$Fish.Area[quads$Quad==170]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="175"]<-quads$Fish.Area[quads$Quad==175]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="176"]<-quads$Fish.Area[quads$Quad==176]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="181"]<-quads$Fish.Area[quads$Quad==181]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="182"]<-quads$Fish.Area[quads$Quad==182]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="20"]<-quads$Fish.Area[quads$Quad==20]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="226"]<-quads$Fish.Area[quads$Quad==226]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="227"]<-quads$Fish.Area[quads$Quad==227]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="228"]<-quads$Fish.Area[quads$Quad==228]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="229"]<-quads$Fish.Area[quads$Quad==229]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="232"]<-quads$Fish.Area[quads$Quad==232]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="237"]<-quads$Fish.Area[quads$Quad==237]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="238"]<-quads$Fish.Area[quads$Quad==238]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="240"]<-quads$Fish.Area[quads$Quad==240]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="241"]<-quads$Fish.Area[quads$Quad==241]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="258"]<-quads$Fish.Area[quads$Quad==258]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="317"]<-quads$Fish.Area[quads$Quad==317]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="43"]<-quads$Fish.Area[quads$Quad==43]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="62"]<-quads$Fish.Area[quads$Quad==62]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="64"]<-quads$Fish.Area[quads$Quad==64]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="65"]<-quads$Fish.Area[quads$Quad==65]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="67"]<-quads$Fish.Area[quads$Quad==67]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="76"]<-quads$Fish.Area[quads$Quad==76]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="77"]<-quads$Fish.Area[quads$Quad==77]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="80"]<-quads$Fish.Area[quads$Quad==80]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="81"]<-quads$Fish.Area[quads$Quad==81]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="95"]<-quads$Fish.Area[quads$Quad==95]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="96"]<-quads$Fish.Area[quads$Quad==96]
d$FishArea[d$FishArea==""|d$FishArea=="<Null>"& d$Quadrant=="97"]<-quads$Fish.Area[quads$Quad==97]

#d$Quadrant[which(is.na(d$FishArea))]#all="" so can't be added
#some sights have fishareas of 42583 or 42584 which don't make sense, but quadrant does make sense. Fix these

d$FishArea[d$FishArea=="42583" & d$Quadrant=="366"]<-quads$Fish.Area[quads$Quad==366]
d$FishArea[d$FishArea=="42583" & d$Quadrant=="372"]<-quads$Fish.Area[quads$Quad==372]
d$FishArea[d$FishArea=="42583" & d$Quadrant=="373"]<-quads$Fish.Area[quads$Quad==373]
d$FishArea[d$FishArea=="42583" & d$Quadrant=="374"]<-quads$Fish.Area[quads$Quad==374]
d$FishArea[d$FishArea=="42583" & d$Quadrant=="375"]<-quads$Fish.Area[quads$Quad==375]
d$FishArea[d$FishArea=="42583" & d$Quadrant=="379"]<-quads$Fish.Area[quads$Quad==379]
d$FishArea[d$FishArea=="42583" & d$Quadrant=="380"]<-quads$Fish.Area[quads$Quad==380]

d$FishArea[d$FishArea=="42584" & d$Quadrant=="378"]<-quads$Fish.Area[quads$Quad==378]
d$FishArea[d$FishArea=="42584" & d$Quadrant=="381"]<-quads$Fish.Area[quads$Quad==381]
d$FishArea[d$FishArea=="42584" & d$Quadrant=="383"]<-quads$Fish.Area[quads$Quad==383]
d$FishArea[d$FishArea=="42584" & d$Quadrant=="384"]<-quads$Fish.Area[quads$Quad==384]
d$FishArea[d$FishArea=="42584" & d$Quadrant=="385"]<-quads$Fish.Area[quads$Quad==385]
d$FishArea[d$FishArea=="42584" & d$Quadrant=="386"]<-quads$Fish.Area[quads$Quad==386]
d$FishArea[d$FishArea=="42584" & d$Quadrant=="398"]<-quads$Fish.Area[quads$Quad==398]

#Remove non-orca data- removed this because more absence data might be good!
#d<-d[d$LikelyPod!="HB?"|d$LikelyPod!="Not Orcas",]
###Are there any sites that hace qudrant but no lat long?
unique(d$Quadrant[which(is.na(as.numeric(d$Lat)))])
YES! Add these

#7. #Create a new column that combines Pod and Likely Pod columna and removes spaces
d$Pod.cl<-d$Pod

#Always use Likely Pod column, when it is not blank:
d$Pod.cl[d$LikelyPod!="" & d$LikelyPod!=" "]<-d$LikelyPod[d$LikelyPod!="" & d$LikelyPod!=" "]
#perhaps also stick with Pod when LikelyPod has a "?" grep("?",d$LikelyPod,)
#8. Add week and day of year (day)
d$day<-strftime(strptime(paste(d$Month, d$Day, d$Year, sep="."),format= "%m.%d.%Y"),format= "%j")
d$week<-strftime(strptime(paste(d$Month, d$Day, d$Year, sep="."),format= "%m.%d.%Y"), format = "%V")#new weeks start on mondays

d<-d[-which(is.na(d$day)),]

#clean the Latitude and  longitude columns- replace with ActLat and ActLong columns when these are present
d$Lat[which(!is.na(d$ActLat))]<-d$ActLat[which(!is.na(d$ActLat))]
d$Long[which(!is.na(d$ActLong))]<-d$ActLong[which(!is.na(d$ActLong))]

#9. Create a new cleaned datafile
write.csv(d,"analyses/output/AppendixII_cleaned.csv")


#check some things about this datafile and check that it matches up with numbers in Olson et al Table 1:
#tapply(d$SightDate[d$Year>1975 & d$Year<2015],d$Source[d$Year>1975 & d$Year<2015],length)
#to get their Lime Kiln Station sightings data:
#tapply(d$SightDate[d$Year>1975 & d$Year<2015 & d$Quadrant == 18],d$Source[d$Year>1975 & d$Year<2015 & d$Quadrant == 18],length)
#I only get 4 sites...Olson et al says 1881

#d[d$Year>1975 & d$Year<2015 & d$Quadrant == 18,]

#to get TWM "sighting archive" which are "sighitng records reported by public and relaibale observers to TWM
#length(d$SightDate[d$Source =="TWM-HYD-Pub" | d$Source =="TWM-HYD-Rel"| d$Source =="TWM-SA-Pub"|d$Source =="TWM-SA-Rel"|d$Source == "TWM-Otis"])
#39819 without Otis, 41800 with OTtis
#d$year.doy<-paste(d$Year,d$day,sep=".")
