#Code for Cleaning the Sears Dataset
#searsd = SearsCompiled
#1. Clean the Pod and LikelyPod Columns
#sort(unique(searsd$Pod))
searsd$Pod[searsd$Pod=="J, K"|searsd$Pod=="J,K"]<-"JK"
searsd$Pod[searsd$Pod=="K,L"]<-"KL"
searsd$Pod[searsd$Pod=="J,L"]<-"JL"
searsd$Pod[searsd$Pod=="J,K,L"|searsd$Pod=="J,K.L"]<-"JKL"
searsd$Pod[searsd$Pod=="T"]<-"Ts"
searsd$Pod[searsd$Pod=="SR"]<-"SRs"

sort(unique(searsd$LikelyPod))
searsd$LikelyPod[searsd$LikelyPod=="T"]<-"Ts"

#2. Clean the FishArea column and format them to match the WDFW rec data formatting
#sort(unique(searsd$FishArea))
searsd$FishArea[searsd$FishArea=="7"]<-"07"
searsd$FishArea[searsd$FishArea=="9"]<-"09"

#3. Convert quad data to FishArea and Lat Long when missing
#dim(searsd[which(is.na(searsd$ActLat)),])#304 rows without lat/long but some of these have quad so can add the lat/long
#sort(unique(searsd$Quadrant[which(is.na(as.numeric(searsd$ActLat)))]))
#sort(unique(searsd$Quadrant[searsd$ActLat==0.0000]))
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="188")]<-quads$Lat[quads$Quad==188]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="388")]<-quads$Lat[quads$Quad==388]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="394")]<-quads$Lat[quads$Quad==394]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="397")]<-quads$Lat[quads$Quad==397]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="398")]<-quads$Lat[quads$Quad==398]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="400")]<-quads$Lat[quads$Quad==400]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="402")]<-quads$Lat[quads$Quad==402]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="403")]<-quads$Lat[quads$Quad==403]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="404")]<-quads$Lat[quads$Quad==404]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="407")]<-quads$Lat[quads$Quad==407]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="408")]<-quads$Lat[quads$Quad==408]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="410")]<-quads$Lat[quads$Quad==410]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="412")]<-quads$Lat[quads$Quad==412]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="413")]<-quads$Lat[quads$Quad==413]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="414")]<-quads$Lat[quads$Quad==414]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="415")]<-quads$Lat[quads$Quad==415]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="416")]<-quads$Lat[quads$Quad==416]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="417")]<-quads$Lat[quads$Quad==417]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="418")]<-quads$Lat[quads$Quad==418]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="419")]<-quads$Lat[quads$Quad==419]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="420")]<-quads$Lat[quads$Quad==420]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="422")]<-quads$Lat[quads$Quad==422]
searsd$ActLat[which(searsd$ActLat==0.0000 & searsd$Quadrant=="442")]<-quads$Lat[quads$Quad==442]

searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="176")]<-quads$Lat[quads$Quad==176]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="394")]<-quads$Lat[quads$Quad==394]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="395")]<-quads$Lat[quads$Quad==395]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="396")]<-quads$Lat[quads$Quad==396]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="397")]<-quads$Lat[quads$Quad==397]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="398")]<-quads$Lat[quads$Quad==398]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="400")]<-quads$Lat[quads$Quad==400]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="401")]<-quads$Lat[quads$Quad==401]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="402")]<-quads$Lat[quads$Quad==402]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="404")]<-quads$Lat[quads$Quad==404]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="407")]<-quads$Lat[quads$Quad==407]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="408")]<-quads$Lat[quads$Quad==408]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="409")]<-quads$Lat[quads$Quad==409]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="410")]<-quads$Lat[quads$Quad==410]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="411")]<-quads$Lat[quads$Quad==411]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="412")]<-quads$Lat[quads$Quad==412]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="413")]<-quads$Lat[quads$Quad==413]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="414")]<-quads$Lat[quads$Quad==414]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="415")]<-quads$Lat[quads$Quad==415]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="416")]<-quads$Lat[quads$Quad==416]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="417")]<-quads$Lat[quads$Quad==417]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="418")]<-quads$Lat[quads$Quad==418]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="419")]<-quads$Lat[quads$Quad==419]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="420")]<-quads$Lat[quads$Quad==420]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="421")]<-quads$Lat[quads$Quad==421]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="422")]<-quads$Lat[quads$Quad==422]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="426")]<-quads$Lat[quads$Quad==426]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="427")]<-quads$Lat[quads$Quad==427]
searsd$ActLat[which(is.na(searsd$ActLat) & searsd$Quadrant=="442")]<-quads$Lat[quads$Quad==442]

searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="188")]<-quads$Long[quads$Quad==188]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="388")]<-quads$Long[quads$Quad==388]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="394")]<-quads$Long[quads$Quad==394]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="397")]<-quads$Long[quads$Quad==397]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="398")]<-quads$Long[quads$Quad==398]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="400")]<-quads$Long[quads$Quad==400]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="402")]<-quads$Long[quads$Quad==402]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="403")]<-quads$Long[quads$Quad==403]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="404")]<-quads$Long[quads$Quad==404]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="407")]<-quads$Long[quads$Quad==407]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="408")]<-quads$Long[quads$Quad==408]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="410")]<-quads$Long[quads$Quad==410]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="412")]<-quads$Long[quads$Quad==412]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="413")]<-quads$Long[quads$Quad==413]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="414")]<-quads$Long[quads$Quad==414]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="415")]<-quads$Long[quads$Quad==415]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="416")]<-quads$Long[quads$Quad==416]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="417")]<-quads$Long[quads$Quad==417]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="418")]<-quads$Long[quads$Quad==418]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="419")]<-quads$Long[quads$Quad==419]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="420")]<-quads$Long[quads$Quad==420]
searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="422")]<-quads$Long[quads$Quad==422]

searsd$ActLong[which(searsd$ActLong==0.0000 & searsd$Quadrant=="442")]<-quads$Long[quads$Quad==442]

searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="176")]<-quads$Long[quads$Quad==176]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="394")]<-quads$Long[quads$Quad==394]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="395")]<-quads$Long[quads$Quad==395]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="396")]<-quads$Long[quads$Quad==396]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="397")]<-quads$Long[quads$Quad==397]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="398")]<-quads$Long[quads$Quad==398]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="400")]<-quads$Long[quads$Quad==400]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="401")]<-quads$Long[quads$Quad==401]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="402")]<-quads$Long[quads$Quad==402]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="404")]<-quads$Long[quads$Quad==404]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="407")]<-quads$Long[quads$Quad==407]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="408")]<-quads$Long[quads$Quad==408]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="409")]<-quads$Long[quads$Quad==409]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="410")]<-quads$Long[quads$Quad==410]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="411")]<-quads$Long[quads$Quad==411]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="412")]<-quads$Long[quads$Quad==412]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="413")]<-quads$Long[quads$Quad==413]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="414")]<-quads$Long[quads$Quad==414]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="415")]<-quads$Long[quads$Quad==415]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="416")]<-quads$Long[quads$Quad==416]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="417")]<-quads$Long[quads$Quad==417]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="418")]<-quads$Long[quads$Quad==418]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="419")]<-quads$Long[quads$Quad==419]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="420")]<-quads$Long[quads$Quad==420]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="421")]<-quads$Long[quads$Quad==421]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="422")]<-quads$Long[quads$Quad==422]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="426")]<-quads$Long[quads$Quad==426]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="427")]<-quads$Long[quads$Quad==427]
searsd$ActLong[which(is.na(searsd$ActLong) & searsd$Quadrant=="442")]<-quads$Long[quads$Quad==442]

#4. #Create a new column that combines Pod and Likely Pod column and removes spaces
searsd$Pod.cl<-searsd$Pod

#Always use Likely Pod column, when it is not blank:
searsd$Pod.cl[searsd$LikelyPod!=""]<-searsd$LikelyPod[searsd$LikelyPod!=""]

#5. Add month, week and day of year (day)

searsd$day<-strftime(strptime(searsd$Sight.Date,format= "%m/%d/%y"),format= "%j")
searsd$week<-strftime(strptime(searsd$Sight.Date,format= "%m/%d/%y"), format = "%V")#new weeks start on mondays
searsd$date<-strftime(strptime(searsd$Sight.Date,format= "%m/%d/%y"),format= "%m/%d/%Y")
searsd$year<-substr(searsd$date,nchar(searsd$date)-3,nchar(searsd$date))
searsd$month<-substr(searsd$date,1,2)



