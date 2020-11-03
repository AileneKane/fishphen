#Limit space and time of interest:
#1. Space: only using fishing areas in Salish Sea- includes WAshington and Canada
if(includeCanada==TRUE & use3regions==FALSE){d<-d[d$FishArea %in% c("01","02","03","04","05","06","07","09","10","11","12","13","81","82","16C","17C","18C","19C","20C","29C"),]}
if(includeCanada==FALSE& use3regions==FALSE){d<-d[d$FishArea %in% c("01","02","03","04","05","06","07","09","10","11","12","13","81","82"),]}

if(includeCanada==TRUE & use3regions==TRUE){d<-d[d$FishArea %in% c("01","02","03","04","05","06","07","09","10","11","12","13","81","82","16C","17C","18C","19C","20C","21C","22C","29C","121C"),]}
if(includeCanada==FALSE & use3regions==TRUE){d<-d[d$FishArea %in% c("01","02","03","04","05","06","07","09","10","11","12","13","81","82"),]}

#remove sites with no fishing area:
d<-d[!d$FishArea %in% c(""),]

#2.#Assign region, based on fishing area:
if(includeCanada==TRUE & use3regions==FALSE){
  d$region<-"ps"
  d$region[d$FishArea=="04"|d$FishArea=="05"|d$FishArea=="06"|d$FishArea=="07"|d$FishArea== "16C"|d$FishArea=="17C"|d$FishArea== "18C"|d$FishArea=="19C"|d$FishArea=="20C"|d$FishArea=="29C"]<-"uss"
  d$region[d$FishArea=="01"|d$FishArea=="02"|d$FishArea=="03"]<-"oc"#outer coast
}

if(includeCanada==TRUE & use3regions==TRUE){
  
  d$region<-"ps"
  d$region[d$FishArea=="06"|d$FishArea=="07"|d$FishArea== "16C"|d$FishArea=="17C"|d$FishArea== "18C"|d$FishArea=="19C"|d$FishArea=="29C"]<-"uss"
  d$region[d$FishArea=="01"|d$FishArea=="02"|d$FishArea=="03"]<-"oc"#outer coast
  d$region[d$FishArea=="04"|d$FishArea=="05"|d$FishArea=="20C"|d$FishArea=="21C"|d$FishArea=="22C"|d$FishArea=="121C"]<-"jf"#straight of juan de fuca
}

#3. Limit to after desired year (Olson use 1976)

d<-d[d$Year>=firstyear,]


#4. Only keep sites north of Washington state southern border
d<-d[as.numeric(d$Lat)>46.9,]
##check that this lines up with olson et al numbers
#tapply(d$SightDate[d$Year>1975 & d$Year<2015],d$Source[d$Year>1975 & d$Year<2015],length)
#to get their Lime Kiln Station sightings data (the below is after limitspacetime):
#BCCSN        SPOT TWM-HYD-Pub TWM-HYD-Rel    TWM-Otis   TWM-Pager  TWM-SA-Pub  TWM-SA-Rel   TWM-SA-WW 
#9333        8360        1011        2377        1844       18887       13418       15507        7787 
#TWM-SW 
#13100 
#tapply(d$SightDate[d$Year>1975 & d$Year<2015 & d$Quadrant == 18],d$Source[d$Year>1975 & d$Year<2015 & d$Quadrant == 18],length)
# BCCSN TWM-SA-Pub TWM-SA-Rel 
#1          1          2 
#I think some of the difference in whale days is due to this part- throw out some data without fishing area when i should
#be able to include it! with lat/longs. Olson et al use the Quadrants...
##d$year.doy<-pa  ste(d$Year,d$day,sep=".")
#length(unique(d$year.doy))#8447

