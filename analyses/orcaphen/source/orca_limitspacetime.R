#Limit space and time of interest:

#1. Space: only using fishing areas in Salish Sea- includes WAshington and Canada
if(includeCanada==TRUE)d<-d[d$FishArea %in% c("01","02","03","04","05","06","07","09","10","11","12","13","81","82","19C","18C","29C","20C"),]#not sure where 17, 18, 19, 20, 28, 29 are...need to find out. also, where is 42583,42584
if(includeCanada==FALSE)d<-d[d$FishArea %in% c("01","02","03","04","05","06","07","09","10","11","12","13","81","82"),]

#remove sites with no fishing area:
d<-d[!d$FishArea %in% c(""),]

#2.#Assign region, based on fishing area:
d$region<-"ps"
d$region[d$FishArea=="07"|d$FishArea=="06"|d$FishArea=="05"|d$FishArea=="04"|d$FishArea=="19C"|d$FishArea== "18C"|d$FishArea=="20C"|d$FishArea=="29C"]<-"uss"
d$region[d$FishArea=="01"|d$FishArea=="02"|d$FishArea=="03"]<-"oc"#outer coast

#3. Limit to after desired year (Olson use 1976)

d<-d[d$Year>=firstyear,]
