#Code for Generating File of observations that have lat/long but need fishing area entered
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/GitHub/fishphen")

# Load libraries
# 1. Get the data
d <- read.csv("data/AppendixII.csv")

# 2. Clean the data (also saved in output/AppendixII_cleaned,csv)
source("analyses/clean_orca.R")

#Create a new column that combines Pod and Likely Pod columna and removes spaces
  d$Pod.cl<-d$Pod


#Always use Likely Pod column, when it is not blank:
d$Pod.cl[d$LikelyPod!="" & d$LikelyPod!=" "]<-d$LikelyPod[d$LikelyPod!="" & d$LikelyPod!=" "]
#perhaps also stick with Pod when LikelyPod has a "?" grep("?",d$LikelyPod,)

d$Pod.cl[d$Pod.cl=="j"|d$Pod.cl==" J"|d$Pod.cl=="J "|d$Pod.cl=="J  "|d$Pod.cl=="J   "|d$Pod.cl=="J?"|d$Pod.cl=="J "|d$Pod.cl=="J  "|d$Pod.cl=="J+"|d$Pod.cl=="Jp"|d$Pod.cl=="Jp "|d$Pod.cl=="Jp  "|d$Pod.cl=="Jp?"| d$Pod.cl=="Js"]<-"J"
d$Pod.cl[d$Pod.cl=="J1 "]<-"J1"
d$Pod.cl[d$Pod.cl=="K "|d$Pod.cl=="K  "|d$Pod.cl=="K?"|d$Pod.cl=="K+"|d$Pod.cl=="Kp"|d$Pod.cl=="KP"|d$Pod.cl=="Kp  "|d$Pod.cl=="Kp  "]<-"K"
d$Pod.cl[d$Pod.cl=="L "|d$Pod.cl=="L  "|d$Pod.cl=="L?"|d$Pod.cl=="L+"|d$Pod.cl=="L+?"|d$Pod.cl=="LP"|d$Pod.cl=="Lp  "|d$Pod.cl=="Lp "|d$Pod.cl=="Lp?"|d$Pod.cl=="Ls"|d$Pod.cl=="Ls?"]<-"L"
d$Pod.cl[d$Pod.cl=="JK "|d$Pod.cl=="JK  "|d$Pod.cl=="JK   "|d$Pod.cl=="J?K?"|d$Pod.cl=="KJ?"|d$Pod.cl=="JK+"|d$Pod.cl=="JKp+"|d$LikelyPod=="Jp+K?"]<-"JK"
d$Pod.cl[d$Pod.cl=="JL "|d$Pod.cl=="JL  "|d$Pod.cl=="JL   "|d$Pod.cl=="JLp? "|d$Pod.cl=="JpLp?"|d$Pod.cl=="JLp"]<-"JL"
d$Pod.cl[d$Pod.cl=="KL "|d$Pod.cl=="KL  "|d$Pod.cl=="KL?"|d$Pod.cl=="KL+? "|d$Pod.cl=="KpLp"|d$Pod.cl=="KpL"|d$Pod.cl=="KpL "|d$Pod.cl=="KpL  "|d$Pod.cl=="KpLp?"|d$Pod.cl=="LK"|d$Pod.cl=="LK?"]<-"KL"
d$Pod.cl[d$Pod.cl=="JL "|d$Pod.cl=="JL  "|d$Pod.cl=="JL   "|d$Pod.cl=="JLp? "|d$Pod.cl=="JpLp?"|d$Pod.cl=="JLp"]<-"JL"
d$Pod.cl[d$Pod.cl=="JKLp"|d$Pod.cl=="JKLm"|d$Pod.cl=="JKl"|d$Pod.cl=="JKL  "|d$Pod.cl=="JKL?"|d$Pod.cl=="JKLm"|d$Pod.cl=="JpKL"|d$Pod.cl=="JKLp"]<-"JKL"

d$Pod.cl[d$Pod.cl=="O?"|d$Pod.cl=="Ts?"]<-"Ts"



sort(unique(d$Pod.cl))

#remove non-orca data
d<-d[d$Pod.cl!="HB?"|d$Pod.cl!="Not Orcas",]


#4.  Add a column for presences (1/0) for each pod, for Ts, and for SRKWs

d$J<-0
d$J[grep("J",d$Pod.cl)]<- 1
d$K<-0
d$K[grep("K",d$Pod.cl)]<- 1
d$L<-0
d$L[grep("L",d$Pod.cl)]<- 1
d$SRKW<-0
d$SRKW[grep("SR",d$Pod.cl)]<- 1
d$SRKW[d$J==1|d$K==1|d$L==1]<- 1   

#select out the Washington sites for which we have a lat/long but no fishing area

dlat<-d[d$FishArea=="" & d$SRKW==1,]
dlat<-dlat[as.numeric(dlat$ActLat)>46,]
dlat<-dlat[as.numeric(dlat$ActLat)<49,]
dlat<-dlat[as.numeric(dlat$ActLong)<125,]

#330 rows have no Fishing area, even though they may have a lat/long
#sort by Pod or Likely Pod
dlat<-dlat[order(as.numeric(dlat$ActLat)),]
dlat<-subset(dlat, select=c(ActLat,ActLong))
write.csv(dlat,"analyses/output/needfisharea.csv", row.names=FALSE)
head(dlat)