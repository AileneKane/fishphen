#Code for Cleaning OrcaMaster Database
#housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/GitHub/fishphen")

# Load libraries
library(mgcv)

# 1. Get the data
d <- read.csv("data/AppendixII.csv")

# 2. Clean the Pod column

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
d$Pod[d$Pod=="SR"|d$Pod=="sRs"|d$Pod=="S"|d$Pod=="SWKW"]<-"SRs"

# 3. Clean the Likely Pod column

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

#4. Clean Year column
d$Year[d$Year==0]<-2001

#5. Create a new cleaned datafile
write.csv(d,"analyses/output/AppendixII_cleaned.csv")
getwd()
