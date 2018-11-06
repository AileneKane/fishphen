#Code for Cleaning OrcaMaster Database
#housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/GitHub/fishphen")

# Load libraries

# 1. Get the data
d <- read.csv("data/AppendixII.csv")

# 2. Clean the Pod column
#Create a new column that combines Pod and Likely Pod columna and removes spaces
d$Pod.cl<-d$Pod


#Not sure if I should always preferentialy use Likely Pod column? For now I'm only doing this in particular cases...
#d$Pod.cl[d$Pod.cl=="Orcas"|d$Pod.cl=="orcas"|d$Pod.cl=="Orca"|d$Pod.cl=="orca"]<-d$LikelyPod[d$Pod.cl=="Orcas"|d$Pod.cl=="orcas"|d$Pod.cl=="Orca"|d$Pod.cl=="orca"]
#d$Pod.cl[d$LikelyPod=="K"|d$LikelyPod=="K"|d$LikelyPod=="K?"|d$LikelyPod=="Ks?"]<-"K"
#d$Pod.cl[d$LikelyPod=="L"|d$LikelyPod=="L?"|d$LikelyPod=="L "|d$LikelyPod=="Lp"|d$LikelyPod=="L+"]<-"L"
#d$Pod.cl[d$LikelyPod=="J"|d$LikelyPod=="j"|d$LikelyPod=="J "|d$LikelyPod=="J  "| d$LikelyPod=="J  "|d$LikelyPod=="J?"|d$LikelyPod=="Jp"|d$LikelyPod=="Jp "|d$LikelyPod=="Jp?"|d$LikelyPod=="Jp+"|d$LikelyPod=="J+"]<-"J"
#d$Pod.cl[d$LikelyPod=="JK"|d$LikelyPod=="JKp"|d$LikelyPod=="JpK"]<-"JK"
#d$Pod.cl[d$LikelyPod=="KL"|d$LikelyPod=="KLp"|d$LikelyPod=="KpLp"]<-"KL"
#d$Pod.cl[d$LikelyPod=="T"|d$LikelyPod=="Ts"|d$LikelyPod=="Ts "|d$LikelyPod=="Ts?"]<-"T"
#d$Pod.cl[d$LikelyPod=="K"|d$LikelyPod=="K"|d$LikelyPod=="K?"|d$LikelyPod=="Ks?"]<-"K"
#d$Pod.cl[d$LikelyPod=="L"|d$LikelyPod=="L?"|d$LikelyPod=="L "|d$LikelyPod=="Lp"|d$LikelyPod=="L+"]<-"L"
#d$Pod.cl[d$LikelyPod=="J"|d$LikelyPod=="j"|d$LikelyPod=="J "|d$LikelyPod=="J  "| d$LikelyPod=="J  "|d$LikelyPod=="J?"|d$LikelyPod=="Jp"|d$LikelyPod=="Jp "|d$LikelyPod=="Jp?"|d$LikelyPod=="Jp+"|d$LikelyPod=="J+"]<-"J"
#d$Pod.cl[d$LikelyPod=="JK"|d$LikelyPod=="JKp"|d$LikelyPod=="JpK"]<-"JK"
#d$Pod.cl[d$LikelyPod=="KL"|d$LikelyPod=="KLp"|d$LikelyPod=="KpLp"]<-"KL"
#d$Pod.cl[d$LikelyPod=="T"|d$LikelyPod=="Ts"|d$LikelyPod=="Ts "|d$LikelyPod=="Ts?"]<-"T"

#Always use Likely Pod column, when it is not blank:
d$Pod.cl[d$LikelyPod!="" & d$LikelyPod!=" "]<-d$LikelyPod[d$LikelyPod!="" & d$LikelyPod!=" "]

d$Pod.cl[d$Pod.cl=="j"|d$Pod.cl==" J"|d$Pod.cl=="J "|d$Pod.cl=="J  "|d$Pod.cl=="J   "|d$Pod.cl=="J?"|d$Pod.cl=="J "|d$Pod.cl=="J  "|d$Pod.cl=="J+"|d$Pod.cl=="Jp"|d$Pod.cl=="Jp "|d$Pod.cl=="Jp  "|d$Pod.cl=="Jp?"| d$Pod.cl=="Js"]<-"J"
d$Pod.cl[d$Pod.cl=="J1 "]<-"J1"
d$Pod.cl[d$Pod.cl=="K "|d$Pod.cl=="K  "|d$Pod.cl=="K?"|d$Pod.cl=="K+"|d$Pod.cl=="Kp"|d$Pod.cl=="KP"|d$Pod.cl=="Kp  "|d$Pod.cl=="Kp  "]<-"K"
d$Pod.cl[d$Pod.cl=="L "|d$Pod.cl=="L  "|d$Pod.cl=="L?"|d$Pod.cl=="L+"|d$Pod.cl=="L+?"|d$Pod.cl=="LP"|d$Pod.cl=="Lp  "|d$Pod.cl=="Lp "|d$Pod.cl=="Lp?"|d$Pod.cl=="Ls"|d$Pod.cl=="Ls?"]<-"L"
d$Pod.cl[d$Pod.cl=="JK "|d$Pod.cl=="JK  "|d$Pod.cl=="JK   "|d$Pod.cl=="J?K?"|d$Pod.cl=="KJ?"|d$Pod.cl=="JK+"|d$Pod.cl=="JKp+"|d$LikelyPod=="Jp+K?"]<-"JK"
d$Pod.cl[d$Pod.cl=="JL "|d$Pod.cl=="JL  "|d$Pod.cl=="JL   "|d$Pod.cl=="JLp? "|d$Pod.cl=="JpLp?"|d$Pod.cl=="JLp"]<-"JL"
d$Pod.cl[d$Pod.cl=="KL "|d$Pod.cl=="KL  "|d$Pod.cl=="KL?"|d$Pod.cl=="KL+? "|d$Pod.cl=="KpLp"|d$Pod.cl=="KpL"|d$Pod.cl=="KpL "|d$Pod.cl=="KpL  "|d$Pod.cl=="KpLp?"|d$Pod.cl=="LK"|d$Pod.cl=="LK?"]<-"KL"
d$Pod.cl[d$Pod.cl=="JL "|d$Pod.cl=="JL  "|d$Pod.cl=="JL   "|d$Pod.cl=="JLp? "|d$Pod.cl=="JpLp?"|d$Pod.cl=="JLp"]<-"JL"
d$Pod.cl[d$Pod.cl=="JKLp"|d$Pod.cl=="JKLm"|d$Pod.cl=="JKl"|d$Pod.cl=="JKL  "|d$Pod.cl=="JKL?"|d$Pod.cl=="JKLm"|d$Pod.cl=="JpKL"|d$Pod.cl=="JKLp"]<-"JKL"
d$Pod.cl[d$Pod.cl=="Ts"|d$Pod.cl=="Ts?"|d$Pod.cl=="Ts "]<-"T"
d$Pod.cl[d$Pod.cl=="Orca"|d$Pod.cl=="orca"|d$Pod.cl=="orcas"]<-"Orcas"
d$Pod.cl[d$Pod.cl=="SR"|d$Pod.cl=="sRs"|d$Pod.cl=="SRs?"]<-"SRs"


#3. Add a column for presences (1/0) for each pod

unique(d$Source)

