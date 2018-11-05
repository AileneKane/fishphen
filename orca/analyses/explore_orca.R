#Exploring the orcamaster data frmo 2017
#some housekeeping code first:
rm(list=ls()) 
options(stringsAsFactors = FALSE)
setwd("~/git/orca")
d<-read.csv("data/2017DataOnly.csv", header=TRUE)
head(d)
sort(unique(d$SightDate))
dim(d)
#rows2113
#daysofyear 214
length(unique(d$SightDate))
unique(d$Pod)
#podsseen33
unique(d$FishArea)
#seenin 32 areas?
sort(numeric(d$FishArea))
table(d$FishArea,d$Month)
#mostsightingsarea 10,11,13,19,19C,20C,7,9
#mostsightingsmonth 3,5,6,9,12
#podsmostseen J,5 JK,12 JL,9 L,6-9
table(d$Pod,d$Month)
table(d$Pod,d$FishArea)
#areasandpods J-7&9&19C, JK-10&11&7&9, JKL-7, Jp ,K-10,KL-10, L-19, JKp ,JL
#podareamost J., JK., JKL., JKp., JL, JLp, Jp, K, KL, L, L12s, Lp, 