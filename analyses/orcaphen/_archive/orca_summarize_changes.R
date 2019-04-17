#Script to make figure of change over time by pod and season
#By Ailene
#Started 29 January 2019
#For now, using model output gathered "by hand" into a csv
#i need to automate this so that i can load the model results, then get estimates
rm(list=ls())
options(stringsAsFactors = FALSE)
setwd("~/Documents/GitHub/fishphen")

# Load libraries
library(R2jags)
library(scales)

sums<-read.csv("analyses/output/modsums.csv", header=TRUE)

quartz()
y<-as.numeric(sums[4,2:7])
x<-c(1.25,1.75,3.25,3.75,5.25,5.75)
colors<-c("darkred","lightblue")
shape=c(21,22,24)
plot(x,y,pch=shape[factor(sums[1,2:7])], bg=colors[factor(sums[3,2:7])],ylab="Change in days/year", xlab="Pod", ylim=c(-2.5, 1.5), xaxt="n")
arrows(x,as.numeric(sums[5,2:7]),x,as.numeric(sums[6,2:7]), length=0.05, angle=90, code=3)
points(x,y,pch=shape[factor(sums[1,2:7])], bg=colors[factor(sums[3,2:7])])
abline(h=0, lty=2)
axis(side=1,at=c(1.5,3.5,5.5), labels=c("J","K","L"))


#Look at od vs even years

cbind(phen.yr.all.doy$year,phen.yr.all.doy$pk.doy)
phen.yr.all.doy$yeartype<-"odd"
phen.yr.all.doy$yeartype[(phen.yr.all.doy$year %% 2)==0]<-"even"
#fit a gam to weekly data
#Is there a difference bewteen odd and even years?
oddeven.mod<-lm(pk.doy~as.factor(yeartype), data=phen.yr.all.doy)
summary(oddeven.mod)#no!
odd<-phen.yr.all.doy[phen.yr.all.doy$yeartype=="odd",]
even<-phen.yr.all.doy[phen.yr.all.doy$yeartype=="even",]
t.test(odd$pk.doy,even$pk.doy[1:length(odd$pk.doy)], paired=TRUE)
#No difference in odd versus even years
boxplot(pk.doy~as.factor(yeartype), data=phen.yr.all.doy)
