#Simulate orca data and vary number of observations to see if increasing effort yields shift in estimate of first, peak, or last observation day.

#Create a dataset with a highest probability of seeing the whales in the summer, lowest in the spring and fall
#explore how chaning the number of observations changes our estimates of first, last, peak whale presence
#probability of seeing a whale depends on day of year
#for days 1:180, probability is low (0.10); for days 181-260, proability is higher (0.75); and for days 261-366, probabilty is quite low again
springprob<-0.10
summerprob<-0.75
fallprob<-0.20
springdays<-seq(1,180, by=1)
summerdays<-seq(181,260, by=1)
falldays<-seq(261,366, by=1)

springobs<-sample(c(0,1), length(springdays), replace=TRUE, prob = c(1-springprob,springprob))
summerobs<-sample(c(0,1), length(summerdays), replace=TRUE, prob = c(1-summerprob,summerprob))
fallobs<-sample(c(0,1), length(falldays), replace=TRUE, prob = c(1-fallprob,fallprob))
#put together the true data
days<-c(springdays,summerdays,falldays)
whaleobs<-c(springobs,summerobs,fallobs)

whalepres<-cbind(days, whaleobs)
rownames(whalepres)<-NULL
#now, draw random samples from the whale presence dataset, assuming that effort is evenly distributed across the year
nobs<-100
obs<-as.data.frame(whalepres[sample(nrow(whalepres), replace=TRUE,nobs), ])
firstobs<-min(obs$days[obs$whaleobs==1])
lastobsobs<-max(obs$days[obs$whaleobs==1])
meanobs<-mean(obs$days[obs$whaleobs==1])


#Look at effect of increasing effort on estimate of first observation and estimate of last observation
nreps<-10#look at effect of increasing observations on estimates

lowobs<-10#number of days whales observed in 1970s
highobs<-30#number of days whales observed in 2000

df <- as.data.frame(matrix(nrow=length(numobs)*nreps,ncol=7))
colnames(df)<-c("nobs","firstest","lastest","meanest","firsttrue", "lasttrue","meantrue") 
numobs<-c(rep(lowobs,nreps),rep(highobs,nreps))
for (i in 1:length(numobs)){
  springobs<-sample(c(0,1), length(springdays), replace=TRUE, prob = c(1-springprob,springprob))
  summerobs<-sample(c(0,1), length(summerdays), replace=TRUE, prob = c(1-summerprob,summerprob))
  fallobs<-sample(c(0,1), length(falldays), replace=TRUE, prob = c(1-fallprob,fallprob))
  #put together the true data
  days<-c(springdays,summerdays,falldays)
  whaleobs<-c(springobs,summerobs,fallobs)
  whalepres<-as.data.frame(cbind(days, whaleobs))
  obs<-as.data.frame(whalepres[sample(nrow(whalepres), replace=TRUE,numobs[i]), ])
  df$nobs[i]<-numobs[i]
  df$firstest[i]<- min(obs$days[obs$whaleobs==1])
  df$lastest[i]<-max(obs$days[obs$whaleobs==1])
  df$meanest[i]<-mean(obs$days[obs$whaleobs==1])
  df$firsttrue[i]<-min(whalepres$days[whalepres$whaleobs==1])
  df$lasttrue[i]<-max(whalepres$days[whalepres$whaleobs==1])
  df$meantrue[i]<-mean(whalepres$days[whalepres$whaleobs==1])
}
quartz()
par(mfrow=c(1,3))
#First obs
boxplot(df$firstest~as.factor(df$nobs), xlab="Number of days observed", ylab="Estimate of first obs (doy)", main="First obs")
t<-t.test(df$firstest~as.factor(df$nobs), conf.level=0.95)
mtext(paste("Change=",round(t$estimate[1]-t$estimate[2], digits=2),"(",round(t$conf.int[1],digits=2),",",round(t$conf.int[2],digits=2),")", sep=""),side=3,line=-3, adj=1)
#Last obs
boxplot(df$lastest~as.factor(df$nobs), xlab="Number of days observed", ylab="Estimate of last obs (doy)", main="Last obs")
t<-t.test(df$lastest~as.factor(df$nobs), conf.level=0.95)
mtext(paste("Change=",round(t$estimate[1]-t$estimate[2], digits=2),"(",round(t$conf.int[1],digits=2),",",round(t$conf.int[2],digits=2),")", sep=""),side=3,line=-3, adj=1)

#Mean obs
boxplot(df$meanest~as.factor(df$nobs), xlab="Number of days observed", ylab="Estimate of mean obs (doy)", main="Mean obs")
t<-t.test(df$meanest~as.factor(df$nobs), conf.level=0.95)
mtext(paste("Change=",round(t$estimate[1]-t$estimate[2], digits=2),"(",round(t$conf.int[1],digits=2),",",round(t$conf.int[2],digits=2),")", sep=""),side=3,line=-3, adj=1)

