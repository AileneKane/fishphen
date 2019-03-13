#Simulate orca data and vary number of observations to see if increasing effort yields shift in estimate of first, peak, or last observation day.
wholeyear=FALSE#TRUE If you want the test data to be year-round (with peak prob in the summer)
                #FALSE If you want the test data to be fall only (i.e. after day 273, like puget sound, with peak  
biasearly=TRUE#TRUE if you want observations to be biased early in the season
              #FALSE if you want observations to be unbiased

if(wholeyear==TRUE){
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
}
if(wholeyear==FALSE){
  #Create a dataset with uniform probability of 0.5 for all days from 261-366
  fallprob<-0.5
  falldays<-seq(261,366, by=1)
  fallobs<-sample(c(0,1), length(falldays), replace=TRUE, prob = c(1-fallprob,fallprob))
  #put together the true data
  days<-falldays
  whaleobs<-fallobs
  
  whalepres<-cbind(days, whaleobs)
  rownames(whalepres)<-NULL
}



#now, draw random samples from the whale presence dataset, assuming that effort is evenly distributed across the year
nobs<-100
obs<-as.data.frame(whalepres[sample(nrow(whalepres), replace=TRUE,nobs), ])
firstobs<-min(obs$days[obs$whaleobs==1])
lastobsobs<-max(obs$days[obs$whaleobs==1])
meanobs<-mean(obs$days[obs$whaleobs==1])


#Look at effect of increasing effort on estimate of first observation and estimate of last observation
nreps<-10#

if(wholeyear=TRUE){
  lowobs<-90#mean whales days first 20 years: ps=13.5, uss=88
  highobs<-150#mean whales days last 20 years: ps=27.7, uss=146.5
}
if(wholeyear==FALSE){
lowobs<-13#mean whales days first 20 years: ps=13.5, uss=88
highobs<-30#mean whales days last 20 years: ps=27.7, uss=146.5
}
numobs<-c(rep(lowobs,nreps),rep(highobs,nreps))

 
bias=c(1.5,1.6,1.7,1.8,1.9,2)
for(b in bias){
  df <- as.data.frame(matrix(nrow=length(numobs)*nreps,ncol=7))
  colnames(df)<-c("nobs","firstest","lastest","meanest","firsttrue", "lasttrue","meantrue")
for (i in 1:length(numobs)){
  if(wholeyear==TRUE){
    springobs<-sample(c(0,1), length(springdays), replace=TRUE, prob = c(1-springprob,springprob))
    summerobs<-sample(c(0,1), length(summerdays), replace=TRUE, prob = c(1-summerprob,summerprob))
    fallobs<-sample(c(0,1), length(falldays), replace=TRUE, prob = c(1-fallprob,fallprob))
    #put together the true data
    days<-c(springdays,summerdays,falldays)
    whaleobs<-c(springobs,summerobs,fallobs)
    }
  if(wholeyear==FALSE){
    fallobs<-sample(c(0,1), length(falldays), replace=TRUE, prob = c(1-fallprob,fallprob))
    #put together the true data
    days<-falldays
    whaleobs<-fallobs
  }
  whalepres<-as.data.frame(cbind(days, whaleobs))
  obs<-as.data.frame(whalepres[sample(nrow(whalepres), replace=TRUE,numobs[i]), ])#random sample from whale true presence data
  if(biasearly==TRUE){
    unbiasprop<-14/nrow(whalepres)#first 2 weeks should be 0.14 of full season of observations
    bias=b #what proportion more observations occur in the first 2 weeks of observation than the rest of the study period
    biasprop<-unbiasprop*bias
    earlyobs<-as.data.frame(whalepres[sample(nrow(whalepres), replace=TRUE,biasprop*numobs[i]), ])
    otherobs<-as.data.frame(whalepres[sample(nrow(whalepres), replace=TRUE,(1-biasprop)*numobs[i]), ])
  obs<-as.data.frame(rbind(earlyobs,otherobs))

  }
  df$nobs[i]<-numobs[i]
  df$firstest[i]<- min(obs$days[obs$whaleobs==1])
  df$lastest[i]<-max(obs$days[obs$whaleobs==1])
  df$meanest[i]<-mean(obs$days[obs$whaleobs==1])
  df$firsttrue[i]<-min(whalepres$days[whalepres$whaleobs==1])
  df$lasttrue[i]<-max(whalepres$days[whalepres$whaleobs==1])
  df$meantrue[i]<-mean(whalepres$days[whalepres$whaleobs==1])
}
quartz()
par(mfrow=c(1,2))
#First obs)
boxplot(df$firstest~as.factor(df$nobs), xlab="Number of days observed", ylab="Estimate of first obs (doy)", main=paste("First obs",bias))
t<-t.test(df$firstest~as.factor(df$nobs), conf.level=0.95)
mtext(paste("Change=",-1*round(t$estimate[1]-t$estimate[2], digits=2),"(",-1*round(t$conf.int[1],digits=2),",",-1*round(t$conf.int[2],digits=2),")", sep=""),side=3,line=-3, adj=1)
#Last obs
boxplot(df$lastest~as.factor(df$nobs), xlab="Number of days observed", ylab="Estimate of last obs (doy)", main="Last obs")
t<-t.test(df$lastest~as.factor(df$nobs), conf.level=0.95)

mtext(paste("Change=",-1*round(t$estimate[1]-t$estimate[2], digits=2),"(",-1*round(t$conf.int[1],digits=2),",",-1*round(t$conf.int[2],digits=2),")", sep=""),side=1,line=-3, adj=1)
}

#Mean obs
boxplot(df$meanest~as.factor(df$nobs), xlab="Number of days observed", ylab="Estimate of mean obs (doy)", main="Mean obs")
t<-t.test(df$meanest~as.factor(df$nobs), conf.level=0.95)
mtext(paste("Change=",-1*round(t$estimate[1]-t$estimate[2], digits=2),"(",-1*round(t$conf.int[1],digits=2),",",-1*round(t$conf.int[2],digits=2),")", sep=""),side=1,line=-3, adj=1)

