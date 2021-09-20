#Function for getting first, last, peak occupancy prob from a dataframe
#with daily prob of occurrenc across multiple years

get.gests<-function(limegests,occprobs){
  peakoc.doy<-c()
  peakoc<-c()
  firstprob<-c()
  lastprob<-c()
  meanprobs<-c()
  prob.lc<-c()
  prob.uc<-c()
  years<-c()
  for(y in unique(limegests$year)){
    yeardat<-limegests[limegests$year==y,]
    occprobcol<-yeardat[,which(colnames(yeardat)==occprobs)]
    yrpeakoc<-max(occprobcol)
    yrpeakoc.doy<-yeardat$doy[which(occprobcol==yrpeakoc)]
    yrfirst.doy<-yeardat$doy[min(which(occprobcol>0.1))]
    yrlast.doy<-yeardat$doy[max(which(occprobcol>0.1))]
    meanprob<-mean(occprobcol)
    lprob<-quantile(occprobcol,0.25)
    uprob<-quantile(occprobcol,0.75)
    peakoc<-c(peakoc,yrpeakoc)
    peakoc.doy<-c(peakoc.doy,yrpeakoc.doy)
    firstprob<-c(firstprob,yrfirst.doy)
    lastprob<-c(lastprob,yrlast.doy)
    meanprobs<-c(meanprobs,meanprob)
    prob.lc<-c(prob.lc,lprob)
    prob.uc<-c(prob.uc,uprob)
    years<-c(years,y)
  }
  gests<-as.data.frame(cbind(years,peakoc,peakoc.doy,firstprob,lastprob,meanprobs,prob.lc,prob.uc))
  row.names(gests)<-NULL
  return(gests)
}

#modifying above function for salmon cpue
get.gests.chin<-function(gests,cpue){#occprobs=cpue
  peakcpue.doy<-c()
  peakcpue<-c()
  firstcpue<-c()
  lastcpue<-c()
  meancpues<-c()
  totalcpues<-c()
  cpue.lc<-c()
  cpue.uc<-c()
  years<-c()
  for(y in unique(gests$year)){
    yeardat<-gests[gests$year==y,]
    cpuecol<-yeardat[,which(colnames(yeardat)==cpue)]
    yrpeak<-max(cpuecol)
    yrpeak.doy<-yeardat$doy[which(cpuecol==yrpeak)]
    yrfirst.doy<-yeardat$doy[min(which(cpuecol>0))]
    yrlast.doy<-yeardat$doy[max(which(cpuecol>0))]
    meancpue<-mean(cpuecol)
    totalcpue<-sum(cpuecol)
    lprob<-quantile(cpuecol,0.25)
    uprob<-quantile(cpuecol,0.75)
    peakcpue<-c(peakcpue,yrpeak)
    peakcpue.doy<-c(peakcpue.doy,yrpeak.doy)
    firstcpue<-c(firstcpue,yrfirst.doy)
    lastcpue<-c(lastcpue,yrlast.doy)
    meancpues<-c(meancpues,meancpue)
    totalcpues<-c(totalcpues,totalcpue)
    cpue.lc<-c(cpue.lc,lprob)
    cpue.uc<-c(cpue.uc,uprob)
    years<-c(years,y)
  }
  gests<-as.data.frame(cbind(years,peakcpue,peakcpue.doy,firstcpue,lastcpue,meancpue,cpue.lc,cpue.uc,totalcpues))
   row.names(gests)<-NULL
  return(gests)
}


