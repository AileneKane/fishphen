#code snippet that Istarted (but doesn't work yet) to calculate consecutive days of SRKW observations

##consecutive days
df <- orcasum.days[order(orcasum.days$region,orcasum.days$date),] 
df <- df[!duplicated(df[,c("region","date")]),]
df$last_Date <- as.Date(c("1979-08-16 ",df[1:nrow(df)-1,]$date))
df$Date <- as.Date(df$date)
df$diff <- df$Date - df$last_Date 
df$last_region <- c(0,df[1:nrow(df)-1,]$region)
df$diff_region <- ifelse(df$region == df$last_region,0,1)
df$flag <- ifelse(df$diff==1 & df$diff_region==0,0,1)
consecutive_count <- function(x)  {
  x <- !x
  rl <- rle(x)
  len <- rl$lengths
  v <- rl$values
  cumLen <- cumsum(len)
  z <- x
  # replace the 0 at the end of each zero-block in z by the 
  # negative of the length of the preceding 1-block....
  iDrops <- c(0, diff(v)) < 0
  z[ cumLen[ iDrops ] ] <- -len[ c(iDrops[-1],FALSE) ]
  # ... to ensure that the cumsum below does the right thing.
  # We zap the cumsum with x so only the cumsums for the 1-blocks survive:
  x*cumsum(z)
}
df$Consecutive <- consecutive_count(df$flag)

podcols<-c("Jpres", "Kpres", "Lpres", "AllSRpres")
pods<-c("J","K","L","SRs")

if (r=="ps"){
  regdat<-regdat[as.numeric(regdat$day)>273,]#look at data only after Sept 30 for PS
}
boxplot(as.numeric(regdat$day)~as.factor(regdat$year),
        horizontal=TRUE,las=1,
        ylab="year",xlab="DOY",main=paste(r))

if(r=="uss"){mtext(paste(pods[p]), side=3,line=2, adj=0.5)}

}
}


##Number of consecutive days
doy<- strptime(paste(orcasum.days$day,orcasum.days$year, sep="-"),format = "%j")
orcasum.days$date<-paste(orcasum.days$year,substr(doy,6,10), sep="-")
runchecker <- function(data, days){
  data %>% arrange(date) %>%
    group_by(AllSRpres) %>%
    mutate(diff = c(0, diff(date)),
           periodID = 1 + cumsum(diff > days)) %>%
    group_by(ID, periodID) %>%
    summarise(days = last(date) - first(date))
}
#p=4 for "ALLSRpres"
for(p in 1:length(podcols)){
  quartz(width=15,height=6)
  par(mfrow=c(1,3))
  colnum<-which(colnames(orcasum.days)==podcols[p])
  regions=unique(orcasum.days$region)
  for(r in regions){
    regdat<-orcasum.days[orcasum.days$region==r,]
    orcaobs<-subset(regdat, select=c(AllSRpres,date))]
orcaobs$date<-as.Date(orcaobs$date)
runchecker(orcaobs,1)
runchecker(orcaobs$AllSRpres,)
  }
}
