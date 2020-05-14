#Code to analyse the fraser river test fishery data to get estimates of first, last, median dates of spring chinook runs
#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Set working directory: 
setwd("~/GitHub/fishphen")
#or from laptop:
#setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

# Load libraries
library(dplyr)
library(brms)
# 1. Read in the datafiles
source("analyses/salmonreturns/source/read_albiondat.R")
dim(d)#6148
unique(d$calDay)
d$calDay<-as.integer(as.character(d$calDay))
allyears<-unique(d$year)
dat<-d
season="allyear"#choices are "springsum" or "fall" or "allyear
head(dat)
dat<-dat[dat$year<2018,]
# 2. Compare albion data to CTC data
ctc<-read.csv("data/CTCEscapement.csv", header=TRUE)
ctc<-ctc[ctc$Year<2018 & ctc$Yea>1979,]
#select only spring and summer
datsp<-dat[dat$calDay<213,]
cpuesptot<-aggregate(datsp$cpue,list(datsp$year),sum, na.rm= TRUE)
cpuetot<-aggregate(dat$cpue,list(dat$year),sum, na.rm= TRUE)

colnames(cpuetot)<-c("year","cpuetot")
colnames(cpuesptot)<-c("year","cpuesptot")

cpuetot<-cpuetot[cpuetot$year<2018,]
cpuesptot<-cpuesptot[cpuesptot$year<2018,]

ctc$tot<-as.numeric(ctc$SpringSummerTotalRun)
ctc$sp12<-ctc$SpringAge1.2ctc$total<-ctc$SpringAge1.2+ctc$SpringAge1.3+ctc$SummerAge0.3+ctc$SummerAge1.3+ctc$Harrison_Esc+ctc$LowerShuswap_Esc
png(file="analyses/orcaphen/figures/ctcalbion.png",height=600,width=1500)
par(mfcol=c(2,4))
ctccols<-c(2,3,4,5)
for(i in ctccols){
  ctcnums<-ctc[ctc$Year>1990,]
  ctcnums<-ctcnums[,i]
  
plot(ctcnums,cpuesptot$cpuesptot[as.numeric(cpuetot$year)>1990], pch=21, xlab = "Escapement estimates", ylab="Albion Test Fishery CPUE (1April-1Aug)",bg="gray", main = paste(colnames(ctc)[i]))
mod<-lm(cpuesptot$cpuesptot[as.numeric(cpuesptot$year)>1990]~ctcnums)
if(summary(mod)$coef[2,4]<0.1){abline(mod, lwd=2)}
r<-cor.test(cpuesptot$cpuesptot[as.numeric(cpuesptot$year)>1990],ctcnums)
mtext(paste("cor = ",round(r$estimate,digits=2),", p = ",round (r$p.value, digits = 4)), side = 3,adj=0, line = -2, cex=1.1)
plot(ctcnums,cpuetot$cpuetot[as.numeric(cpuetot$year)>1990], pch=21, xlab = "Escapement estimates",  ylab="Albion Test Fishery CPUE (1April-20Oct)",bg="gray")
mod2<-lm(cpuetot$cpuetot[as.numeric(cpuetot$year)>1990]~ctcnums)
if(summary(mod)$coef[2,4]<0.1){abline(mod2, lwd=2)}
r<-cor.test(cpuetot$cpuetot[as.numeric(cpuetot$year)>1990],ctcnums)
mtext(paste("cor = ",round(r$estimate,digits=2),", p = ",round (r$p.value, digits = 4)), side = 3,adj=0, line = -2, cex=1.1)

}

dev.off()



#now fit splines suggested analysis

dat$effort<-as.numeric(dat$effort)
dat$year2<-as.factor(dat$year)
dat$calDay<-as.numeric(dat$calDay)
dat$catch<-as.numeric(dat$catch)
dat$logcpue<-log(dat$cpue)
m1 <- brm(cpue ~ s(calDay) + (1|year2),
          data=dat,
          family =gaussian(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99))

summary(m1)
windows()
#add a very small number to cpue to use lognormal distribution

dat$cpue<-dat$cpue+.0000000000001
m2log <- brm(cpue~ s(calDay) + (calDay|year2),
          data=dat,
          family =lognormal(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99, max_treedepth=15))



#Look at model results          
windows()
conditional_effects(m1, surface = TRUE)


windows()
conditional_effects(m2, surface = TRUE)


albgam<-cbind(dat$year,dat$calDay,fitted(m2),fitted(m2,probs=c(0.05,0.95)),fitted(m2,probs=c(0.25,0.75)))
colnames(albgam)[1:3]<-c("year","doy", "cpue")
write.csv(albgam,"analyses/output/albionchiphenbrms.csv", row.names = FALSE)


albgamlog<-cbind(dat$year,dat$calDay,fitted(m2log),fitted(m2log,probs=c(0.05,0.95)),fitted(m2log,probs=c(0.25,0.75)))
colnames(albgamlog)[1:3]<-c("year","doy", "cpue")
write.csv(albgamlog,"analyses/output/albionchiphenbrmslog.csv", row.names = FALSE)

save(m2, file="analyses/output/albionchibrms.Rda")
save(m2log, file="analyses/output/albionchibrmslog.Rda")

#poisson models
dat$effort<-as.integer(dat$effort)
dat$year2<-as.factor(dat$year)
dat$calDay<-as.numeric(dat$calDay)
dat$catch<-as.integer(dat$catch)
dat$effort<-as.integer(dat$effort)

m1 <- brm(catch ~ s(calDay) + offset(effort) + (1|year2),
          data=dat,
          family =poisson(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99))

summary(m1)
windows()alDay

m2 <- brm(catch~ s(calDay)  + offset(effort) + (c|year2),
          data=dat,
          family =poisson(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99, max_treedepth=15))







#old code using sme

fit<-sme(cpue,doy,year,criteria="AIC")
quartz()
plot(fit,type="diagnostic")
fit$info
quartz()
plot(fit,type="model",xlab="doy",ylab="cpue")
length(fit$fitted)
summary(fit)
#this fitting takes a while
plot(fit,type="raw",showModelFits=TRUE,xlab="doy",ylab="cpue")
#next extract peak doy around doy 100 and around doy 150 from model predictions
summary(fit)
preds<-as.data.frame(fit$fitted)
preds$year<-substr(rownames(preds),1,4)
#preds$n<-substr(rownames(preds),5,length(rownames(preds)))
preds$doy<-dat$calDay

colnames(preds)[1]<-"cpue.est"
preds$cpue.est[preds$cpue.est<0]<-0
tail(preds)
write.csv(preds,"analyses/output/albiongamests.csv", row.names=FALSE)

allyears<-unique(preds$year)

seasons<-c("springsum","fall","allyear")
years<-c()
allseasons<-c()
firstobsdate<-c()
lastobsdate<-c()
midobsdate<-c()
peakobsdate<-c()
peakobsdate.sp<-c()
peakobsdate.fa<-c()
alltotal<-c()
alltotal.sp<-c()
alltotal.fa<-c()

for(y in allyears){
  datyr<-preds[preds$year==y,]
  if (dim(datyr)[1]<=1){
    first<-last<-mid<-peak<-NA
    total<-NA
  }
  if (dim(datyr)[1]>0){
    cpue<-datyr$cpue.est
    cpuesp<-datyr$cpue.est[datyr$doy<213]#213= aug 1
    cpuefa<-datyr$cpue.est[datyr$doy>=213]
    #plot(datyr$doy,count, pch=21, bg="gray", main=paste(y))
    #if(y==min(allyears)){mtext(paste(sites[i], species[p]),side=3, line=3)}
    datdoy<-datyr
    datdoysp<-datyr[datyr$doy<213,]
    datdoyfa<-datyr[datyr$doy>=213,]
    first<-min(datdoy$doy[which(cpue>0)])
    last<-max(datdoy$doy[which(cpue>0)])
    total<-sum(cpue,na.rm=TRUE)
    totalsp<-sum(cpuesp,na.rm=TRUE)
    totalfa<-sum(cpuefa,na.rm=TRUE)
    
    mid<-datdoy$doy[min(which(cumsum(cpue)>(total/2)))]#date at which half of fish have arrived
    peak<-min(datdoy$doy[which(cpue==max(cpue, na.rm=TRUE))])#date of peak number of fish observed, if multiple dates with same number, choose first of these
    peaksp<-min(datdoysp$doy[which(cpuesp==max(cpuesp, na.rm=TRUE))])#date of peak number of fish observed, if multiple dates with same number, choose first of these
    peakfa<-min(datdoyfa$doy[which(cpuefa==max(cpuefa, na.rm=TRUE))])#date of peak number of fish observed, if multiple dates with same number, choose first of these
    #print(peak)
  }
  print(y);print(first);print(last);print(total); print(mid)
  years<-c(years,y)
  #allseasons<-c(allseasons,season[s])
  firstobsdate<-c(firstobsdate,first)
  lastobsdate<-c(lastobsdate,last)
  midobsdate<-c(midobsdate,mid)
  peakobsdate<-c(peakobsdate,peak)
  peakobsdate.fa<-c(peakobsdate.fa,peakfa)
  peakobsdate.sp<-c(peakobsdate.sp,peaksp)
  alltotal<-c(alltotal,total)
  alltotal.fa<-c(alltotal.fa,totalfa)
  alltotal.sp<-c(alltotal.sp,totalsp)
  
  #firstobsdate[which(firstobsdate=="Inf")]<-NA
  #peakobsdate[which(peakobsdate=="Inf")]<-NA
  #lastobsdate[which(lastobsdate=="-Inf")]<-NA
  }

#if(length(which(is.na(firstobsdate)))<5){next}
#Save a file with these estimates in it
albchiphenest<-cbind("ck","albion",years,firstobsdate,lastobsdate,peakobsdate,peakobsdate.sp,peakobsdate.fa,midobsdate,alltotal,alltotal.sp,alltotal.fa)

colnames(albchiphenest)[1:3]<-c("sp","site","year")
write.csv(albchiphenest,"analyses/output/albionchiphenest.csv", row.names =FALSE)

#Now estimate trends in phenology from gamests rather than those above calculated frmo raw data
albchiphenest<-read.csv("analyses/output/albionchiphenest.csv", header=TRUE)
#restrict to time frame consistent with orcas
albchiphenest<-albchiphenest[albchiphenest$year>1995,]
albchiphenest<-albchiphenest[albchiphenest$year<2018,]
firstmod<-lm(firstobsdate~year, data=albchiphenest)
  firstcoefs<-coef(firstmod)
  firstcoefs.50ci<-confint(firstmod,level = 0.50)
  firstcoefs.95ci<-confint(firstmod,level = 0.95)
  lastmod<-lm(lastobsdate~year, data=albchiphenest)
  lastcoefs<-coef(lastmod)
  lastcoefs.50ci<-confint(lastmod,level = 0.50)
  lastcoefs.95ci<-confint(lastmod,level = 0.95)
  peakmod<-lm(peakobsdate~year, data=albchiphenest)
  peakcoefs<-coef(peakmod)
  peakcoefs.50ci<-confint(peakmod,level = 0.50)
  peakcoefs.95ci<-confint(peakmod,level = 0.95)
  abundmod<-lm(alltotal~year, data=albchiphenest)
  abundcoefs<-coef(abundmod)
  abundcoefs.50ci<-confint(abundmod,level = 0.50)
  abundcoefs.95ci<-confint(abundmod,level = 0.95)
  

allmodsums<-c(round(firstcoefs, digits=3),round(lastcoefs, digits=3),round(peakcoefs, digits=3))
allmodsums.50ci<-rbind(round(firstcoefs.50ci, digits=3),round(lastcoefs.50ci, digits=3),round(peakcoefs.50ci, digits=3))
allmodsums.95ci<-rbind(round(firstcoefs.95ci, digits=3),round(lastcoefs.95ci, digits=3),round(peakcoefs.95ci, digits=3))
phen<-c("first","first","last","last","peak","peak")
sums<-cbind("ck","albion",phen,allmodsums,allmodsums.50ci,allmodsums.95ci)
colnames(sums)<-c("sp","site","phen","est","ci25","ci75","ci2.5","ci97.5")

write.csv(sums, "analyses/output/albionreturntrends_linmodyrs.csv", row.names = TRUE)
