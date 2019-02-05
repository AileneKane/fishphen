##################################################################
# Orca phenology occupancy model, based on the
# Worked example to run the model presented in Strebel et al., 2014
# (Study of phenology by flexible estimation and modeling of seasonal detectability peaks)
# Ailene Ettinger, ailene.ettinger@noaa.gov
# (modifed from code of  Nicolas Strebel, nicolas_strebel@gmx.ch)
# Start Date:	November 27, 2018
# Title:	orca_run_occ_model
##################################################################
# Set working directory
setwd("~/Documents/GitHub/fishphen")

# Load libraries
library(R2jags)
library(scales)

# Choose the data you want:
pod="L"#options= J,K,L,SR
season="1"#options= 1(winter) or 2(summer)
region="ps"#options=upper salish sea (uss) or puget sound (ps)

# Read observation data from focal pod (created in orca_dataprep_occmodel.R)

if(pod=="J"){dat<-read.csv("analyses/output/j_dat.csv",header=T)}
if(pod=="K"){dat<-read.csv("analyses/output/k_dat.csv",header=T)}
if(pod=="L"){dat<-read.csv("analyses/output/l_dat.csv",header=T)}
if(pod=="SR"){dat<-read.csv("analyses/output/allsr_dat.csv",header=T)}

#restrict to season
dat<-dat[dat$season==season,]
#if winter  (season 1), then days= days ater sept 30
if(season=="1"){
  dat<-subset(dat,select=c(nrep,ndet,site, daysaftsept30,year,season,region))
colnames(dat)[4]<-"day"
  }

#choose region
dat<-dat[dat$region==region,]

dim(dat)



#-----------------------------------------------------------------
# Codes prepare data for jags and run the analysis
#-----------------------------------------------------------------

# Specify model in BUGS language
sink("analyses/splinesSiteOcc S4.txt")
cat("
    model { 
    ### Define seasonal and annual patterns in detectability
    for (m in 1:nyear) {  
    for (i in 1:n) {
    logit(p[m,i]) <- lp[m,i]
    lp[m,i] <- mfe[m,i]+mre[m,i]
    mfe[m,i] <- a[m]*X[i,1]+b[m]*X[i,2]+c[m]*X[i,3]
    mre[m,i]<-sum(n.mre[m,i,1:nknots])
    for (k in 1:nknots) {
    n.mre[m,i,k]<-b.k[m,k]*Z[i,k]
    }
    }
    
    ### Random regression coefficients corresponding to the truncated polynomial functions
    for (k in 1:nknots) {
    b.k[m,k] ~ dnorm(0,taub)
    }
    
    ### Fixed regression coefficients corresponding to the 'plus' functions
    
    a[m] ~ dnorm(0,0.01)
    b[m] ~ dnorm(0,0.01)
    c[m] ~ dnorm(0,0.01)
    }
    
    ### precision for random regression coefficients corresponding to the truncated polynomial functions
    taub~dgamma(1.0E-6,1.0E-6)      
    
    # Specify priors
  for (k in 1:nyear) {
    psi[k] ~ dunif(0, 1)
    }

    # Ecological submodel: Define state conditional on parameters
    for (i in 1:nsite){
    for (k in 1:nyear){
    z[i,k] ~ dbern(psi[k])
    }
    }
    
    # Observation model
    for (i in 1:nobs){
    muy[site[i],survey[i],year[i]] <- z[site[i],year[i]]*p[year[i],survey[i]]
    y[i] ~ dbin(muy[site[i],survey[i],year[i]], nrep[i])
    }
    
    }
    ",fill = TRUE)
    sink()

### The following procedure is based on the models presented in Crainiceanu et al. 2005 and in Gimenez et al. 2006 
# Degree of splines
degree <- 2

# covariate
covariate<-as.numeric(scale(range(dat$day)[1]:range(dat$day)[2]))

# covariate length
n <- length(covariate)

# location of knots
nk<-round((max(dat$day)-min(dat$day)+1)/4)
nknots<-ifelse(nk<35,nk,35)
knots<-quantile(unique(covariate),seq(0,1,length=(nknots+2))[-c(1,(nknots+2))])
#Note: the maximum number of nots is 35. thus, the annual model (for which nk=92 in many cases, but it is restricted to 35 by default) differs in flexibility than the seasonal model
#perhaps better to extract the seasonal peaks after fitting the whole year of data
# fixed effects matrix
 X<-NULL
for (l in 0:degree) {
  X<-cbind(X,covariate^l)  
}

# random coefficients matrix
Z_K<-(abs(outer(covariate,knots,"-")))^3
OMEGA_all<-(abs(outer(knots,knots,"-")))^3
svd.OMEGA_all<-svd(OMEGA_all)
sqrt.OMEGA_all<-t(svd.OMEGA_all$v %*% (t(svd.OMEGA_all$u)*sqrt(svd.OMEGA_all$d)))
Z<-t(solve(sqrt.OMEGA_all,t(Z_K)))

# Input data
dat$site <- factor(dat$site)#
dat$site <- droplevels(dat$site)
dat$site <- as.integer(dat$site)

site <- dat$site
survey <- dat$day-min(dat$day)+1
nobs <- length(unique(paste(dat$site,dat$day,dat$year)))
nrep <- dat$nrep
nsite <- length(unique(dat$site))
nyear <- length(unique(dat$year))
year <- as.numeric(factor(dat$year))
zst <- array(1, dim=c(nsite,nyear))  
y <- dat$ndet

# Simulation parameters
#ni=15000; nc=2; nb=0; nt=10
ni=5000; nc=2; nb=1500; nt=1
# List input data
jags.data <- list("site","survey","nobs","nrep","nsite","nyear","year","nknots","n","X","Z","nc", "nb", "ni", "nt","zst","y")

# Inits function
f.inits <- function(){list(a=rep(0,nyear), b=rep(0,nyear), c=rep(0,nyear), z=zst)}

# specify the parameters to be monitored
parameters <- c("a","b","c","lp","psi","taub")

### Run MCMC Analysis using jags

jags.out<-jags.parallel(jags.data,f.inits,parameters,"splinesSiteOcc S4.txt",nc,ni,nb,nt)
names(jags.out$BUGSoutput)
quartz()
plot(jags.out)
#traceplot(jags.out, dig=3)#
#For Jpod season 2: most of these chains do not look good. b.k.[14,3], b.k.[14,4]...everything in year 14  look good. These are all year 1992
#For Jpod season 1: the following chains look good:
#a[1-4,7,8,13,14,19]
#b[1-4,7,8,9,10,11,13,14,19]
#c[1-4,7,8,9,11,13,16,19]
#lp[1-4,6,7,8,9,11,12,13,14,18,19,23,25,27,30,31,35]

#Questions: why?



#Look at psi
out<-jags.out$BUGSoutput
jags.out$BUGSoutput$mean$psi#probability of presence (annual)
# Save model output
if(pod=="J" & season=="1"){save(out,file="jags.output/jpod out season1")}
if(pod=="J" & season=="2"){save(out,file="jags.output/jpod out season2")}
if(pod=="K" & season=="1"){save(out,file="jags.output/kpod out season1")}
if(pod=="K" & season=="2"){save(out,file="jags.output/kpod out season2")}
if(pod=="L" & season=="1"){save(out,file="jags.output/lpod out season1")}
if(pod=="L" & season=="2"){save(out,file="jags.output/lpod out season2")}
if(pod=="SR" & season=="1"){save(out,file="jags.output/allsrpods out season1")}
if(pod=="SR" & season=="2"){save(out,file="jags.output/allsrpods out season2")}

#If you don't want to run the model:
#if(pod=="J" & season=="1"){load("jpod out season1")}
#I usually run two chains over 50'000 iterations, this takes several hours on my PC (3.4GHz, 4GB RAM)
#Usually convergence is reached within the first 10'000; I set burnin to 25'000
#To plot model output run the following codes:

#-----------------------------------------------------------------
# Codes to summarize the output
#-----------------------------------------------------------------
### If you want to skip running the bugs function, then load the output here:
#load(file="out S4")

### get estimated date of peak detectability based on posterior distribution
# get date of peak detectability in each simulation
findmax.fn<-function(x) {
  mean(which(x==max(x)))
}
lpmax<-array(data=NA,dim=c(out$n.sims,nyear))
dimnames(lpmax)<-list(c(1:out$n.sims),c(sort(unique(dat$year))))
for (xj in sort(unique(as.numeric(factor(dat$year))))) { 
  lpmax[,xj]<-apply(out$sims.array[,,paste("lp[",xj[1],",",1:(max(dat$day)-min(dat$day)+1),"]",sep="")],MARGIN=c(if(out$n.chains>1) 1:2 else 1),findmax.fn)
}
lpmax<-lpmax+min(dat$day)-1
lpmax[lpmax==max(dat$day)]<-NA
lpmax[lpmax==min(dat$day)]<-NA
#would like to Extract psi (probability of presence by day...)
dim(out$sims.list$psi)

#get first date when detectability is greater than 0.5
findfirst.fn<-function(x) {
  min(which(plogis(x)>0.10))
}
firstlp<-array(data=NA,dim=c(out$n.sims,nyear))
dimnames(firstlp)<-list(c(1:out$n.sims),c(sort(unique(dat$year))))
for (xj in sort(unique(as.numeric(factor(dat$year))))) { 
  firstlp[,xj]<-apply(out$sims.array[,,paste("lp[",xj[1],",",1:(max(dat$day)-min(dat$day)+1),"]",sep="")],MARGIN=c(if(out$n.chains>1) 1:2 else 1),findfirst.fn)
}
firstlp<-firstlp+min(dat$day)-1
firstlp[firstlp==max(dat$day)]<-NA
firstlp[firstlp==min(dat$day)]<-NA


# summarize estimates
ann.res<-array(NA, dim=c(max(dat$year)-min(dat$year)+1,3),dimnames=list(c(min(dat$year):max(dat$year)),c("mean","10%","90%")))
res<-apply(lpmax,c(2),mean,na.rm=T)
ann.res[names(res),"mean"]<-res
res<-apply(lpmax,c(2),quantile,probs=0.10,na.rm=T)
ann.res[names(res),"10%"]<-res
res<-apply(lpmax,c(2),quantile,probs=0.90,na.rm=T)
ann.res[names(res),"90%"]<-res

# get estimate of trend in date of peak detectability over years
do.lm<-function(x) {
  lmres<-lm(x~as.numeric(names(x)))$coefficients
  return(lmres)
}
r<-matrix(NA,dim(lpmax)[1],2)
for (o in 1:(dim(lpmax)[1])) {
 # if(!is.na(sum(lpmax[o,]))) {
    lm(lpmax[o,]~as.numeric(colnames(lpmax)))$coefficients->r[o,]
  #}    
}
slopevec<-as.vector(r[,2])
intercept<-mean(r[,1],na.rm=T)
slope<-mean(r[,2],na.rm=T)
intercept.10<-quantile(r[,1],c(0.10),na.rm=T)
intercept.90<-quantile(r[,1],c(0.90),na.rm=T)

slope.10<-quantile(r[,2],c(0.10),na.rm=T)
slope.90<-quantile(r[,2],c(0.90),na.rm=T)

### Write results (in console if argument file is not specified in function cat)
if(season=="1"){
cat(paste("summary results",pod,region,season),"\n",
    paste("annual change of activity peak:", round(mean(slopevec,na.rm=T),digits=2),"days"),
    paste("confidence interval from", round(quantile(slopevec,0.10,na.rm=T),digits=2),
          "to",round(quantile(slopevec,0.90,na.rm=T),digits=2)),
    "\n","mean estimate of activity peak","as date",
    as.character(as.Date(x=c(ann.res[,colnames(ann.res)=="mean"]),origin=c(paste(row.names(ann.res),"-09-30",sep="")))),"\n",
    sep="\n","as days after sept 30",
    paste(rownames(ann.res),round(ann.res[,"mean"])))   
}
if(season=="2"){
  cat(paste("summary results",pod,region,season),"\n",
      paste("annual change of activity peak:", round(mean(slopevec,na.rm=T),digits=2),"days"),
      paste("confidence interval from", round(quantile(slopevec,0.10,na.rm=T),digits=2),
            "to",round(quantile(slopevec,0.90,na.rm=T),digits=2)),
      "\n","mean estimate of activity peak","as date",
      as.character(as.Date(x=c(ann.res[,colnames(ann.res)=="mean"]),origin=c(paste(row.names(ann.res),"-01-01",sep="")))),"\n",
      sep="\n","as day of year",
      paste(rownames(ann.res),round(ann.res[,"mean"])))   
}
#-----------------------------------------------------------------
# Plot output
#-----------------------------------------------------------------
# save plotted results as pdf
if(pod=="J" & season=="1" & region=="uss"){pdf(file=paste("analyses/figures/J/orcaphen_1976_2017_USS_winter_J.pdf"),width=7,height=6)}
if(pod=="J" & season=="1" & region=="ps"){pdf(file=paste("analyses/figures/J/orcaphen_1976_2017_PS_winter_J.pdf"),width=7,height=6)}
if(pod=="J" & season=="2" & region=="uss"){pdf(file=paste("analyses/figures/J/orcaphen_1976_2017_USS_summer_J.pdf"),width=7,height=6)}
if(pod=="K" & season=="1" & region=="ps"){pdf(file=paste("analyses/figures/K/orcaphen_1976_2017_PS_winter_K.pdf"),width=7,height=6)}
if(pod=="K" & season=="2" & region=="uss"){pdf(file=paste("analyses/figures/K/orcaphen_1976_2017_USS_summer_K.pdf"),width=7,height=6)}
if(pod=="L" & season=="1" & region=="ps"){pdf(file=paste("analyses/figures/L/orcaphen_1976_2017_PS_winter_L.pdf"),width=7,height=6)}
if(pod=="L" & season=="2" & region=="uss"){pdf(file=paste("analyses/figures/L/orcaphen_1976_2017_USS_summer_L.pdf"),width=7,height=6)}
if(pod=="SR" & season=="1" & region=="ps"){pdf(file=paste("analyses/figures/SR/orcaphen_1976_2017_PS_winter_SR.pdf"),width=7,height=6)}
if(pod=="SR" & season=="2" & region=="uss"){pdf(file=paste("analyses/figures/SR/orcaphen_1976_2017_USS_summer_SR.pdf"),width=7,height=6)}

### plot estimates of peak detectability over all years
#quartz(width=7, height=6)
par(mfrow=c(1,1),mai=c(1,1,1,0.5))
x=rownames(ann.res)
y=ann.res[,"mean"]
seasname<-c("winter","summer")
plot(x,y,xlab="",ylab="",axes=F,main=paste("Peak Detection Probability","\n",pod," Pod",seasname[as.numeric(season)],region),
     ylim=c(min(ann.res, na.rm = TRUE),max(ann.res, na.rm = TRUE)),pch=16,type="l", lwd=2,col="black")
#polygon(c(rev(x),x),c(rev(ann.res[,"90%"]),ann.res[,"10%"]),col=alpha("grey",0.2),lwd=0.1)

axis(side=1,at=x)
if(season==2){
  axis(side=2,at=c(122,152,183,214,244,274),
    labels=c("1May","1Jun","1Jul","1Aug","1Sept","1Oct"))
  }
if(season==1){
  axis(side=2,at=c(1,32,62,93,124,153,184,214),
       labels=c("1Oct","1Nov","1Dec","1Jan","1Feb","1Mar","1Apr","1May"))
}

for (o in 1:500) {
   #if(!is.na(sum(lpmax[o,]))) {
  mod<-lm(lpmax[o,]~as.numeric(colnames(lpmax)))$coefficients->r[o,]
  abline(mod,col=alpha("grey",0.2),lwd=2)
  
  #}    
}
abline(a=intercept,b=slope,col="darkred",lwd=2)

dev.off()

### Plot annual detectability pattern
# loop over all years
years<-sort(unique(as.numeric((dat$year))))
seasonname<-c("winter","summer")
for (xj in 1:length(years)) {
  j<-years[xj]
  
  # Get BUGS estimates
  res.chains<-out$sims.array[,,paste("lp[",xj[1],",",1:(max(dat$day)-min(dat$day)+1),"]",sep="")]
  res=plogis(apply(res.chains,MARGIN=c(length(dim(res.chains))),quantile,probs=c(.10,.5,.90)))
  
  ### Plot "naive" estimate of detectability
  # prepare bars to compare barplot of observation data (bars) with estimates (line); barheight represents weekly proportion of detection events divided by all surveys
  barwidth<-7
  z<-1
  for(m in seq(from=min(dat$day),to=max(dat$day),by=barwidth)) {
    dat$barpos[dat$day >= m & dat$day < m+barwidth]<-z
    z<-z+1
  }
  barheight<-rep(NA,times=max(dat$day)+7)
  names(barheight)<-1:max(dat$day)
  
  # height of the bars equals to a seven day successful obs to all obs ratio
  n<-(max(dat$day)-min(dat$day)+1)
  res.height<-tapply(dat$ndet[dat$year==j],dat$barpos[dat$year==j],sum)/tapply(dat$nrep[dat$year==j],dat$barpos[dat$year==j],sum)
  
  barheight[as.numeric(names(res.height))*7-3+min(dat$day)]<-res.height
  
  # plot bars
  #for seasonal values...    
  figpath<- paste("analyses/figures/",pod,sep="")
  figname<-paste("orcaphen",j,region,seasonname[as.numeric(season)],pod,".pdf",sep="_")
  pdf(file.path(figpath, figname), width = 9, height = 6)
  
  #quartz()
  
  
  x<-barplot(as.numeric(barheight[min(dat$day):max(dat$day)]),
          width=1,space=0,ylim=c(0,1),xlab="", ylab="Detection Probability", 
          main=paste(pod," pod",j),border=NA,axes=F)#ylim:max(res[3,])
  
  ### Plot model estimates  
  # plot seasonal estimates of detectability p
  lines(res[3,],lty=3,col=1,lwd=2.5) # lower bound of the 90% CI
  lines(res[2,],lty=1,col=1,lwd=2) # median
  lines(res[1,],lty=3,col=1,lwd=2.5) # upper bound of the 90% CI
  axis(2)
  if(season==2){
  axis(side=1,at=c(122-121,152-121,183-121,214-121,244-121,274-121),
       labels=c("1May","1Jun","1Jul","1Aug","1Sept","1Oct"))
  }
  if(season==1){
    axis(side=1,at=c(1,32,62,93,124,153,184,214),
         labels=c("1Oct","1Nov","1Dec","1Jan","1Feb","1Mar","1Apr","1May"))
    abline(a=intercept,b=slope,lty=2,col=colors()[200])
  }
  dev.off()
  }

