##################################################################
# Orca phenology occupancy model, based on the
# Worked example to run the model presented in Strebel et al., 2014
# (Study of phenology by flexible estimation and modeling of seasonal detectability peaks)
# Ailene Ettinger, ailene.ettinger@noaa.gov
# (modifed from code of  Nicolas Strebel, nicolas_strebel@gmx.ch)
# Start Date:	November 27, 2018
# Title:	orca_run_occ_model
##################################################################
#housekeeping

rm(list=ls()) 
options(stringsAsFactorwants = FALSE)

# Set working directory
setwd("~/GitHub/fishphen")

# Load libraries
library(R2jags)
library(scales)

# Choose the data you want:
pod="J"#options= J,K,L,SR
region="uss"#options=upper salish sea (uss) or puget sound (ps)
wholeyear=FALSE #if FALSE then resitrct to assigned seasons for uss and ps
assumeSRKW=FALSE
#Choose the credible intervals you 
lci<-0.125
uci<-0.875

prob<-0.5
# Read observation data from focal pod (created in orca_dataprep_occmodel.R)
if(assumeSRKW==FALSE){
  if(pod=="J"){dat<-read.csv("analyses/output/j_dat.csv",header=T)}
  if(pod=="K"){dat<-read.csv("analyses/output/k_dat.csv",header=T)}
  if(pod=="L"){dat<-read.csv("analyses/output/l_dat.csv",header=T)}
  if(pod=="SR"){dat<-read.csv("analyses/output/allsr_dat.csv",header=T)}
}
if(assumeSRKW==TRUE){
  if(pod=="J"){dat<-read.csv("analyses/output/j_dat_assumeSRKW.csv",header=T)}
  if(pod=="K"){dat<-read.csv("analyses/output/k_dat_assumeSRKW.csv",header=T)}
  if(pod=="L"){dat<-read.csv("analyses/output/l_dat_assumeSRKW.csv",header=T)}
  if(pod=="SR"){dat<-read.csv("analyses/output/allsr_dat_assumeSRKW.csv",header=T)}
}
#choose region
dat<-dat[which(dat$region==region),]
#Add a column for "season" and restrict data to season that is appropriate to the region
#use may 1 for uss season, oct 1 for ps season as start dates
#use oct 31 for uss season, jan31 for ps season, as end dates
if(wholeyear==FALSE & region == "ps"){
  season="1"#winter
  dat$season<-NA
  dat$season[dat$day>182]<-1#winter (July 1-Dec 31 = >182#should extend this to Jan 31
  }

#to extend to jan31, add an "orca year" which runs Apr 1-Mar 31
#dat$orcayear<-dat$year
#dat$orcayear[which(dat$day>273)]<-dat$year[which(dat$day>273)]+1
#dat$daysaftsept30<-NA
#dat$daysaftsept30[which(dat$day>273 & dat$day<367)]<-dat$day[which(dat$day>273 & dat$day<367)]-273
#dat$daysaftsept30[which(dat$day<274)]<-dat$day[which(dat$day<274)]+93#this should actually vary depending on whether or not it is a leap year

if(wholeyear==FALSE & region == "uss"){
  season="2"#summer
  dat$season<-NA
  dat$season[dat$day>91 & dat$day<304]<-2#summer (April 1-Oct 31)
}
if(wholeyear==TRUE ){
  season="3"#season 3 = whole year
  dat$season<-3
  }

dat<-dat[which(dat$season==season),]

#if winter  (season 1), then days= days after sept 30
#if(season=="1"){
#  dat<-subset(dat,select=c(nrep,ndet,site, daysaftsept30,year,season,region))
#colnames(dat)[4]<-"day"
#  }


dim(dat)



#-----------------------------------------------------------------
# Codes prepare data for jags and run the analysis
#-----------------------------------------------------------------

# Specify model in BUGS language
sink("analyses/splinesSiteOccS4psi.txt")
cat("
    model { 
    ### Define seasonal and annual patterns in occurrence probability
    for (m in 1:nyear) {  
    for (i in 1:n) {
    logit(psi[m,i]) <- lp[m,i]
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
    
    # Specify priors for detection model
    for (i in 1:nsite){# 
    for (y in 1:nyear) {
    p[i,y] ~ dunif(0, 1)
    }
    }
    # Ecological submodel: Define state conditional on parameters
    
    for (y in 1:nyear) {  
    for (i in 1:n) {
    z[y,i] ~ dbern(psi[y,i])
    }    
    }
    
    # Observation model
    for (i in 1:nobs){
    muy[site[i],survey[i],year[i]] <- z[year[i],survey[i]]*p[site[i],year[i]]
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
#Note: the maximum number of knots is 35. thus, the annual model (for which nk=92 in many cases, but it is restricted to 35 by default) differs in flexibility than the seasonal model
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
fas<-sort(unique(dat$site))
dat$site <- as.integer(dat$site)

site <- dat$site
survey <- dat$day-min(dat$day)+1
nsurveys<-length(survey)
nobs <- length(unique(paste(dat$site,dat$day,dat$year)))
nrep <- dat$nrep
nsite <- length(unique(dat$site))
nyear <- length(unique(dat$year))
year <- as.numeric(factor(dat$year))
zst <- array(1, dim=c(nyear,n))  
y <- dat$ndet

# Simulation parameters

#ni=15000; nc=2; nb=0; nt=10
ni=20000; nc=2; nb=10000; nt=10
# List input data
jags.data <- list("site","survey","nobs","nrep","nsite","nyear","year","nknots","n","X","Z","nc", "nb", "ni", "nt","zst","y")

# Inits function
f.inits <- function(){list(a=rep(0,nyear), b=rep(0,nyear), c=rep(0,nyear), z=zst)}

# specify the parameters to be monitored
parameters <- c("a","b","c","lp","psi","taub","p")

### Run MCMC Analysis using jags

jags.out<-jags.parallel(jags.data,f.inits,parameters,"analyses/splinesSiteOccS4psi.txt",nc,ni,nb,nt)
#names(jags.out$BUGSoutput)
#diagnose the model
#quartz()
#plot(jags.out)
#traceplot(jags.out, dig=3)#


#Look at psi
out<-jags.out$BUGSoutput
jags.out$BUGSoutput$mean$psi#probability of presence (daily, across 40 years)

#Look at Rhat
range(out$summary[,8])
length(which(out$summary[,8]>1.1))/length(out$summary[,8])
rownames(out$summary[which(out$summary[,8]>1.1),])
  #look at problem psis & problem lps

dim(jags.out$BUGSoutput$mean$psi)
meanpsi<-rowMeans(jags.out$BUGSoutput$mean$psi)#meanannual mean prob occurrence
names(meanpsi)<-seq(min(dat$year),max(dat$year), by=1)

meanp<-jags.out$BUGSoutput$mean$p
rownames(meanp)<-fas
colnames(meanp)<-seq(min(dat$year),max(dat$year), by=1)
# Save model output
#if(pod=="J" & season=="1"){save(out,file="jags.output/jpod.season1.psi")}
#if(pod=="J" & season=="2"){save(out,file="jags.output/jpod.season2.psi")}
#if(pod=="K" & season=="1"){save(out,file="jags.output/kpod.season1.psi")}
#if(pod=="K" & season=="2"){save(out,file="jags.output/kpod.season2.psi")}
#if(pod=="L" & season=="1"){save(out,file="jags.output/lpod.season1.psi")}
#if(pod=="L" & season=="2"){save(out,file="jags.output/lpod.season2.psi")}

#Strebel paper says  "usually runs two chains over 50'000 iterations,
#Usually convergence is reached within the first 10'000; set burnin to 25'000"
#To plot model output run the following codes:

#-----------------------------------------------------------------
# Codes to summarize the output
#-----------------------------------------------------------------

### get estimated date of peak occurrence based on posterior distribution
# get date of peak probability of occurrence in each simulation

if(region == "uss"){color = "darkblue"
cols = c("darkblue","royalblue")}
if(region == "ps"){color = "yellow1"
cols = c("yellow1","gold3")}

prob<-c(0.2,0.3,0.4,0.5)
for(p in prob){
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
#would like to Extract and plot psi (probability of presence by day...)
dim(out$sims.list$psi)
doy<-seq(from=min(dat$day),to=max(dat$day),by=1)
#Plot by decade to see changes in psi over time
#quartz(height=6,width=12)
par(mfrow=c(1,4))
plot(doy,out$mean$psi[1,], type= "l", ylim=c(0,1))
for(y in 2:10){
  lines(doy,out$mean$psi[y,])
}
plot(doy,out$mean$psi[11,], type= "l", ylim=c(0,1))
for(y in 12:20){
  lines(doy,out$mean$psi[y,])
}
plot(doy,out$mean$psi[21,], type= "l", ylim=c(0,1))
for(y in 22:30){
  lines(doy,out$mean$psi[y,])
}
plot(doy,out$mean$psi[31,], type= "l", ylim=c(0,1))
for(y in 32:40){
lines(doy,out$mean$psi[y,])
}
length(unique(dat$day))

# summarize estimates and look at change across the whole time series

ann.res<-array(NA, dim=c(max(dat$year)-min(dat$year)+1,3),dimnames=list(c(min(dat$year):max(dat$year)),c("mean","lci","uci")))
res<-apply(lpmax,c(2),mean,na.rm=T)
ann.res[names(res),"mean"]<-res
res<-apply(lpmax,c(2),quantile,probs=lci,na.rm=T)
ann.res[names(res),"lci"]<-res
res<-apply(lpmax,c(2),quantile,probs=uci,na.rm=T)
ann.res[names(res),"uci"]<-res


psi.ann<-array(NA, dim=c(max(dat$year)-min(dat$year)+1,3),dimnames=list(c(min(dat$year):max(dat$year)),c("mean","lci","uci")))
psi<-apply(out$sims.list$psi,c(2),mean,na.rm=T)
psi.ann[names(res),"mean"]<-psi
psi<-apply(out$sims.list$psi,c(2),quantile,probs=lci,na.rm=T)
psi.ann[names(res),"lci"]<-psi
psi<-apply(out$sims.list$psi,c(2),quantile,probs=uci,na.rm=T)
psi.ann[names(res),"uci"]<-psi


#plot mean psi across all years for the season
if(assumeSRKW==TRUE){pdf(file=paste("analyses/figures/",pod,"/orcaphen_",min(dat$year),"-",max(dat$year),"_",region,"_",season,"_doy",min(dat$day),"-",max(dat$day),"_",pod,"_",p,"meanoccprob_assumeSRKW.pdf", sep=""),height=6,width=8)}
if(assumeSRKW==FALSE){pdf(file=paste("analyses/figures/",pod,"/orcaphen_",min(dat$year),"-",max(dat$year),"_",region,"_",season,"_doy",min(dat$day),"-",max(dat$day),"_",pod,"_",p,"meanoccprob.pdf", sep=""),height=6,width=8)}


#windows(height=6,width=8)
psi.med<-apply(out$sims.list$psi[,32:40,],c(3),median)
plot(doy,psi.med, type= "l", ylim=c(0,1.2), 
     ylab= "Probability of occurrence", xlab= "Day of Year", yaxt ="n",
     bty="l", lty=1,col=color, lwd=2)
axis(side = 2, at = seq(from = 0, to = 1.0, by = .2), labels = c("0","0.2","0.4","0.6","0.8", "1.0"))
#names(out)
#lines(doy,psi.med)
psi.uci<-apply(out$sims.list$psi[,32:40,],c(3),quantile,probs=uci)
psi.lci<-apply(out$sims.list$psi[,32:40,],c(3),quantile,probs=lci)
#lines(doy,psi.lci,col=cols[2], lwd=2)
#lines(doy,psi.uci,col=cols[2], lwd=2)
#psi<-apply(out$sims.list$psi[,31:40,],c(3),quantile,probs=.5)
polygon(c(rev(doy),doy),c(rev(psi.uci),psi.lci),col=alpha(color,0.2),lty=0)
#add point for mean peak day of year
pkocdoy<-mean(ann.res[,"mean"][32:40])
pkocdoy.lci<-quantile(ann.res[,"mean"][32:40],lci)
pkocdoy.uci<-quantile(ann.res[,"mean"][32:40],uci)
#arrows(pkocdoy.lci,psi.med[which(doy==as.integer(pkocdoy))],pkocdoy.uci,psi.med[which(doy==as.integer(pkocdoy))],code = 0, col = cols[1], lwd = 3)
#points(pkocdoy,psi.med[which(doy==as.integer(pkocdoy))], pch = 21, bg=cols[1], cex = 2)

arrows(pkocdoy.lci,1.2,pkocdoy.uci,1.2,code = 0, col = cols[1], lwd = 3)
points(pkocdoy,1.2, pch = 21, bg=cols[1], cex = 2)

psi.med<-apply(out$sims.list$psi[,24:31,],c(3),median)
#lines(doy,colMeans(out$mean$psi[11:20,]),col=cols[4])
lines(doy,psi.med,col=cols[2], lwd=2, lty=2)
psi.uci<-apply(out$sims.list$psi[,24:31,],c(3),quantile,probs=uci)
#lines(doy,psi.uci)
psi.lci<-apply(out$sims.list$psi[,24:31,],c(3),quantile,probs=lci)
#lines(doy,psi.lci)
polygon(c(rev(doy),doy),c(rev(psi.uci),psi.lci),col=alpha(cols[2],0.2),lty=0)
#add point for mean peak day of year
pkocdoy<-mean(ann.res[,"mean"][24:31])
pkocdoy.lci<-quantile(ann.res[,"mean"][24:31],lci)
pkocdoy.uci<-quantile(ann.res[,"mean"][24:31],uci)
#arrows(pkocdoy.lci,psi.med[which(doy==as.integer(pkocdoy))],pkocdoy.uci,psi.med[which(doy==as.integer(pkocdoy))],code = 0, col = cols[2], lwd = 3)
#points(pkocdoy,psi.med[which(doy==as.integer(pkocdoy))], pch = 21, bg=cols[2], cex = 2)
#or always above plot?
arrows(pkocdoy.lci,1.14,pkocdoy.uci,1.14,code = 0, col = cols[2], lty=2,lwd = 3)
points(pkocdoy,1.14, pch = 21, bg=cols[2], cex = 2)

#arrows(pkocdoy.rec,psi.med[which(doy==as.integer(pkocdoy.rec))]+.4,pkocdoy.rec,psi.med[which(doy==as.integer(pkocdoy.rec))]+.02,code = 2, col = cols[1], lwd = 2)

#lines(doy,colMeans(out$mean$psi[1:10,]),col=cols[1], lwd=2)
#lines(doy,colMeans(out$mean$psi[1:30,]),col=color,lwd=3)
if(season==1){mtext("A)",side = 3, line = 0, adj =0)}
if(season==2){  mtext("C)",side = 3, line = 0, adj =0)}

legend("topleft",legend=c(paste(unique(dat$year)[32],"-",unique(dat$year)[40],sep= ""),
                         #paste(unique(dat$year)[11],"-",unique(dat$year)[20],sep= ""),
                         #paste(unique(dat$year)[21],"-",unique(dat$year)[30],sep= ""),
                         #paste(unique(dat$year)[1],"-",unique(dat$year)[10],sep= ""),
                          paste(unique(dat$year)[24],"-",unique(dat$year)[31],sep= "")),lwd=c(2,2),lty=c(1,2),col=c(cols), bty="n")
dev.off()


# get estimate of trend in date of peak detectability over years
do.lm<-function(x) {
  lmres<-lm(x~as.numeric(names(x)))$coefficients
  return(lmres)
}
r<-matrix(NA,dim(lpmax)[1],2)
#just do lm from 1978-2017
for (o in 1:(dim(lpmax)[1])) {
  # if(!is.na(sum(lpmax[o,]))) {
  lm(lpmax[o,]~as.numeric(colnames(lpmax)))$coefficients->r[o,]
  #}    
}
slopevec<-as.vector(r[,2])
intercept<-mean(r[,1],na.rm=T)
slope<-mean(r[,2],na.rm=T)
intercept.lci<-quantile(r[,1],c(lci),na.rm=T)
intercept.uci<-quantile(r[,1],c(uci),na.rm=T)

slope.lci<-quantile(r[,2],c(lci),na.rm=T)
slope.uci<-quantile(r[,2],c(uci),na.rm=T)
#look just at recent trends from 2001-present
r.recent<-matrix(NA,dim(lpmax)[1],2)
for (o in 1:(dim(lpmax)[1])) {
  y<-lpmax[o,24:40]
  y[y=="Inf"]<-NA
  lm(y~as.numeric(colnames(lpmax))[24:40])$coefficients->r.recent[o,]
}
slopevec.recent<-as.vector(r.recent[,2])
intercept.recent<-mean(r.recent[,1],na.rm=T)
slope.recent<-mean(r.recent[,2],na.rm=T)
intercept.recent.lci<-quantile(r.recent[,1],c(lci),na.rm=T)
intercept.recent.uci<-quantile(r.recent[,1],c(uci),na.rm=T)

slope.recent.lci<-quantile(r.recent[,2],c(lci),na.rm=T)
slope.recent.uci<-quantile(r.recent[,2],c(uci),na.rm=T)



#get first date when occurrenceis greater than some chosen probability
findfirst.fn<-function(x) {
  min(which(plogis(x)>p), na.rm=TRUE)
}
#check:
count.fn<-function(x) {
  length(which(plogis(x)<p))
}
firstlp<-array(data=NA,dim=c(out$n.sims,nyear))
dimnames(firstlp)<-list(c(1:out$n.sims),c(sort(unique(dat$year))))
for (xj in sort(unique(as.numeric(factor(dat$year))))) { 
  firstlp[,xj]<-apply(out$sims.array[,,paste("lp[",xj[1],",",1:(max(dat$day)-min(dat$day)+1),"]",sep="")],MARGIN=c(if(out$n.chains>1) 1:2 else 1),findfirst.fn)
}
firstlp<-firstlp+min(dat$day)-1
#firstlp[firstlp==max(dat$day)]<-NA
#firstlp[firstlp==min(dat$day)]<-NA
firstlp[which(firstlp=="Inf")]<-NA
#firstlp<-as.numeric(firstlp)
#countlp<-array(data=NA,dim=c(out$n.sims,nyear))
#dimnames(countlp)<-list(c(1:out$n.sims),c(sort(unique(dat$year))))
#for (xj in sort(unique(as.numeric(factor(dat$year))))) { 
#  countlp[,xj]<-apply(out$sims.array[,,paste("lp[",xj[1],",",1:(max(dat$day)-min(dat$day)+1),"]",sep="")],MARGIN=c(if(out$n.chains>1) 1:2 else 1),count.fn)
#}
#returns "inf" when no estimates are >prob

# summarize estimates
ann.first<-array(NA, dim=c(max(dat$year)-min(dat$year)+1,3),dimnames=list(c(min(dat$year):max(dat$year)),c("mean","lci","uci")))

first<-apply(firstlp,c(2),mean,na.rm=T)
ann.first[names(first),"mean"]<-first
first<-apply(firstlp,c(2),quantile,probs=lci,na.rm=T)
ann.first[names(first),"lci"]<-first
first<-apply(firstlp,c(2),quantile,probs=uci,na.rm=T)
ann.first[names(first),"uci"]<-first

# get estimate of trend in date of peak detectability over years
#firstlp<-as.numeric()
r.first<-matrix(NA,dim(firstlp)[1],2)
for (o in 1:(dim(firstlp)[1])) {
  # if(!is.na(sum(lpmax[o,]))) {
  y<-firstlp[o,]
  y[y=="Inf"]<-NA
  lm(y~as.numeric(colnames(firstlp)))$coefficients->r.first[o,]
  #}    
}


slopevec.first<-as.vector(r.first[,2])
intercept.first<-mean(r.first[,1],na.rm=T)
slope.first<-mean(r.first[,2],na.rm=T)
intercept.first.lci<-quantile(r.first[,1],c(lci),na.rm=T)
intercept.first.uci<-quantile(r.first[,1],c(uci),na.rm=T)

slope.first.lci<-quantile(r.first[,2],c(lci),na.rm=T)
slope.first.uci<-quantile(r.first[,2],c(uci),na.rm=T)
#Look at trends just in recent data
r.first.recent<-matrix(NA,dim(firstlp)[1],2)
for (o in 1:(dim(firstlp)[1])) {
  y<-firstlp[o,24:40]
  y[y=="Inf"]<-NA
  lm(y~as.numeric(colnames(firstlp))[24:40])$coefficients->r.first.recent[o,]
}
slopevec.first.recent<-as.vector(r.first.recent[,2])
intercept.first.recent<-mean(r.first.recent[,1],na.rm=T)
slope.first.recent<-mean(r.first.recent[,2],na.rm=T)
intercept.first.recent.lci<-quantile(r.first.recent[,1],c(lci),na.rm=T)
intercept.first.recent.uci<-quantile(r.first.recent[,1],c(uci),na.rm=T)

slope.first.recent.lci<-quantile(r.first.recent[,2],c(lci),na.rm=T)
slope.first.recent.uci<-quantile(r.first.recent[,2],c(uci),na.rm=T)

#get last date when detectability is greater than prob
findlast.fn<-function(x) {
  max(which(plogis(x)>p), na.rm=TRUE)
}
#check:
#count.fn<-function(x) {
#  length(which(plogis(x)<prob))
#}
lastlp<-array(data=NA,dim=c(out$n.sims,nyear))
dimnames(lastlp)<-list(c(1:out$n.sims),c(sort(unique(dat$year))))
for (xj in sort(unique(as.numeric(factor(dat$year))))) { 
  lastlp[,xj]<-apply(out$sims.array[,,paste("lp[",xj[1],",",1:(max(dat$day)-min(dat$day)+1),"]",sep="")],MARGIN=c(if(out$n.chains>1) 1:2 else 1),findlast.fn)
}
lastlp<-lastlp+min(dat$day)-1
#lastlp[lastlp==max(dat$day)]<-NA
#lastlp[lastlp==min(dat$day)]<-NA
#lastlp[which(lastlp=="Inf")]<-NA
lastlp[which(lastlp=="-Inf")]<-NA

#lastlp<-as.numeric(lastlp)
#countlp<-array(data=NA,dim=c(out$n.sims,nyear))
#dimnames(countlp)<-list(c(1:out$n.sims),c(sort(unique(dat$year))))
#for (xj in sort(unique(as.numeric(factor(dat$year))))) { 
#  countlp[,xj]<-apply(out$sims.array[,,paste("lp[",xj[1],",",1:(max(dat$day)-min(dat$day)+1),"]",sep="")],MARGIN=c(if(out$n.chains>1) 1:2 else 1),count.fn)
#}
#returns "inf" when no estimates are >prob

# summarize estimates
ann.last<-array(NA, dim=c(max(dat$year)-min(dat$year)+1,3),dimnames=list(c(min(dat$year):max(dat$year)),c("mean","lci","uci")))

last<-apply(lastlp,c(2),mean,na.rm=T)
ann.last[names(last),"mean"]<-last
last<-apply(lastlp,c(2),quantile,probs=lci,na.rm=T)
ann.last[names(last),"lci"]<-last
last<-apply(lastlp,c(2),quantile,probs=uci,na.rm=T)
ann.last[names(last),"uci"]<-last

# get estimate of trend in date of peak detectability over years
#firstlp<-as.numeric()
r.last<-matrix(NA,dim(lastlp)[1],2)
for (o in 1:(dim(lastlp)[1])) {
 # if(!is.na(sum(lpmax[o,]))) {
  y<-lastlp[o,]
  y[y=="Inf"]<-NA
    lm(y~as.numeric(colnames(lastlp)))$coefficients->r.last[o,]
  #}    
}
slopevec.last<-as.vector(r.last[,2])
intercept.last<-mean(r.last[,1],na.rm=T)
slope.last<-mean(r.last[,2],na.rm=T)
intercept.last.lci<-quantile(r.last[,1],c(lci),na.rm=T)
intercept.last.uci<-quantile(r.last[,1],c(uci),na.rm=T)

slope.last.lci<-quantile(r.last[,2],c(lci),na.rm=T)
slope.last.uci<-quantile(r.last[,2],c(uci),na.rm=T)

r.last.recent<-matrix(NA,dim(lastlp)[1],2)
for (o in 1:(dim(lastlp)[1])) {
  # if(!is.na(sum(lpmax[o,]))) {
  y<-lastlp[o,24:40]
  y[y=="Inf"]<-NA
  lm(y~as.numeric(colnames(lastlp)[24:40]))$coefficients->r.last.recent[o,]
  #}    
}
slopevec.last.recent<-as.vector(r.last.recent[,2])
intercept.last.recent<-mean(r.last.recent[,1],na.rm=T)
slope.last.recent<-mean(r.last.recent[,2],na.rm=T)
intercept.last.recent.lci<-quantile(r.last.recent[,1],c(lci),na.rm=T)
intercept.last.recent.uci<-quantile(r.last.recent[,1],c(uci),na.rm=T)

slope.last.recent.lci<-quantile(r.last.recent[,2],c(lci),na.rm=T)
slope.last.recent.uci<-quantile(r.last.recent[,2],c(uci),na.rm=T)


### Write results (in console if argument file is not specified in function cat)
if(season=="1"){
cat(paste("summary results",pod,region,season),"\n",
    paste("annual change of activity peak:", round(mean(slopevec,na.rm=T),digits=2),"days"),
    paste("confidence interval from", round(quantile(slopevec,lci,na.rm=T),digits=2),
          "to",round(quantile(slopevec,uci,na.rm=T),digits=2)),
    "\n","mean estimate of activity peak","as date",
        as.character(as.Date(x=c(ann.res[,colnames(ann.res)=="mean"]),origin=c(paste(row.names(ann.res),"-01-01",sep="")))),"\n",
    sep="\n","as day of year",
    paste(rownames(ann.res),round(ann.res[,"mean"])))   
  
  cat(paste("summary results",pod,region,season),"\n",
      paste("annual change of first activity doy:", round(mean(slopevec.first,na.rm=T),digits=2),"days"),
      paste("confidence interval from", round(quantile(slopevec.first,lci,na.rm=T),digits=2),
            "to",round(quantile(slopevec.first,uci,na.rm=T),digits=2)),
      "\n","mean estimate of first activity doy","as date",
      as.character(as.Date(x=c(ann.first[,colnames(ann.first)=="mean"]),origin=c(paste(row.names(ann.first),"-01-01",sep="")))),"\n",
      sep="\n","as day of year",
          paste(rownames(ann.first),round(ann.first[,"mean"])))
  
  cat(paste("summary results",pod,region,season),"\n",
      paste("annual change of last activity doy:", round(mean(slopevec.last,na.rm=T),digits=2),"days"),
      paste("confidence interval from", round(quantile(slopevec.last,lci,na.rm=T),digits=2),
            "to",round(quantile(slopevec.last,uci,na.rm=T),digits=2)),
      "\n","mean estimate of last activity doy","as date",
      as.character(as.Date(x=c(ann.res[,colnames(ann.res)=="mean"]),origin=c(paste(row.names(ann.res),"-01-01",sep="")))),"\n",
         paste(rownames(ann.last),round(ann.last[,"mean"])))
  }
if(season=="2"){
  cat(paste("summary results",pod,region,season),"\n",
      paste("annual change of peak occurrence prob:", round(mean(slopevec,na.rm=T),digits=2),"days"),
      paste("confidence interval from", round(quantile(slopevec,lci,na.rm=T),digits=2),
            "to",round(quantile(slopevec,uci,na.rm=T),digits=2)),
      "\n","mean estimate of activity peak","as date",
      as.character(as.Date(x=c(ann.res[,colnames(ann.res)=="mean"]),origin=c(paste(row.names(ann.res),"-01-01",sep="")))),"\n",
      sep="\n","as day of year",
      paste(rownames(ann.res),round(ann.res[,"mean"])))   
}
#save a dataframe of trends in all three phenophases
df<-rbind(
  c(pod,region,season,"peak",round(mean(slopevec,na.rm=T),digits=2),round(quantile(slopevec,lci,na.rm=T),digits=2),round(quantile(slopevec,uci,na.rm=T),digits=2),round(quantile(slopevec,.025,na.rm=T),digits=2),round(quantile(slopevec,.975,na.rm=T),digits=2)),
  c(pod,region,season,"first",round(mean(slopevec.first,na.rm=T),digits=2),round(quantile(slopevec.first,lci,na.rm=T),digits=2),round(quantile(slopevec.first,uci,na.rm=T),digits=2),round(quantile(slopevec.first,.025,na.rm=T),digits=2),round(quantile(slopevec.first,.975,na.rm=T),digits=2)),
  c(pod,region,season,"last",round(mean(slopevec.last,na.rm=T),digits=2),round(quantile(slopevec.last,lci,na.rm=T),digits=2),round(quantile(slopevec.last,uci,na.rm=T),digits=2),round(quantile(slopevec.last,.025,na.rm=T),digits=2),round(quantile(slopevec.last,.975,na.rm=T),digits=2)),
  c(pod,region,season,"peak.20012016",round(mean(slopevec.recent,na.rm=T),digits=2),round(quantile(slopevec.recent,lci,na.rm=T),digits=2),round(quantile(slopevec.recent,uci,na.rm=T),digits=2),round(quantile(slopevec.recent,.025,na.rm=T),digits=2),round(quantile(slopevec.recent,.975,na.rm=T),digits=2)),
  c(pod,region,season,"first.20012016",round(mean(slopevec.first.recent,na.rm=T),digits=2),round(quantile(slopevec.first.recent,lci,na.rm=T),digits=2),round(quantile(slopevec.first.recent,uci,na.rm=T),digits=2),round(quantile(slopevec.first.recent,.025,na.rm=T),digits=2),round(quantile(slopevec.first.recent,.975,na.rm=T),digits=2)),
  c(pod,region,season,"last.20012016",round(mean(slopevec.last.recent,na.rm=T),digits=2),round(quantile(slopevec.last.recent,lci,na.rm=T),digits=2),round(quantile(slopevec.last.recent,uci,na.rm=T),digits=2),round(quantile(slopevec.last.recent,.025,na.rm=T),digits=2),round(quantile(slopevec.last.recent,.975,na.rm=T),digits=2))
  )

colnames(df)[1:7]<-c("pod","region","season","phase","slope.mn","slope.lci","slope.uci")
cis<-uci-lci
if(assumeSRKW==TRUE){df.name<-paste("analyses/output/",pod,"_",season,region,"_","doy",min(dat$day),"-",max(dat$day),min(dat$year),"-",max(dat$year),"_ci",cis,"_occprobmin",p,"_wrecent_assumeSRKW.csv", sep="")}
if(assumeSRKW==FALSE){df.name<-paste("analyses/output/",pod,"_",season,region,"_","doy",min(dat$day),"-",max(dat$day),min(dat$year),"-",max(dat$year),"_ci",cis,"_occprobmin",p,"_wrecent.csv", sep="")}

write.csv(df,df.name, row.names=FALSE)
#save years and first-last-peak estimates
phen<-cbind(pod,region,season,rownames(ann.res),round(ann.res[,"mean"]),round(ann.res[,"lci"]),round(ann.res[,"uci"]),
            round(ann.first[,"mean"]),round(ann.first[,"lci"]),round(ann.first[,"uci"]),
            round(ann.last[,"mean"]),round(ann.last[,"lci"]),round(ann.last[,"uci"]),round(meanpsi, digits=2))
colnames(phen)<-c("pod","region","season","year","peak.psi","peak.lcl","peak.ucl","first.psi","first.lcl","first.ucl","last.psi","last.lcl","last.ucl", "mean.psi")
if(assumeSRKW==TRUE){phen.name<-paste("analyses/output/",pod,"_",season,region,"_doy",min(dat$day),"-",max(dat$day),"_",min(dat$year),"-",max(dat$year),"_ci",cis,"_occprobmin",p,"_assumeSRKWoccprobdoy.csv", sep="")}
if(assumeSRKW==FALSE){phen.name<-paste("analyses/output/",pod,"_",season,region,"_doy",min(dat$day),"-",max(dat$day),"_",min(dat$year),"-",max(dat$year),"_ci",cis,"_occprobmin",p,"_occprobdoy.csv", sep="")}

write.csv(phen,phen.name, row.names=FALSE)

if(assumeSRKW==TRUE){p.name<-paste("analyses/output/",pod,"_",season,region,"_doy",min(dat$day),"-",max(dat$day),"_",min(dat$year),"-",max(dat$year),"_ci",cis,"_occprobmin",p,"assumeSRKWdetprob.csv", sep="")}
if(assumeSRKW==FALSE){p.name<-paste("analyses/output/",pod,"_",season,region,"_doy",min(dat$day),"-",max(dat$day),"_",min(dat$year),"-",max(dat$year),"_ci",cis,"_occprobmin",p,"detprob.csv", sep="")}

write.csv(meanp, p.name, row.names=TRUE)
if(assumeSRKW==TRUE){p.name<-paste("analyses/output/",pod,"_",season,region,"_doy",min(dat$day),"-",max(dat$day),"_",min(dat$year),"-",max(dat$year),"_ci",cis,"_occprobmin",p,"assumeSRKWdetprob.csv", sep="")}
if(assumeSRKW==FALSE){p.name<-paste("analyses/output/",pod,"_",season,region,"_doy",min(dat$day),"-",max(dat$day),"_",min(dat$year),"-",max(dat$year),"_ci",cis,"_occprobmin",p,"detprob.csv", sep="")}

mean.psi.allyears<-jags.out$BUGSoutput$mean$psi
if(assumeSRKW==TRUE){psi.name<-paste("analyses/output/",pod,"_",season,region,"_doy",min(dat$day),"-",max(dat$day),"_",min(dat$year),"-",max(dat$year),"assumeSRKWmeanoccprob_all.csv", sep="")}
if(assumeSRKW==FALSE){psi.name<-paste("analyses/output/",pod,"_",season,region,"_doy",min(dat$day),"-",max(dat$day),"_",min(dat$year),"-",max(dat$year),"meanoccprob_all.csv", sep="")}

write.csv(mean.psi.allyears, psi.name, row.names=TRUE)

#-----------------------------------------------------------------
# Plot output
#-----------------------------------------------------------------
if(region == "uss"){color = "darkblue"}
if(region == "ps"){color = "yellow1"}

# save plotted results as pdf
if(assumeSRKW==TRUE){pdf(file=paste("analyses/figures/",pod,"/orcaphen_",min(dat$year),"-",max(dat$year),"_",region,"_",season,"_doy",min(dat$day),"-",max(dat$day),"_",pod,"_",p,"assumeSRKWpeakoccprob.pdf", sep=""),width=7,height=7)}

if(assumeSRKW==FALSE){pdf(file=paste("analyses/figures/",pod,"/orcaphen_",min(dat$year),"-",max(dat$year),"_",region,"_",season,"_doy",min(dat$day),"-",max(dat$day),"_",pod,"_",p,"peakoccprob.pdf", sep=""),width=7,height=7)}

### plot estimates of peak prob of occurrence over all years
#quartz(width=7, height=6)
par(mfrow=c(1,1),mai=c(1,1,1,0.5))
x=rownames(ann.res)
y=ann.res[,"mean"]
seasname<-c("winter","summer")

plot(x,y,xlab="",ylab="",axes=F,main=paste("Date of Peak Occurence Probability","\n",pod," Pod",seasname[as.numeric(season)],region),
     ylim=range(dat$day),pch=16,type="l", lwd=1.5,col=color)
polygon(c(rev(x),x),c(rev(ann.res[,"uci"]),ann.res[,"lci"]),col=alpha(color,0.2),lty=0)

axis(side=1,at=x)
if(season==2){
  axis(side=2,at=c(92,122,152,183,214,244,274,303),
       labels=c("1Apr","1May","1Jun","1Jul","1Aug","1Sept","1Oct","1Nov"))
}
if(season==1){
  axis(side=2,at=c(182,213,244,274,305,335,366),
       labels=c("1Jul","1Aug","1Sept","1Oct","1Nov","1Dec","1Jan"))}

if(season ==3){
  axis(side=2,at=c(1,92,183,274,365),
       labels=c("1Jan","1Apr","1Jul","1Oct","31Dec"))
  
}
#for (o in 1:500) {
   #if(!is.na(sum(lpmax[o,]))) {
#  mod<-lm(lpmax[o,]~as.numeric(colnames(lpmax)))$coefficients->r[o,]
#  abline(mod,col=alpha("salmon",0.2),lwd=2)
  
  #}    
#}
#clip(2008,2017, min(y), max(y))
if(season==1){abline(a=intercept,b=slope,col="gold3",lwd=2)
    mtext("B)",side = 3, line = 0, adj =0)}
if(season==2){abline(a=intercept,b=slope,col="midnightblue",lwd=2)
  mtext("D)",side = 3, line = 0, adj =0)}

dev.off()
#plot only recent years
# save plotted results as pdf
if(assumeSRKW==TRUE){pdf(file=paste("analyses/figures/",pod,"/orcaphen_","2001-",max(dat$year),"_",region,"_",season,"_doy",min(dat$day),"-",max(dat$day),"_",pod,"_",p,"assumeSRKWpeakoccprob.pdf", sep=""),width=7,height=7)}

if(assumeSRKW==FALSE){pdf(file=paste("analyses/figures/",pod,"/orcaphen_","2001-",max(dat$year),"_",region,"_",season,"_doy",min(dat$day),"-",max(dat$day),"_",pod,"_",p,"peakoccprob.pdf", sep=""),width=7,height=7)}

### plot estimates of peak occurence over only recent years
#quartz(width=7, height=6)
par(mfrow=c(1,1),mai=c(1,1,1,0.5))
x=rownames(ann.res)[24:length(rownames(ann.res))]
y=ann.res[,"mean"][24:length(rownames(ann.res))]
seasname<-c("winter","summer")

plot(x,y,xlab="",ylab="",axes=F,main=paste("Date of Peak Occurence Probability","\n",pod," Pod",seasname[as.numeric(season)],region),
     ylim=range(dat$day),pch=16,type="l", lwd=1.5,col=color)
polygon(c(rev(x),x),c(rev(ann.res[,"uci"][24:length(rownames(ann.res))]),ann.res[,"lci"][24:length(rownames(ann.res))]),col=alpha(color,0.2),lty=0)

axis(side=1,at=x)
if(season==2){
  axis(side=2,at=c(92,122,152,183,214,244,274,303),
       labels=c("1Apr","1May","1Jun","1Jul","1Aug","1Sept","1Oct","1Nov"))
}
if(season==1){
  axis(side=2,at=c(182,213,244,274,305,335,366),
       labels=c("1Jul","1Aug","1Sept","1Oct","1Nov","1Dec","1Jan"))}

if(season ==3){
  axis(side=2,at=c(1,92,183,274,365),
       labels=c("1Jan","1Apr","1Jul","1Oct","31Dec"))
  
}
#for (o in 1:500) {
#if(!is.na(sum(lpmax[o,]))) {
#  mod<-lm(lpmax[o,]~as.numeric(colnames(lpmax)))$coefficients->r[o,]
#  abline(mod,col=alpha("salmon",0.2),lwd=2)

#}    
#}
#clip(2008,2017, min(y), max(y))
if(season==1){abline(a=intercept,b=slope,col="gold3",lwd=2)
  mtext("B)",side = 3, line = 0, adj =0)}
if(season==2){abline(a=intercept,b=slope,col="midnightblue",lwd=2)
  mtext("D)",side = 3, line = 0, adj =0)}

dev.off()



#-----------------------------------------------------------------
# Plot first date detectability > selected prob  
#-----------------------------------------------------------------
# save plotted results as pdf
if(assumeSRKW==TRUE){pdf(file=paste("analyses/figures/",pod,"/orcaphen",min(dat$year),"-",max(dat$year),"_",region,"_",season,"_doy",min(dat$day),"-",max(dat$day),"_",pod,"_first_",prob,"assumeSRKWoccprob.pdf", sep=""),width=7,height=7)}
if(assumeSRKW==FALSE){pdf(file=paste("analyses/figures/",pod,"/orcaphen",min(dat$year),"-",max(dat$year),"_",region,"_",season,"_doy",min(dat$day),"-",max(dat$day),"_",pod,"_first_",prob,"occprob.pdf", sep=""),width=7,height=7)}

### plot estimates of first det.prob > XX over all years
#quartz(width=7, height=6)
par(mfrow=c(1,1),mai=c(1,1,1,0.5))
x=rownames(ann.first)
y=ann.first[,"mean"]
seasname<-c("winter","summer")
plot(x,y,xlab="",ylab="",axes=F,main=paste("First Detection Probability >",p,"\n",pod," Pod",seasname[as.numeric(season)],region),
     ylim=range(dat$day),pch=16,type="l", lwd=1.5,col=color)
polygon(c(rev(x),x),c(rev(ann.first[,"uci"]),ann.first[,"lci"]),col=alpha(color,0.2),lty=0)

axis(side=1,at=x)
if(season==2){
  axis(side=2,at=c(92,122,152,183,214,244,274,303),
       labels=c("1Apr","1May","1Jun","1Jul","1Aug","1Sept","1Oct","1Nov"))
}
if(season==1){
  axis(side=2,at=c(182,213,244,274,305,335,366),
       labels=c("1Jul","1Aug","1Sept","1Oct","1Nov","1Dec","1Jan"))
}
if(season==3){
  axis(side=2,at=c(1,92,183,274,365),
       labels=c("1Jan","1Apr","1Jul","1Oct","31Dec"))
}

#for (o in 1:500) {
#  #if(!is.na(sum(lpmax[o,]))) {
#  mod.first<-lm(firstlp[o,]~as.numeric(colnames(firstlp)))$coefficients->r.first[o,]
#  abline(mod.first,col=alpha("grey",0.2),lwd=2)
  
  #}    
#}
if(season==1){abline(a=intercept.first,b=slope.first,col="gold3",lwd=2)}
if(season==2){abline(a=intercept.first,b=slope.first,col="midnightblue",lwd=2)}

dev.off()

# save plotted results as pdf
if(assumeSRKW==TRUE){pdf(file=paste("analyses/figures/",pod,"/orcaphen",min(dat$year),"-",max(dat$year),"_",region,"_",season,"_doy",min(dat$day),"-",max(dat$day),"_",pod,"last_",p,"assumeSRKWoccprob.pdf", sep=""),width=7,height=7)}
if(assumeSRKW==FALSE){pdf(file=paste("analyses/figures/",pod,"/orcaphen",min(dat$year),"-",max(dat$year),"_",region,"_",season,"_doy",min(dat$day),"-",max(dat$day),"_",pod,"last_",p,"occprob.pdf", sep=""),width=7,height=7)}

### plot estimates of last detectability > selected prob over all years
#quartz(width=7, height=6)
par(mfrow=c(1,1),mai=c(1,1,1,0.5))
x=rownames(ann.last)
y=ann.last[,"mean"]
seasname<-c("winter","summer")
plot(x,y,xlab="",ylab="",axes=F,main=paste("Last Occurence Probability >",p,"\n",pod," Pod",seasname[as.numeric(season)],region),
     ylim=range(dat$day),pch=16,type="l", lwd=1.5,col=color)
polygon(c(rev(x),x),c(rev(ann.last[,"uci"]),ann.last[,"lci"]),col=alpha(color,0.2),lty=0)

axis(side=1,at=x)
if(season==2){
  axis(side=2,at=c(92,122,152,183,214,244,274,303),
       labels=c("1Apr","1May","1Jun","1Jul","1Aug","1Sept","1Oct","1Nov"))
}
if(season==1){
  axis(side=2,at=c(182,213,244,274,305,335,366),
       labels=c("1Jul","1Aug","1Sept","1Oct","1Nov","1Dec","1Jan"))
}
if(season==3){
  axis(side=2,at=c(1,92,183,274,365),
       labels=c("1Jan","1Apr","1Jul","1Oct","31Dec"))
}

#for (o in 1:500) {
#  #if(!is.na(sum(lpmax[o,]))) {
#  mod.last<-lm(lastlp[o,]~as.numeric(colnames(lastlp)))$coefficients->r.last[o,]
#  abline(mod.last,col=alpha("grey",0.2),lwd=2)
#  
  #}    
#}
if(season==1){abline(a=intercept,b=slope,col="gold3",lwd=2)
  mtext("B)",side = 3, line = 0, adj =0)}
if(season==2){abline(a=intercept,b=slope,col="midnightblue",lwd=2)
  mtext("D)",side = 3, line = 0, adj =0)}

dev.off()




### Plot annual detectability pattern
# loop over all years
years<-sort(unique(as.numeric((dat$year))))
seasonname<-c("winter","summer","wholeyear")
for (xj in 1:length(years)) {
  j<-years[xj]
  
  # Get BUGS estimates
  res.chains<-out$sims.array[,,paste("lp[",xj[1],",",1:(max(dat$day)-min(dat$day)+1),"]",sep="")]
  res=plogis(apply(res.chains,MARGIN=c(length(dim(res.chains))),quantile,probs=c(.05,.5,.95)))
  
  ### Plot "naive" estimate of occurence
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
  if(assumeSRKW==TRUE){figname<-paste("orcaphen",j,region,seasonname[as.numeric(season)],"doy",min(dat$day),"-",max(dat$day),"_",pod,"_",p, "assumeSRKW.pdf",sep="_")}
  if(assumeSRKW==FALSE){figname<-paste("orcaphen",j,region,seasonname[as.numeric(season)],"doy",min(dat$day),"-",max(dat$day),"_",pod,"_",p, ".pdf",sep="_")}
  
  pdf(file.path(figpath, figname), width = 9, height = 6)
  
  #quartz()
  
  
  x<-barplot(as.numeric(barheight[min(dat$day):max(dat$day)]),
          width=1,space=0,ylim=c(0,1),xlab="", ylab="Occurence Probability", 
          main=paste(pod," pod",j),border=NA,axes=F)#ylim:max(res[3,])
  
  ### Plot model estimates  
  # plot seasonal estimates of detectability p
  lines(res[3,],lty=3,col=1,lwd=2.5) # lower bound of the 90% CI
  lines(res[2,],lty=1,col=1,lwd=2) # median
  lines(res[1,],lty=3,col=1,lwd=2.5) # upper bound of the 90% CI
  axis(2)
   if(season==1){
    axis(side=1,at=c(183-183,213-183,244-183,274-183,305-183,335-183,366-183),
         labels=c("1Jul","1Aug","1Sept","1Oct","1Nov","1Dec","1Jan"))
   }
  if(season==2){
    axis(side=1,at=c(92-92,122-92,152-92,183-92,214-92,244-92,274-92,303-92),
         labels=c("1Apr","1May","1Jun","1Jul","1Aug","1Sept","1Oct","1Nov"))
  }
  
  if(season==3){
    axis(side=1,at=c(1,92,183,274,365),
         labels=c("1Jan","1Apr","1Jul","1Oct","31Dec"))
  }
  dev.off()
  }

}


