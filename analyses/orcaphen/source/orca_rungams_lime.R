tail(limewdaysabs)
unique(limewdaysabs$AllSRpres)
#Fit a bernouli gam with presence of SRs as the response
y<-limewdaysabs$AllSRpres
j<-limewdaysabs$Jpres
k<-limewdaysabs$Kpres
l<-limewdaysabs$Lpres

head(limewdaysabs)
doy<-limewdaysabs$day
year<-as.factor(limewdaysabs$year)
#mod<-gam(y ~ s(doy,by=year),family=binomial)
library(sme)
fit<-sme(y,doy,year,criteria="AIC",family=binomial)
fitj<-sme(j,doy,year,criteria="AIC",family=binomial)
fitk<-sme(k,doy,year,criteria="AIC",family=binomial)
fitl<-sme(l,doy,year,criteria="AIC",family=binomial)

quartz()
plot(fit,type="diagnostic")
fit$info
quartz()
plot(fit,type="model",xlab="doy",ylab="probability of occurrence")
length(fit$fitted)
summary(fit)

plot(fit,type="raw",showModelFits=TRUE,xlab="Day of Year",ylab="SRKW Presence at Lime Kiln")
plot(fitj,type="raw",showModelFits=TRUE,xlab="Day of Year",ylab="J Presence at Lime Kiln")
plot(fitk,type="raw",showModelFits=TRUE,xlab="Day of Year",ylab="K Presence at Lime Kiln")
plot(fitl,type="raw",showModelFits=TRUE,xlab="Day of Year",ylab="L Presence at Lime Kiln")

head(fit$fitted)
preds<-as.data.frame(fit$fitted)
predsj<-as.data.frame(fitj$fitted)
predsk<-as.data.frame(fitk$fitted)
predsl<-as.data.frame(fitl$fitted)

preds$year<-substr(rownames(preds),1,4)
#preds$n<-substr(rownames(preds),5,length(rownames(preds)))
preds$doy<-limewdaysabs$day
preds<-cbind(preds,predsj$`fitj$fitted`,predsk$`fitk$fitted`,predsl$`fitl$fitted`)
colnames(preds)[1]<-"prob.occ"
colnames(preds)[4:6]<-c("jprob.occ","kprob.occ","lprob.occ")

preds$prob.occ[preds$prob.occ<0]<-0
preds$jprob.occ[preds$jprob.occ<0]<-0
preds$kprob.occ[preds$kprob.occ<0]<-0
preds$lprob.occ[preds$lprob.occ<0]<-0


write.csv(preds,"analyses/output/limekiln.srkw.gamests.csv", row.names=FALSE)
