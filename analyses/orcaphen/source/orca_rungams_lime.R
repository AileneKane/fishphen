tail(limewdaysabs)
unique(limewdaysabs$AllSRpres)
#Fit a bernouli gam with presence of SRs as the response
y<-limewdaysabs$AllSRpres
head(limewdaysabs)
doy<-limewdaysabs$day
year<-as.factor(limewdaysabs$year)
#mod<-gam(y ~ s(doy,by=year),family=binomial)
library(sme)
fit<-sme(y,doy,year,criteria="AIC",family=binomial)

quartz()
plot(fit,type="diagnostic")
fit$info
quartz()
plot(fit,type="model",xlab="doy",ylab="probability of occurrence")
length(fit$fitted)
summary(fit)

plot(fit,type="raw",showModelFits=TRUE,xlab="Day of Year",ylab="SRKW Presence at Lime Kiln")
head(fit$fitted)
preds<-as.data.frame(fit$fitted)
preds$year<-substr(rownames(preds),1,4)
#preds$n<-substr(rownames(preds),5,length(rownames(preds)))
preds$doy<-limewdaysabs$day

colnames(preds)[1]<-"prob.occ"
preds$prob.occ[preds$prob.occ<0]<-0
head(preds)
write.csv(preds,"analyses/output/limekiln.srkw.gamests.csv", row.names=FALSE)
