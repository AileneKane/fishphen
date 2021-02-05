#tail(limewdaysabs)
#unique(limewdaysabs$AllSRpres)
#Fit a bernouli gam with presence of SRs as the response

#limewdaysabs$year<-as.factor(limewdaysabs$year)
#goo<-limewdaysabs
#try fitting gams in brms!

# m1 <- brm(AllSRpres ~ s(day) + (1|year),
#           data=limewdaysabs,
#           family =bernoulli(), cores = 4,
#           iter = 4000, warmup = 1000, thin = 10,
#           control = list(adapt_delta = 0.99))
# 
# summary(m1)
# windows()

# m2 <- brm(AllSRpres ~ s(day) + (day|year),
#           data=limewdaysabs,
#           family =bernoulli(), cores = 4,
#           iter = 4000, warmup = 1000, thin = 10,
#           control = list(adapt_delta = 0.99))
# save(m2, file="analyses/output/sr.brms.Rda")

load("analyses/output/sr.brms.Rda")

# 
# j2 <- brm(Jpres ~ s(day) + (day|year),
#           data=limewdaysabs,
#           family =bernoulli(), cores = 4,
#           iter = 4000, warmup = 1000, thin = 10,
# #           control = list(adapt_delta = 0.99, max_treedepth=15))
# # save(j2, file="analyses/output/j.brms.Rda")
# 
# load("analyses/output/j.brms.Rda")
# 
# # k2 <- brm(Kpres ~ s(day) + (day|year),
# #           data=limewdaysabs,
# #           family =bernoulli(), cores = 4,
# #           iter = 4000, warmup = 1000, thin = 10,
# #           control = list(adapt_delta = 0.99, max_treedepth=15))
# # save(k2, file="analyses/output/k.brms.Rda")
# 
# load("analyses/output/k.brms.Rda")
# 
# # l2 <- brm(Lpres ~ s(day) + (day|year),
# #           data=limewdaysabs,
# #           family =bernoulli(), cores = 4,
# #           iter = 4000, warmup = 1000, thin = 10,
# #           control = list(adapt_delta = 0.99, max_treedepth=15 ))
# # save(l2, file="analyses/output/l.brms.Rda")
# load("analyses/output/l.brms.Rda")
# 
# prob.occ.95<-cbind(limewdaysabs$year,limewdaysabs$day,fitted(m2),fitted(j2),fitted(k2),fitted(l2))
# 
# colnames(prob.occ.95)<-c("year","doy", paste("SRprob",colnames(fitted(m2)),sep="."),
#                                     paste("Jprob",colnames(fitted(j2)),sep="."),
#                                     paste("Kprob",colnames(fitted(k2)),sep="."),
#                                     paste("Lprob",colnames(fitted(l2)),sep="."))
# prob.occ.90<-cbind(limewdaysabs$year,limewdaysabs$day,fitted(m2,probs=c(0.05,0.95)),fitted(j2,probs=c(0.05,0.95)),fitted(k2,probs=c(0.05,0.95)),fitted(l2,probs=c(0.05,0.95)))
# 
# colnames(prob.occ.90)<-c("year","doy", paste("SRprob",colnames(fitted(m2,probs=c(0.05,0.95))),sep="."),
#                          paste("Jprob",colnames(fitted(j2,probs=c(0.05,0.95))),sep="."),
#                          paste("Kprob",colnames(fitted(k2,probs=c(0.05,0.95))),sep="."),
#                          paste("Lprob",colnames(fitted(l2,probs=c(0.05,0.95))),sep="."))
# prob.occ.50<-cbind(limewdaysabs$year,limewdaysabs$day,fitted(m2,probs=c(0.25,0.75)),fitted(j2,probs=c(0.25,0.75)),fitted(k2,probs=c(0.25,0.75)),fitted(l2,probs=c(0.25,0.75)))
# 
# colnames(prob.occ.50)<-c("year","doy", paste("SRprob",colnames(fitted(m2,probs=c(0.25,0.75))),sep="."),
#                          paste("Jprob",colnames(fitted(j2,probs=c(0.25,0.75))),sep="."),
#                          paste("Kprob",colnames(fitted(k2,probs=c(0.25,0.75))),sep="."),
#                          paste("Lprob",colnames(fitted(l2,probs=c(0.25,0.75))),sep="."))
# 
# prob.occ.75<-cbind(limewdaysabs$year,limewdaysabs$day,fitted(m2,probs=c(0.125,0.875)),fitted(j2,probs=c(0.125,0.875)),fitted(k2,probs=c(0.125,0.875)),fitted(l2,probs=c(0.125,0.875)))
# 
# colnames(prob.occ.75)<-c("year","doy", paste("SRprob",colnames(fitted(m2,probs=c(0.125,0.875))),sep="."),
#                          paste("Jprob",colnames(fitted(j2,probs=c(0.125,0.875))),sep="."),
#                          paste("Kprob",colnames(fitted(k2,probs=c(0.125,0.875))),sep="."),
#                          paste("Lprob",colnames(fitted(l2,probs=c(0.125,0.875))),sep="."))
# 
# #Save model results
# write.csv(prob.occ.95,"analyses/output/lime_prob.occ.95.csv", row.names = FALSE)
# write.csv(prob.occ.90,"analyses/output/lime_prob.occ.90.csv", row.names = FALSE)
# write.csv(prob.occ.50,"analyses/output/lime_prob.occ.50.csv", row.names = FALSE)
# write.csv(prob.occ.75,"analyses/output/lime_prob.occ.75.csv", row.names = FALSE)


#Make figures of model estimated whale days with 75 percent
limedays<-read.csv("analyses/output/lime_prob.occ.75.csv", header=TRUE)
limedays$year<-as.numeric(as.character(limewdaysabs$year))
limedays<-limedays[limedays$year>1991,]

yearsum<-aggregate(limedays$SRprob.Estimate,by=list(limedays$year),sum)

jsum<-aggregate(limedays$Jprob.Estimate,by=list(limedays$year),sum)
ksum<-aggregate(limedays$Kprob.Estimate,by=list(limedays$year),sum)
lsum<-aggregate(limedays$Lprob.Estimate,by=list(limedays$year),sum)
yearsum.lc<-aggregate(limedays$SRprob.Q12.5,by=list(limedays$year),sum)
jsum.lc<-aggregate(limedays$Jprob.Q12.5,by=list(limedays$year),sum)
ksum.lc<-aggregate(limedays$Kprob.Q12.5,by=list(limedays$year),sum)
lsum.lc<-aggregate(limedays$Lprob.Q12.5,by=list(limedays$year),sum)
yearsum.uc<-aggregate(limedays$SRprob.Q87.5,by=list(limedays$year),sum)
jsum.uc<-aggregate(limedays$Jprob.Q87.5,by=list(limedays$year),sum)
ksum.uc<-aggregate(limedays$Kprob.Q87.5,by=list(limedays$year),sum)
lsum.uc<-aggregate(limedays$Lprob.Q87.5,by=list(limedays$year),sum)

colnames(yearsum)<-colnames(jsum)<-colnames(ksum)<-colnames(lsum)<-
  colnames(yearsum.lc)<-colnames(jsum.lc)<-colnames(ksum.lc)<-colnames(lsum.lc)<-
  colnames(yearsum.uc)<-colnames(jsum.uc)<-colnames(ksum.uc)<-colnames(lsum.uc)<-c("year","wdays")

#sum actual wale days for comparison
limewdaysabs<-limewdaysabs[as.numeric(as.character(limewdaysabs$year))>1991,]
jsumobs<-aggregate(limewdaysabs$Jpres,by=list(limewdaysabs$year),sum)
ksumobs<-aggregate(limewdaysabs$Kpres,by=list(limewdaysabs$year),sum)
lsumobs<-aggregate(limewdaysabs$Lpres,by=list(limewdaysabs$year),sum)
allsumobs<-aggregate(limewdaysabs$AllSRpres,by=list(limewdaysabs$year),sum)
colnames(jsumobs)<-colnames(ksumobs)<-colnames(lsumobs)<-colnames(allsumobs)<-c("year","wdays")
png(filename="analyses/orcaphen/figures/modwhaledays_lime.png",height=480,width=960)
#quartz(height=6,width=12)
par(mfrow=c(2,3))
plot(as.numeric(yearsum$year),yearsum$wdays,ylab= "Year", xlab= "Number of Modeled Whale Days", bty="l", type="l", col=alpha("darkblue",0.8),lwd=2,main = "All Pods")
lines(as.numeric(allsumobs$year),allsumobs$wdays, type="l", col=alpha("darkblue",0.8),, lty=2,lwd=2)
polygon(c(rev(as.numeric(yearsum$year)),as.numeric(yearsum$year)), c(rev(yearsum.uc$wdays), yearsum.lc$wdays), col = alpha("darkblue", 0.2), border = NA)

mtext("A)", side = 3, line = 1, adj=0)
legend("bottomleft",legend=c("Observed", "Model estimate"), lty=c(1,2), col="darkblue", lwd=2, bty="n")
plot(as.numeric(jsum$year),jsum$wdays,ylab= "Year", xlab= "Number of Modeled Whale Days", bty="l", type="l", col="darkblue",lwd=2, main = "J Pod")
lines(as.numeric(jsumobs$year),jsumobs$wdays, type="l", col=alpha("darkblue",0.8),, lty=2,lwd=2)
polygon(c(rev(as.numeric(jsum$year)),as.numeric(jsum$year)), c(rev(jsum.uc$wdays), jsum.lc$wdays), col = alpha("darkblue", 0.2), border = NA)

mtext("B)", side = 3, line = 1, adj=0)

plot(as.numeric(ksum$year),ksum$wdays,ylab= "Year", xlab= "Number of Modeled Whale Days", bty="l", type="l", col="darkblue",lwd=2, main = "K Pod")
lines(as.numeric(ksumobs$year),ksumobs$wdays, type="l", col=alpha("darkblue",0.8),, lty=2,lwd=2)
polygon(c(rev(as.numeric(ksum$year)),as.numeric(ksum$year)), c(rev(ksum.uc$wdays), ksum.lc$wdays), col = alpha("darkblue", 0.2), border = NA)

mtext("C)", side = 3, line = 1, adj=0)

plot(as.numeric(lsum$year),lsum$wdays,ylab= "Year", xlab= "Number of Modeled Whale Days", bty="l", type="l", col="darkblue",lwd=2, main = "L Pod")
lines(as.numeric(lsumobs$year),lsumobs$wdays, type="l", col=alpha("darkblue",0.8), lty=2,lwd=2)
polygon(c(rev(as.numeric(lsum$year)),as.numeric(lsum$year)), c(rev(lsum.uc$wdays), lsum.lc$wdays), col = alpha("darkblue", 0.2), border = NA)

mtext("D)", side = 3, line = 1, adj=0)


#Add modeled salmon cpue and data cpue
chindat<-read.csv("analyses/output/albionchiphen_allyear.csv", header = TRUE)
chindat<-chindat[chindat$year>1993,]
chindat<-chindat[chindat$year<2018,]

chinab<-read.csv("analyses/output/albiongamests.csv", header = TRUE)
#restrict days
chinab<-chinab[chinab$doy<230,]
chinab<-chinab[chinab$doy>90,]
chinab<-chinab[chinab$year>1993,]
chinab<-chinab[chinab$year<2018,]

chinsum<-aggregate(chinab$cpue.est,by=list(chinab$year),sum)
colnames(chinsum)<-c("year","totalcpue")
plot(as.numeric(chindat$year),chindat$alltotal,ylim=c(0,360),xlab= "Year", ylab= "Total Estimated Chinook Abundance", bty="l", lty=1,type="l", col="salmon",lwd=2)
lines(as.numeric(chinsum$year),chinsum$totalcpue,lty=2, col="salmon",lwd=2)

mtext("E)", side = 3, line = 1, adj=0)

dev.off()
