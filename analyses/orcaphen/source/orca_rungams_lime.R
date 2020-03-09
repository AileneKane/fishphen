#tail(limewdaysabs)
#unique(limewdaysabs$AllSRpres)
#Fit a bernouli gam with presence of SRs as the response

limewdaysabs$year<-as.factor(limewdaysabs$year)

#try fitting gams in brms!

# m1 <- brm(AllSRpres ~ s(day) + (1|year),
#           data=limewdaysabs,
#           family =bernoulli(), cores = 4,
#           iter = 4000, warmup = 1000, thin = 10,
#           control = list(adapt_delta = 0.99))
# 
# summary(m1)
# windows()

m2 <- brm(AllSRpres ~ s(day) + (day|year),
          data=limewdaysabs,
          family =bernoulli(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99))
save(m2, file="analyses/output/sr.brms.Rda")


j2 <- brm(Jpres ~ s(day) + (day|year),
          data=limewdaysabs,
          family =bernoulli(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99, max_treedepth=15))
save(j2, file="analyses/output/j.brms.Rda")

k2 <- brm(Kpres ~ s(day) + (day|year),
          data=limewdaysabs,
          family =bernoulli(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99, max_treedepth=15))
save(k2, file="analyses/output/k.brms.Rda")

l2 <- brm(Lpres ~ s(day) + (day|year),
          data=limewdaysabs,
          family =bernoulli(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99, max_treedepth=15 ))
save(l2, file="analyses/output/l.brms.Rda")


prob.occ.95<-cbind(limewdaysabs$year,limewdaysabs$day,fitted(m2),fitted(j2),fitted(k2),fitted(l2))

colnames(prob.occ.95)<-c("year","doy", paste("SRprob",colnames(fitted(m2)),sep="."),
                                    paste("Jprob",colnames(fitted(j2)),sep="."),
                                    paste("Kprob",colnames(fitted(k2)),sep="."),
                                    paste("Lprob",colnames(fitted(l2)),sep="."))
prob.occ.90<-cbind(limewdaysabs$year,limewdaysabs$day,fitted(m2,probs=c(0.05,0.95)),fitted(j2,probs=c(0.05,0.95)),fitted(k2,probs=c(0.05,0.95)),fitted(l2,probs=c(0.05,0.95)))

colnames(prob.occ.90)<-c("year","doy", paste("SRprob",colnames(fitted(m2,probs=c(0.05,0.95))),sep="."),
                         paste("Jprob",colnames(fitted(j2,probs=c(0.05,0.95))),sep="."),
                         paste("Kprob",colnames(fitted(k2,probs=c(0.05,0.95))),sep="."),
                         paste("Lprob",colnames(fitted(l2,probs=c(0.05,0.95))),sep="."))
prob.occ.50<-cbind(limewdaysabs$year,limewdaysabs$day,fitted(m2,probs=c(0.25,0.75)),fitted(j2,probs=c(0.25,0.75)),fitted(k2,probs=c(0.25,0.75)),fitted(l2,probs=c(0.25,0.75)))

colnames(prob.occ.50)<-c("year","doy", paste("SRprob",colnames(fitted(m2,probs=c(0.25,0.75))),sep="."),
                         paste("Jprob",colnames(fitted(j2,probs=c(0.25,0.75))),sep="."),
                         paste("Kprob",colnames(fitted(k2,probs=c(0.25,0.75))),sep="."),
                         paste("Lprob",colnames(fitted(l2,probs=c(0.25,0.75))),sep="."))
#Save model results
write.csv(prob.occ.95,"analyses/output/lime_prob.occ.95.csv", row.names = FALSE)
write.csv(prob.occ.90,"analyses/output/lime_prob.occ.90.csv", row.names = FALSE)
write.csv(prob.occ.50,"analyses/output/lime_prob.occ.50.csv", row.names = FALSE)


#Make figures of model estimated whale days
limedays<-read.csv("analyses/output/lime_prob.occ.50.csv", header=TRUE)
limedays$year<-as.numeric(limewdaysabs$year)

yearsum<-aggregate(limedays$SRprob.Estimate,by=list(limedays$year),sum)
jsum<-aggregate(limedays$Jprob.Estimate,by=list(limedays$year),sum)
ksum<-aggregate(limedays$Kprob.Estimate,by=list(limedays$year),sum)
lsum<-aggregate(limedays$Lprob.Estimate,by=list(limedays$year),sum)
colnames(yearsum)<-colnames(jsum)<-colnames(ksum)<-colnames(lsum)<-c("year","wdays")
png(filename="analyses/orcaphen/figures/modwhaledays_lime.png",height=480,width=960)
#windows(height=6,width=12)
par(mfrow=c(1,4))
plot(as.numeric(yearsum$year),yearsum$wdays,ylab= "Year", xlab= "Number of Modeled Whale Days", bty="l", type="l", col="darkblue",lwd=2,main = "All Pods")
mtext("A)", side = 3, line = 1, adj=0)
plot(as.numeric(jsum$year),jsum$wdays,ylab= "Year", xlab= "Number of Modeled Whale Days", bty="l", type="l", col="darkblue",lwd=2, main = "J Pod")
mtext("B)", side = 3, line = 1, adj=0)

plot(as.numeric(ksum$year),ksum$wdays,ylab= "Year", xlab= "Number of Modeled Whale Days", bty="l", type="l", col="darkblue",lwd=2, main = "K Pod")
mtext("C)", side = 3, line = 1, adj=0)

plot(as.numeric(lsum$year),lsum$wdays,ylab= "Year", xlab= "Number of Modeled Whale Days", bty="l", type="l", col="darkblue",lwd=2, main = "L Pod")
mtext("D)", side = 3, line = 1, adj=0)

dev.off()


#Look at model results          
# windows()
# conditional_effects(m2, surface = TRUE)
# 
# 
# windows()
# conditional_effects(j2, surface = TRUE)
# windows()
# conditional_effects(k2, surface = TRUE)
# windows()
# conditional_effects(l2, surface = TRUE)
