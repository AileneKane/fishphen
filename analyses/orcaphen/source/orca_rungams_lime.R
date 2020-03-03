tail(limewdaysabs)
unique(limewdaysabs$AllSRpres)
#Fit a bernouli gam with presence of SRs as the response
y<-limewdaysabs$AllSRpres
j<-limewdaysabs$Jpres
k<-limewdaysabs$Kpres
l<-limewdaysabs$Lpres

head(limewdaysabs)
doy<-limewdaysabs$day
limewdaysabs$year<-as.factor(limewdaysabs$year)

#try fitting gams in brms!

m1 <- brm(AllSRpres ~ s(day) + (1|year),
          data=limewdaysabs,
          family =bernoulli(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99))

summary(m1)
windows()

m2 <- brm(AllSRpres ~ s(day) + (day|year),
          data=limewdaysabs,
          family =bernoulli(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99))


j2 <- brm(Jpres ~ s(day) + (day|year),
          data=limewdaysabs,
          family =bernoulli(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99))
k2 <- brm(Kpres ~ s(day) + (day|year),
          data=limewdaysabs,
          family =bernoulli(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99))
l2 <- brm(Lpres ~ s(day) + (day|year),
          data=limewdaysabs,
          family =bernoulli(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99))


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
prob.occ.50<-cbind(limewdaysabs$year,limewdaysabs$day,fitted(m2,probs=c(0.25,0.75)),fitted(j2,probs=c(0.25,0.75)),fitted(k2,probs=c(0.25,0.75)),fitted(l2,probs=c(0.05,0.95)))

colnames(prob.occ.50)<-c("year","doy", paste("SRprob",colnames(fitted(m2,probs=c(0.25,0.75))),sep="."),
                         paste("Jprob",colnames(fitted(j2,probs=c(0.25,0.75))),sep="."),
                         paste("Kprob",colnames(fitted(k2,probs=c(0.25,0.75))),sep="."),
                         paste("Lprob",colnames(fitted(l2,probs=c(0.25,0.75))),sep="."))

write.csv(prob.occ.95,"analyses/output/J_lime_prob.occ.95.csv", row.names = FALSE)
write.csv(prob.occ.90,"analyses/output/J_lime_prob.occ.90.csv", row.names = FALSE)
write.csv(prob.occ.50,"analyses/output/J_lime_prob.occ.50.csv", row.names = FALSE)

          
          
          windows()
conditional_effects(m2, surface = TRUE)


windows()
conditional_effects(j2, surface = TRUE)
windows()
conditional_effects(k2, surface = TRUE)
windows()
conditional_effects(l2, surface = TRUE)
