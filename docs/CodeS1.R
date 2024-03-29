##########################################################
### Model code for `Shifting phenology of an endangered ###
#### apex predator tracks changes in its favored prey' ####
##### Appendix 1. Code for Lime Kiln and Albion data ######
######## See Supplemental Materials For details ###########
##### By Ailene Ettinger, ailene.ettinger@tnc.org #########
###########################################################
##1.Southern resident killer whale presence at Lime Kiln ##
### limedat = SRKW presence/absence data from Lime Kiln ###
###########################################################

m2 <- brm(AllSRpres ~ s(day) + (day|year),
          data=limedat,
          family =bernoulli(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99))
save(m2, file="analyses/output/sr.brms.Rda")

j2 <- brm(Jpres ~ s(day) + (day|year),
          data=limedat,
          family =bernoulli(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99, max_treedepth=15))
save(j2, file="analyses/output/j.brms.Rda")

k2 <- brm(Kpres ~ s(day) + (day|year),
          data=limedat,
          family =bernoulli(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99, max_treedepth=15))
save(k2, file="analyses/output/k.brms.Rda")

l2 <- brm(Lpres ~ s(day) + (day|year),
          data=limedat,
          family =bernoulli(), cores = 4,
          iter = 4000, warmup = 1000, thin = 10,
          control = list(adapt_delta = 0.99, max_treedepth=15 ))
save(l2, file="analyses/output/l.brms.Rda")

###########################################################
###########################################################
## 2. Fraser River Chinook salmon abundance index model
## dat = albion dest fishery data, available at 
#https://www.pac.dfo-mpo.gc.ca/fm-gp/fraser/docs/commercial/albionchinook-quinna-eng.html
###########################################################
dat<-dat[dat$year>1993,]
dat$effort<-as.numeric(dat$effort)
dat$year2<-as.factor(dat$year)
dat$calDay<-as.numeric(dat$calDay)
dat$catch<-as.numeric(dat$catch)
dat$cpue1<-dat$cpue+.001
dat$logcpue<-log(dat$cpue1)
m <- brm(logcpue~ s(calDay) + (calDay|year2),
         data=dat, chains = 2,
         iter = 6000, warmup = 1000, thin = 10,
         control = list(adapt_delta = 0.99, max_treedepth=15))
save(m, file="analyses/output/albionchibrms.Rda")
###########################################################}
