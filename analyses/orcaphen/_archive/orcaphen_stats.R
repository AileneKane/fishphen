library(qlcMatrix)
setwd("C:/Users/ailene.ettinger/Documents/GitHub/fishphen")
#get some stats from occ mods for paper
occprobsj<-read.csv("analyses/output/J_2uss_doy92-303_1978-2017meanoccprob_all.csv", header=TRUE)
occprobsj<-occprobsj[,-1]
detprobsj<-read.csv("analyses/output/J_2uss_doy92-303_1978-2017detprob.csv", header=TRUE)
yearmeans<-rowMeans(occprobsj)
years<-seq(1978,2017)
yearpeaks<-apply(occprobsj,1,max)


summary(lm(yearmeans~years))
summary(lm(yearpeaks[24:40]~years[24:40]))
yearmeans[24]-yearmeans[40]/yearmeans[24]
head(occprobsj)
head(detprobsj)
tail(occprobsj)
