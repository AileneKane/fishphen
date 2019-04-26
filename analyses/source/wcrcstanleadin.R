#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Set working directory: 
setwd("~/Documents/GitHub/fishphen")
#or from laptop:
#setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

# Load libraries

# libraries needed for the leadin code
#library(plyr)
#library(dplyr)
library(rstan)
library(mgcv)

source("analyses/source/savestan.R") # Dan Flynn code
source("analyses/source/stan_utility.R") # From Mike Betancourt

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## First steps to cleaning: Get the data, subset down to exact data columns that we want 
## Be sure to keep an eye on this part of the code and the files it sources, they will need updating!

## (1) Get the data and slim down to correct response and no NAs ...

d<-read.csv("analyses/output/wacrdat_1984_1993.csv", header=TRUE)#for now just use the old data

source("analyses/source/wcrcdataplease.R")
dim(fishsum)

##################################
## Prep the data for Stan model ##
##################################

# making some list out of the processed data. It will be input for the model ...

#  source("source/bb_zscorepreds.R")
    datalist.chin <- with(fishsum, 
                    list(y = chin, 
                         week = week, 
                         effort = anglers,
                         N = nrow(fishsum)
                         )
                    )
    datalist.coho <- with(fishsum, 
                      list(y = coho, 
                           week = week, 
                           effort = anglers,
                           N = nrow(fishsum)
                          )
                    )

#Now you're ready to fit the model in stan!
## AT THE END ...
str(datalist.chin)
#print("Unique forcing types are ...")


#First try fitting the model as a gam
for(y in 1987:1993) {
  if(y==1988){next}
  yr<-fishsum[fishsum$year==y,]
  w=as.integer(yr$week)
  c = yr$chin
  effort = yr$anglers
  g = gam(log(c+1) ~ s(w) + offset(log(effort)))
  plot(w,c, pch=21,bg="gray",xlab = "Week",
       ylab = "Expected recreational catch", main = paste("Year: ",y))
  lines(w,exp(g$fitted.values),lwd=3)
}
