#Code to analyse the fraser river test fishery data to get estimates of first, last, median dates of spring chinook runs
#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Set working directory: 
setwd("~/Documents/GitHub/fishphen")
#or from laptop:
#setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

# Load libraries
library(dplyr)
# 1. Read in the datafiles
source("analyses/salmonreturns/source/read_albiondat.R")
dim(d)#6148
unique(d$calDay)
