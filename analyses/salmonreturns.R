#Salmon return timing phenology using WDFW data
#Started by Ailene on March 12, 2019

#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Set working directory: 
setwd("~/Documents/GitHub/fishphen")
#or from laptop:
#setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

# Load libraries

# 1. Get the data
d <- read.csv("data/TrapEstimate&SpawnCHCKCO.csv")
head(d)
unique(d$X.4)

