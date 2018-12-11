##################################################################
# Start to look at orca movement,
# Code based off Kery's movement examples
# Ailene Ettinger, ailene.ettinger@noaa.gov
# (modifed from code of  Mark Kery, marc.kery@vogelwarte.ch
# Start Date:December 11, 2018
# Title:	orca_movement_explore
##################################################################
#-----------------------------------------------------------------
# Data exploration code
#-----------------------------------------------------------------

#housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/GitHub/fishphen")

# Load libraries

# 1. Get the orcamaster data
d <- read.csv("data/AppendixII.csv")

# Select columns that you want and clean the orcamaster data
source("analyses/orca_dataprep_movement.R")
#for starters, run on most recent 7 years only:
j2017<-j[j$Year==2017,]
k2017<-k[k$Year==2017,]
#str(j)
#head(j)  
#Make plot of movement of j pod in one year
# Static plot

# Grab x and y coordinates
x <- j2017$Lat.cl
y <- j2017$Long.cl
x.k <- k2017$Lat.cl
y.k <- k2017$Long.cl

quartz()
plot(x, y, type = "b", col = "red", pch = 16, main = "Start is at black letter", ylab = "Latitude", xlab = "Longitude",ylim = rev(range(y)),xlim = rev(range(x)))
n <- length(x)
points(x.k,y.k, type = "b",col="blue",pch = 16,)
points(x[1], y[1], col = "black", pch = "J", cex = 2)
points(x.k[1], y.k[1], col = "black", pch = "K", cex = 2)

# Dynamic plot (browse through)
range.x <- range(x)
range.y <- range(y)
for(t in 1:n){
  label <- paste("Day", t) 
  plot(x[1:t], y[1:t], type = "b", col = "red", pch = 16,
       xlim = range.x, ylim = range.y, main = label, 	
       xlab = "Longitude", ylab = "Latitude")
  points(x[1], y[1], col = "black", pch = "+", cex = 2)
  browser()
}

