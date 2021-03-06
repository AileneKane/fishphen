#Script to make schematic figure of hypotheses tested with the SRKW work
#started October 8, 2019
#Create phenology curve for SRKWs:
meannum<-30
phenshift<-10
numshift<--5
doy<-seq(1,365,by=1)
whaledays<-dnorm(doy,mean=200,sd=50)
whaledays_shift<-dnorm(doy,mean=200,sd=80)

doy_shift<-doy+40
quartz(height=5,width=8)
par(mfrow=c(1,2))
plot(doy,whaledays, xlab= "day of year", xlim=c(1,365),yaxt="n", type="l", col = "royalblue",lty=2, lwd=2, cex.axis=1.5, cex.lab=1.5, bty="l")

lines(doy_shift,whaledays,lty=1, lwd=2,col="darkblue")
lines(doy,whaledays_shift,lty=1, lwd=2,col="darkblue")

plot(doy,doy, xlab= "salmon arrival day", ylab= "SRKW arrival day", type="l", lty=1, lwd=2, cex.axis=1.5, cex.lab=1.5, bty="l")

x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}
