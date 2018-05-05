install.packages("fpp")
library(fpp)
plot(melsyd[,"Economy.Class"],main="Economy class passengers: Melbourne-Sydney",xlab="Year",ylab="Thousands")
plot(a10, ylab="$ million", xlab="Year", main="Antidiabetic drug sales")
summary(melsyd)

dj2 <- window(dj,end=250)
plot(dj2,main="Dow Jones Index (daily ending 15 Jul 94)",
     ylab="",xlab="Day",xlim=c(2,290))
lines(meanf(dj2,h=42)$mean,col=4)
lines(rwf(dj2,h=42)$mean,col=2)
lines(rwf(dj2,drift=TRUE,h=42)$mean,col=3)
legend("topleft",lty=1,col=c(4,2,3),
       legend=c("Mean method","Naive method","Drift method"))
