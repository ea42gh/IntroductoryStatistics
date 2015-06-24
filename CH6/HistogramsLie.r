x <- c(1.03, 1.24, 1.47, 1.52, 1.92, 1.93, 1.94, 1.95, 1.96, 1.97, 1.98, 
         1.99, 2.72, 2.75, 2.78, 2.81, 2.84, 2.87, 2.9, 2.93, 2.96, 2.99, 3.6, 
           3.64, 3.66, 3.72, 3.77, 3.88, 3.91, 4.14, 4.54, 4.77, 4.81, 5.62)

x11()
par(mfrow=c(2,2))
hist(x,breaks=seq(0.3,6.7,by=0.8),xlim=c(0,6.7),col="green3",freq=FALSE,xlab='x,binwidth=1')
rug(jitter(x))

hist(x,breaks=0:8,col="aquamarine",freq=FALSE,xlab='x,binwidth=0.8')
rug(jitter(x))

qqnorm(x)
qqline(x,col="red")

plot(ecdf(x), main="estimated cdf")
