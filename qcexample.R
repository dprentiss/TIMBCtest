set.seed(3)
b <- rlnorm(200, 3, 0.5)
hist(b, 50)
a <- b
a[which(b <= 15)] = 5
p1 <- hist(a, plot=F)     
p2 <- hist(b, plot=F)
#plot( p2, col=rgb(0,0,1,1/2), xlim=c(0,80), main="Test")  # first histogram
#plot( p1, col=rgb(1,0,0,1/2), xlim=c(0 ,80), add=T)  # second
par(mfrow=c(1,2))
plot( p2, ylim=c(0,80), main="Before driver removal law", xlab="Duration", ylab="Number of incidents")  # first histogram
plot( p1, ylim=c(0,80), main="After driver removal law", xlab="Duration", ylab="Number of incidents")  # second
plot( p2, ylim=c(0,80), lty="dashed", add=T)  # first histogram
