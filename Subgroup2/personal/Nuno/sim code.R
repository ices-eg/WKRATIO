#source("N:/work/WKRATIO/Rcode/sim code.R")
N <- 1000 # population size
n <- 20	# sample size
nBoot <- 1e3
set.seed(378572)
x0<-runif(N)
x <- round(100*x0,2) # creates x
a <- 5 # slope
b <- 0 # intercept
m <- 0 # mean error
v <- 40 # sd error
e <- rnorm(N,m,v) # error

x1 <- factor(round(x/10)*10)


# define correlation model
	# linear
		y2 <- a*x + b + e
		
		par(mfcol=c(3,4))
		plot.new()
		plot(x,y2)
		
		boxplot(y2~x1)
	# curved [same error structure]
		library(bezier)
		
		# slight concave
		t <- x0
		p <- matrix(c(0,0,2,4,5,5), ncol=2, byrow=T)

		y1<-bezier(t=t, p=p[, 1:2])
		y2<-(a*x + b)*y1[,2]/y1[,1]+e
		plot(bezier(t=t, p=p[, 1:2]))
		plot(x, y2)
		boxplot(y2~x1)
		# slight convex
		p <- matrix(c(0,0,3,1,5,5), ncol=2, byrow=T)
		y1<-bezier(t=t, p=p[, 1:2])
		y2<-(a*x + b)*y1[,2]/y1[,1]+e
		plot(bezier(t=t, p=p[, 1:2]))
		plot(x, y2)
		boxplot(y2~x1)
		# highly concave
		p <- matrix(c(0,0,2,4.5,3,2), ncol=2, byrow=T)
		y1<-bezier(t=t, p=p[, 1:2])
		y2<-(a*x + b)*y1[,2]/y1[,1]+e
		plot(bezier(t=t, p=p[, 1:2]))
		plot(x, y2)
		boxplot(y2~x1)

y <- y2

plot(x,y)

dat <- data.frame(x,y,e)

Y <- sum(y)
X <- sum(x)

print(Y)

print(cor(x,y))

xHat <- yHatHT <- yHatRatio <- numeric(nBoot)

for (i in 1:nBoot) {

  samp <- dat[sample(1:N,n),]
	
	
  if (i==1) points(samp$x,samp$y,col=2,pch=19)

  xHat[i] <- sum(samp$x)
  yHatHT[i] <- N/n*sum(samp$y)

  yHatRatio[i] <- X*sum(samp$y)/sum(samp$x)

}
rseHT <- sd(yHatHT)/mean(yHatHT)
rseRatio <- sd(yHatRatio)/mean(yHatRatio)

cat("\n")
print("HT")
print(mean(yHatHT))
print("HT bias (kind of)")
print(round(mean(yHatHT)/Y,3))
sdMeanHT <- sd(yHatHT)/sqrt(nBoot)
print("HT bias (CI)")
print(round((mean(yHatHT)+c(-2,2)*sdMeanHT)/Y,3))
print("HT RSE")
print(round(100*rseHT,2))
print("HT CIs")
print(quantile(yHatHT,c(0.025,0.975)))
cat("\n")
print("Ratio")
print(mean(yHatRatio))
print("Ratio bias (kind of)")
print(round(mean(yHatRatio)/Y,2))
print("Ratio bias (CI)")
sdMeanRatio <- sd(yHatRatio)/sqrt(nBoot)
print(round((mean(yHatRatio)+c(-2,2)*sdMeanRatio)/Y,3))
print("Ratio RSE")
print(round(100*rseRatio,3))
print("Ratio CIs")
print(quantile(yHatRatio,c(0.025,0.975)))

yLim <- range(c(yHatHT,yHatRatio))
# plot(xHat,yHatHT,ylim=yLim)
# abline(h=Y,col="grey")
# points(xHat,yHatRatio,col=2)


plot(yHatHT,yHatRatio,xlim=yLim,ylim=yLim)
abline(0,1,col="grey")

boxplot(yHatHT,yHatRatio,names=c("yHatHT","yHatRatio"))
abline(h=Y,col="grey")
points(x=1, y=mean(yHatHT), col=2, pch=4)
points(x=2, y=mean(yHatRatio), col=2, pch=4)

