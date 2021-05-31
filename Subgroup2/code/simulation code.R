#source("N:/work/WKRATIO/Rcode/sim code.R")
N <- 1000
n <- 20
nBoot <- 1e3
set.seed(378572)
x <- round(100*runif(N),2)
a <- 0
b <- 100
m <- 0
v <- 5
e <- rnorm(N,m,v)
y <- a*x + b + e

par(mfrow=c(2,2))
par(mar=c(4,4,1,1))
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
plot(xHat,yHatHT,ylim=yLim)
abline(h=Y,col="grey")
points(xHat,yHatRatio,col=2)


plot(yHatHT,yHatRatio,xlim=yLim,ylim=yLim)
abline(0,1,col="grey")

boxplot(yHatHT,yHatRatio,names=c("yHatHT","yHatRatio"))
abline(h=Y,col="grey")

