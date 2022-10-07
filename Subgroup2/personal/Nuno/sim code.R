rm(list=ls())
library(sampling)

#source("N:/work/WKRATIO/Rcode/sim code.R")
N <- 1000 # population size
n <- 40	# sample size
nBoot <- 1e3
seed = 378572; set.seed(seed)
x0<-runif(N)
x <- round(1500*x0,2) # creates x
a <- 5 # slope
b <- 0 # intercept
root_name = "figure_33"
m <- 0 # mean error
v <- 40 # sd error
#factor.e <- 1
factor.e <- 40*x/max(x)
e <- factor.e*rnorm(N,m,v) # error
model = "linear" # slight_concave, slight_convex, strong_convex, linear
sampMethod = "UPWOR" # SRSWOR, UPWOR
pps.x <- x
if (sampMethod == "UPWOR") pps.x <- x+rnorm(length(x),0,20)




settings = list(file = paste(root_name,".png",sep=""),
				settings = list (N = N,
								n = n,
								nBoot = nBoot,
								seed = seed,
								x0 = x0,
								x = x,
								a = a,
								b = b,
								m = m,
								v = v,
								factor.e = factor.e,
								e = e,
								model = model,
								sampMethod = sampMethod,
								pps.x = pps.x,						
								R.version = R.version))
								
				
					
if (1==1)
{#abline(coef(lm(dat$y~dat$x)))

x1 <- factor(round(x/10)*10)

# define correlation model
	# linear
		y2 <- a*x + b + e
		
		#par(mfcol=c(3,4))
		#plot.new()
		#plot(x,y2); abline(0,1)
		#boxplot(y2~x1)
		

if (!model == "linear")
{

	# curved [same error structure]
		library(bezier)
		t <- x0
		p <- matrix(c(0,0,2,4,5,5), ncol=2, byrow=T)
	
	if (model == "slight_convex"){
		# slight_concave

		y1<-bezier(t=t, p=p[, 1:2])
		y2<-(a*x + b)*y1[,2]/y1[,1]+e
		plot(bezier(t=t, p=p[, 1:2]))
		plot(x, y2)
		#boxplot(y2~x1)
	}


	if (model == "slight_concave"){		
		# slight convex
		p <- matrix(c(0,0,3,1,5,5), ncol=2, byrow=T)
		y1<-bezier(t=t, p=p[, 1:2])
		y2<-(a*x + b)*y1[,2]/y1[,1]+e
		plot(bezier(t=t, p=p[, 1:2]))
		plot(x, y2)
		#boxplot(y2~x1)
	}


	if (model == "strong_convex"){			
		# strong_convex
		p <- matrix(c(0,0,2,4.5,3,2), ncol=2, byrow=T)
		y1<-bezier(t=t, p=p[, 1:2])
		y2<-(a*x + b)*y1[,2]/y1[,1]+e
		plot(bezier(t=t, p=p[, 1:2]))
		plot(x, y2)
		#boxplot(y2~x1)
	}	
}

y <- y2

	# probability of inclusion in sample of size n
		# equal probability without replacement
			if (sampMethod == "SRSWOR") pii <- rep(n/N, N)
		# unequal probability without replacement (proportional to x)
			if (sampMethod == "UPWOR") 
				{
				phii<-inclusionprobabilities(pps.x,1) # selection probability under unequal probability sampling pps - note that n = 1 makes selection = inclusion [Lohr 243]
				pii <- phii*n # inclusion probability assuming with replacement [Lohr 243]
				}
	# weights	
		wi <- 1/pii


windows(15,10)
par(mfrow=c(1,2), oma=c(2,1,1,1))

dat <- data.frame(x,y,e, pii, wi)

plot(dat$x,dat$y, xlab="x", ylab="y", main="Population and 1st sample in bootstrap")

Y <- sum(y)
X <- sum(x)

print(Y)

print(cor(x,y))

cvy <- cvx <- corrHat <- xHat <- yHatSRSWOR <- yHatRatio <- numeric(nBoot)

yHatHT1<- yHatRatio1<- numeric(nBoot)
yHatHT2<- yHatRatio2<- numeric(nBoot)
yHatRatio3 <- numeric(nBoot)
yHatRegressHT1 <- numeric(nBoot)



for (i in 1:nBoot) {

	if (sampMethod == "SRSWOR")	
		{
		# via sampling::srswor
		s = srswor(n, N)
		samp <- dat[s,]
		# via base::sample
		samp <- dat[sample(1:N,n, replace = FALSE),]
		}
		
	if (sampMethod == "UPWOR")	
		{
		# via sampling::UPsystematic
		s = UPsystematic(dat$pii)
		samp <- dat[s==1,]
		}
	
	if (i==1) points(samp$x,samp$y,col=2,pch=19)

	xHat[i] <- sum(samp$x)
  

	# yHatSRSWOR
		yHatSRSWOR[i] <- N/n*sum(samp$y)
		yHatRatio[i] <- X*sum(samp$y)/sum(samp$x)
	# sampling::samplingHT
		yHatHT1[i]<-HTestimator(samp$y, samp$pii)
		#varest(Ys = samp$y, pik = samp$pii) # deville's method
		#varHT(samp$y,samp$pii,method=1) # does not run, requires matrix
		yHatRatio1[i] <- X*HTestimator(samp$y, samp$pii)/HTestimator(samp$x, samp$pii)
	# samplingVarEst::Est.Total.NHT
		#yHatHT2[i] <- Est.Total.NHT(VecY.s = samp$y, VecPk.s = samp$pii)
		#yHatRatio2[i] <- Est.Ratio(VecY.s = samp$y, VecX.s = samp$x, VecPk.s = samp$pii)*X
	# generalized ratio HT estimator [Mary TC 6)]
		#yHatRatio3[i] <- N*sum(samp$y/samp$pii)/sum(1/samp$pii)
	# general regression HT estimator [GREG] [Mary TC 6)]
		miu.y <- sum(samp$y/samp$pii)/sum(1/samp$pii)
		miu.x <- sum(samp$x/samp$pii)/sum(1/samp$pii)
		ratio <- sum((samp$x-miu.x)*(samp$y-miu.y)/samp$pii)/sum((samp$x-miu.x)^2/samp$pii)
		yHatRegressHT1[i] <- N* (miu.y + ratio*(mean(x)-miu.x))
	
	# Lohr via weights
		#sum(samp$wi*samp$y)


  corrHat[i] <- cor(samp$x,samp$y)
  
  cvx[i]<-sqrt(var(samp$x)/n)/mean(samp$x)
  cvy[i]<-sqrt(var(samp$y)/n)/mean(samp$y)
	

}
yLim <- range(c(yHatSRSWOR,yHatRatio,yHatHT1,yHatRatio1,yHatRegressHT1))
#boxplot(yHatSRSWOR, yHatRatio, yHatHT1,yHatHT2,yHatRatio1,yHatRatio2,yHatRatio3,yHatRegressHT1, names=c("yHatSRSWOR","yHatRatio", "yHatHT1","yHatHT2","yHatRatio1","yHatRatio2","yHatRatio3","yHatRegHT1"), ylim=c(0, max(yLim)), las=2, cex.axis=0.8)
boxplot(yHatSRSWOR, yHatRatio, yHatHT1,yHatRatio1,yHatRegressHT1, names=c("yHatSRSWOR","yHatRatioSRSWOR", "yHatHT","yHatRatioHT","yHatRegrHT"), ylim=c(min(yLim), max(yLim)), las=2, cex.axis=0.8, main="yHat Estimators")
abline(h=Y,col="gray")
points(x=1, y=yHatSRSWOR[1], col=2, pch=19)
points(x=2, y=yHatRatio[1], col=2, pch=19)
points(x=3, y=yHatHT1[1], col=2, pch=19)
points(x=4, y=yHatRatio1[1], col=2, pch=19)
points(x=5, y=yHatRegressHT1[1], col=2, pch=19)
points(x=1, y=mean(yHatSRSWOR), col=1, pch=4)
points(x=2, y=mean(yHatRatio), col=1, pch=4)
points(x=3, y=mean(yHatHT1), col=1, pch=4)
points(x=4, y=mean(yHatRatio1), col=1, pch=4)
points(x=5, y=mean(yHatRegressHT1), col=1, pch=4)

savePlot(filename=paste(root_name,".png", sep=""), type="png")
save (settings, file = paste(root_name,"_settings.RData", sep=""))
}

graphics.off()


#

# ========================================
# ========================================

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



# plot(yHatHT,yHatRatio,xlim=yLim,ylim=yLim)
# abline(0,1,col="grey")

boxplot(yHatHT,yHatRatio,names=c("yHatHT","yHatRatio"), ylim=c(0, max(yLim)))
abline(h=Y,col="grey")
points(x=1, y=mean(yHatHT), col=2, pch=4)
points(x=2, y=mean(yHatRatio), col=2, pch=4)
boxplot(corrHat, ylim=c(0,1), xlab = "corrSamples")
abline(h=cor(x,y),col="grey")
points(x=1, y=mean(corrHat), col=2, pch=4)


library(SDaA)
