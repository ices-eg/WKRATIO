## simulate stratified sampling with strata s1 and s2, SRSWOR
## 2 domains with size Nd are simulated
## estimate per strata or per domain, using HT estimator
## estimate per domain using ratio-HT estimator
## link ratio-HT estimator to the more generic regression estimator

## define 2 strata population
N1 <- 50
N2 <- 100
p1 <- data.frame(y=rnorm(N1, 5,3), strata="s1")
p2 <- data.frame(y=rnorm(N2, 10, 3), strata="s2")

p <- rbind(p1, p2)

## define domain 1
Nd <- 50
pd_ind <- sample(1:nrow(p), Nd, replace = F)
p$domain <- NA
p$domain[pd_ind] <- "d1"

## define domain 2, independent of domain 1
Nd <- 60
pd_ind <- sample(1:nrow(p), Nd, replace = F)
p$domain2 <- NA
p$domain2[pd_ind] <- "d2"

## auxilary information
## x1: correlated with y
## x2: independent with y
p$x1 <- 3.5*(p$y) + rnorm(nrow(p), 0,sd(p$y)/2) # linear correlation through origin
#p$x1 <- 3.5*(p$y) + rnorm(nrow(p), 10,sd(p$y)/2) # linear correlation with intercept
p$x2 <- rnorm(nrow(p), 20,5)
#p$x2 <- rgamma(nrow(p), 2,2)
plot(p$x1,p$y)
plot(p$x2,p$y)
lm(p$y ~ p$x1)

## ground truth
sum(p$y[p$strata=="s1"])
sum(p$y[p$strata=="s2"])
sum(p$y[p$domain=="d1" & !is.na(p$domain)])
sum(p$y[p$domain2=="d2" & !is.na(p$domain2)])

N_sim <- 1000
res <- data.frame(est_p1=rep(NA, N_sim),
                  est_p2=rep(NA, N_sim),
                  est_pd=rep(NA, N_sim),
                  est_pd2=rep(NA, N_sim),
                  est_dr11_x1=rep(NA, N_sim),
                  est_dr12_x1=rep(NA, N_sim),
                  est_dr13_x1=rep(NA, N_sim),
                  est_dr11_x2=rep(NA, N_sim),
                  est_dr12_x2=rep(NA, N_sim),
                  est_dr13_x2=rep(NA, N_sim))

for (i in 1:N_sim) {
  
  ## sampling based on strata (SRSWOR)
  n1 <- round(N1*0.1)  ## sample size defined by sampling fraction
  s1_ind <- sample(1:N1, n1, replace = FALSE)
  s1 <- data.frame(y=p$y[p$strata == "s1"][s1_ind],
                   p_i=n1/N1,
                   domain=p$domain[p$strata == "s1"][s1_ind],
                   domain2=p$domain2[p$strata == "s1"][s1_ind],
                   x1=p$x1[p$strata == "s1"][s1_ind],
                   x2=p$x2[p$strata == "s1"][s1_ind])
  
  

  n2 <- round(N2*0.05)
  s2_ind <- sample(1:N2, n2, replace = FALSE)
  s2 <- data.frame(y=p$y[p$strata == "s2"][s2_ind],
                   p_i=n2/N2,
                   domain=p$domain[p$strata == "s2"][s2_ind],
                   domain2=p$domain2[p$strata == "s2"][s2_ind],
                   x1=p$x1[p$strata == "s2"][s2_ind],
                   x2=p$x2[p$strata == "s2"][s2_ind])
  
  s <- rbind(s1, s2)
  

  ## estimate total per strata (HT)
  res$est_p1[i] <- sum(s1$y/s1$p_i)
  res$est_p2[i] <- sum(s2$y/s2$p_i)
  
  ## estimate domain 1 (HT)
  sd <- s[s$domain == "d1" & !is.na(s$domain),]
  res$est_pd[i] <- sum(sd$y/sd$p_i)
  
  ## estimate domain 2 (HT)
  sd <- s[s$domain2 == "d2" & !is.na(s$domain2),]
  res$est_pd2[i] <- sum(sd$y/sd$p_i)
  
  ## estimate domain 1 (ratio using x1 + HT), domain size unknown
  sd        <- s[s$domain == "d1" & !is.na(s$domain),]
  est_Bd    <- sum(sd$y/sd$p_i)/sum(sd$x1/sd$p_i)
  sd$resid  <- sd$y-est_Bd*sd$x1
  ## formula 1
  res$est_dr11_x1[i]   <- sum(p$x1[p$domain=="d1" & !is.na(p$domain)]*est_Bd) + sum(sd$resid/sd$p_i)
  x1_tot    <- sum(p$x1[p$domain=="d1" & !is.na(p$domain)])
  ## regression estimator
  res$est_dr12_x1[i]   <- sum(sd$y/sd$p_i) + est_Bd*(x1_tot-sum(sd$x1/sd$p_i))
  ## ordinary ratio estimator
  res$est_dr13_x1[i]   <- x1_tot*est_Bd
  #est_dr11 == est_dr12
  #est_dr11 == est_dr13
  
  ## estimate domain 1 (ratio using x2 + HT), domain size unknown
  sd        <- s[s$domain == "d1" & !is.na(s$domain),]
  est_Bd    <- sum(sd$y/sd$p_i)/sum(sd$x2/sd$p_i)
  sd$resid  <- sd$y-est_Bd*sd$x2
  x2_tot    <- sum(p$x2[p$domain=="d1" & !is.na(p$domain)])
  ## formula 1
  res$est_dr11_x2[i]   <- sum(x2_tot*est_Bd) + sum(sd$resid/sd$p_i)
  ## regression estimator: more general estimator when intercept !=0
  ## intercept = est_y_mean-B*x_mean, in this case =0
  res$est_dr12_x2[i]   <- sum(sd$y/sd$p_i) + est_Bd*(x2_tot-sum(sd$x2/sd$p_i))
  ## ordinary ratio estimator
  res$est_dr13_x2[i]   <- x2_tot*est_Bd
  #est_dr11 == est_dr12
  #est_dr11 == est_dr13
  
}


## ground truth
sum(p$y[p$strata=="s1"])
mean(res$est_p1)
sum(p$y[p$strata=="s2"])
mean(res$est_p2)
sum(p$y[p$domain=="d1" & !is.na(p$domain)])
mean(res$est_pd)
sum(p$y[p$domain2=="d2" & !is.na(p$domain2)])
mean(res$est_pd2)
                 


