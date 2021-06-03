
# uses RDBshare data and assumes simple random sampling within stages 
# trips within domains (quarter & area) and ages within lengths.
# Note that RDBshare is not actually set up in an accepted RDBES hierarchy. 
# It is similar to H2, but with LE instead of FT
# In the code below, the hierarchy is as follows in order: "LE","SS","SA","FM","BV"
# There is no variance estiamtion yet. :( 

# set up the population totals
popDat <- as.data.frame(matrix(c(16,39,86,14647622,39371876,68317070),byrow=F,nrow=3,ncol=2))
names(popDat) <- c("tripNum","liveWt")
rownames(popDat) <-  c("Q1 27.4.a","Q1 27.6.a","Q4 27.4.a")

# calculate inclusion probabilities assuming SRS within strata
RDBshare$BV$BVincProb <- RDBshare$BV$BVnumSamp/RDBshare$BV$BVnumTotal 
RDBshare$SA$SAincProb <- RDBshare$SA$SAnumSamp/RDBshare$SA$SAnumTotal 
RDBshare$SS$SSincProb <- RDBshare$SS$SSnumSamp/RDBshare$SS$SSnumTotal 
RDBshare$LE$LEincProb <- RDBshare$LE$LEnumSamp/RDBshare$LE$LEnumTotal 

# caclulate quarter from date, and then make a domain of quarter combined with area
# allocate domains to FM & BV as well
RDBshare$SA$LEid <- RDBshare$SS$LEid[match(RDBshare$SA$SSid,RDBshare$SS$SSid)]
RDBshare$SA$date <- RDBshare$LE$LEdate[match(RDBshare$SA$LEid,RDBshare$LE$LEid)]
RDBshare$SA$quarter <- paste("Q",(as.numeric(substr(RDBshare$SA$date,6,7))-1) %/% 3 + 1,sep="")
RDBshare$SA$domain <- paste(RDBshare$SA$quarter,RDBshare$SA$SAarea)
RDBshare$FM$domain <- RDBshare$SA$domain[match(RDBshare$FM$SAid,RDBshare$SA$SAid)]
RDBshare$BV$domain <- RDBshare$FM$domain[match(RDBshare$BV$FMid,RDBshare$FM$FMid)]
RDBshare$BV$numAtUnit <- 1

# -----------------------------------------------------------------------
# a function to calculate inclusion probabilities for the units at the final stage
# of sampling given all the inclusion probabilities for the other stages
# -----------------------------------------------------------------------
getIncProb <- function(RDB,stages){
  nStages <- length(stages)
  if (any(stages %in% c("FM"))) {
    RDB[["FM"]][["FMincProb"]] <- 1
  }
  RDB[[stages[[1]]]][["incProb"]] <- RDB[[stages[[1]]]][[paste(stages[[1]],"incProb",sep="")]]
  for (i in 2:(nStages)) {
    indx <- RDB[[stages[[i]]]][[paste(stages[[i-1]],"id",sep="")]]
    indxPrev <- RDB[[stages[[i-1]]]][[paste(stages[[i-1]],"id",sep="")]]
    RDB[[stages[[i]]]][["incProbPrev"]] <- RDB[[stages[[i-1]]]][[paste("incProb",sep="")]][match(indx,indxPrev)]
    RDB[[stages[[i]]]][["incProb"]] <- RDB[[stages[[i]]]][["incProbPrev"]]*RDB[[stages[[i]]]][[paste(stages[[i]],"incProb",sep="")]]
  }
  return(RDB)
}

# -----------------------------------------------------------------------
# Number-at-Length
# -----------------------------------------------------------------------
# set up the stages in the sampling design used in the estimation
stages <- list("LE","SS","SA","FM")

# calculate the inclusion probabilities by length class given the other inclusion probabilities 
# in the higher stages
test <- getIncProb(RDBshare,stages)

# calculate a Horvitz Thompson estimate for total numbers at length by domain
# assuming srs within the domain - this is valid for the RDBshare data
estL <- tapply(test$FM$FMnumAtUnit/test$FM$incProb,list(test$FM$FMclass,test$FM$domain),sum)

# calculate a HT estimate for total landed weight using the sampled landed weights
# assuming srs etc as before
estX <- tapply(test$SA$SAtotalWtLive/test$SA$incProbPrev,list(test$SA$domain),sum)/1e3

# get the relevant population totals
popX <- popDat[match(names(estX),rownames(popDat)),"liveWt"]

# calculate the ratio estimates for numbers at length
estLR <- estL*matrix(rep(popX/estX,dim(estL)[1]),byrow=T,ncol=dim(estL)[2])

# -----------------------------------------------------------------------
# Numbers-at-age
# -----------------------------------------------------------------------

stages <- list("LE","SS","SA","FM","BV")
test <- getIncProb(RDBshare,stages)

# calculate a Horvitz Thompson estimate for total numbers at length by domain
# assuming srs within the domain - this is valid for the RDBshare data
estA <- tapply(1/test$BV$incProb,list(test$BV$BVvalue,test$BV$domain),sum)

# calculate a HT estimate for total landed weight using the sampled landed weights
# assuming srs etc as before (This is the same as the calculation above)
estX <- tapply(test$SA$SAtotalWtLive/test$SA$incProbPrev,list(test$SA$domain),sum)/1e3

# calculate the ratio estimates for numbers at age
estAR <- estA*matrix(rep(popX/estX,dim(estA)[1]),byrow=T,ncol=dim(estA)[2])

# compare this to the naive calculation used for data calls
test <- RDBshare
test$BV$SAid <- test$FM$SAid[match(test$BV$FMid,test$FM$FMid)]
test$BV$incProb <- test$BV$BVincProb*test$SA$SAincProb[match(test$BV$SAid,test$SA$SAid)]
testA <- tapply(1/test$BV$incProb,list(test$BV$BVvalue,test$BV$domain),sum)
sumX <- tapply(RDBshare$SA$SAtotalWtLive,list(RDBshare$SA$domain),sum)/1e3
testAR <- testA*matrix(rep(popX/sumX,dim(estA)[1]),byrow=T,ncol=dim(estA)[2])
