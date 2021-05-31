# uses RDBshare data and assumes simple random sampling within stages
# trips within domains (quarter & area) and ages within lengths.
# Note that RDBshare is not actually set up in an accepted RDBES hierarchy. 
# It is similar to H2, but with LE instead of FT
# In the code below, the hierarchy is as follows in order: "LE","SS","SA","FM","BV"
# There is no variance estiamtion yet. :( 

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

# a function to calculate inclusion probabilities for the units at the final stage
# of sampling given all the other stages
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

# first do Number-at-Length
stages <- list("LE","SS","SA","FM")
RDBshare <- getIncProb(RDBshare,stages)
estL <- tapply(RDBshare$FM$FMnumAtUnit/RDBshare$FM$incProb,list(test$FM$FMclass,test$FM$domain),sum)
estX <- tapply(RDBshare$SA$SAtotalWtLive/RDBshare$SA$incProbPrev,list(RDBshare$SA$domain),sum)/1e3
popX <- popDat[match(names(estX),rownames(popDat)),"liveWt"]
estLR <- estL*matrix(rep(popX/estX,dim(estL)[1]),byrow=T,ncol=dim(estL)[2])

stages <- list("LE","SS","SA","FM","BV")
RDBshare <- getIncProb(RDBshare,stages)
estA <- tapply(1/test$BV$incProb,list(test$BV$BVvalue,test$BV$domain),sum)
estX <- tapply(RDBshare$SA$SAtotalWtLive/RDBshare$SA$incProbPrev,list(RDBshare$SA$domain),sum)/1e3
estAR <- estA*matrix(rep(popX/estX,dim(estA)[1]),byrow=T,ncol=dim(estA)[2])

test <- RDBshare
test$BV$SAid <- test$FM$SAid[match(test$BV$FMid,test$FM$FMid)]
test$BV$incProb <- test$BV$BVincProb*test$SA$SAincProb[match(test$BV$SAid,test$SA$SAid)]
testA <- tapply(1/test$BV$incProb,list(test$BV$BVvalue,test$BV$domain),sum)
sumX <- tapply(RDBshare$SA$SAtotalWtLive,list(RDBshare$SA$domain),sum)/1e3
testAR <- testA*matrix(rep(popX/sumX,dim(estA)[1]),byrow=T,ncol=dim(estA)[2])









