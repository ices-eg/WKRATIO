#adopted Liz code to H1 and polish data

RDBshare<-list(SS=SS,SA=SA,FM=FM,BV=BV,FT=FT,VS=VS)
RDBshare$BV$BVnumSamp <- as.numeric(RDBshare$BV$BVnumSamp)
RDBshare$BV$BVnumTotal <- as.numeric(RDBshare$BV$BVnumTotal)
###KK everywhere i have npah or npqs i enter 1 in numtotal and num sample
RDBshare$BV$BVnumTotal<-ifelse(RDBshare$BV$BVselectMeth=="NPQS",1,RDBshare$BV$BVnumTotal)
RDBshare$BV$BVnumSamp<-ifelse(RDBshare$BV$BVselectMeth=="NPQS",1,RDBshare$BV$BVnumSamp)
RDBshare$SS$SSnumTotal<-ifelse(RDBshare$SS$SSselectMeth=="CENSUS",1,RDBshare$SS$SSnumTotal)
RDBshare$SS$SSnumSamp<-ifelse(RDBshare$SS$SSselectMeth=="CENSUS",1,RDBshare$SS$SSnumSamp)
RDBshare$FT$FTnumTotal<-ifelse(RDBshare$FT$FTselectMeth=="NPAH",1,RDBshare$FT$FTnumTotal)
RDBshare$FT$FTnumSamp<-ifelse(RDBshare$FT$FTselectMeth=="NPAH",1,RDBshare$FT$FTnumSamp)
RDBshare$VS$FTnumTotal<-ifelse(RDBshare$VS$VSselectMeth=="NPEJ",1,RDBshare$VS$VSnumTotal)
RDBshare$VS$FTnumSamp<-ifelse(RDBshare$VS$VSselectMeth=="NPEJ",1,RDBshare$VS$VSnumSamp)
# calculate inclusion probabilities assuming SRS within strata
RDBshare$BV$BVincProb <- RDBshare$BV$BVnumSamp/RDBshare$BV$BVnumTotal 
RDBshare$SA$SAincProb <- RDBshare$SA$SAnumSamp/RDBshare$SA$SAnumTotal 
RDBshare$SS$SSincProb <- RDBshare$SS$SSnumSamp/RDBshare$SS$SSnumTotal 
RDBshare$FT$FTincProb <- RDBshare$FT$FTnumSamp/RDBshare$FT$FTnumTotal 
RDBshare$VS$VSincProb <- RDBshare$VS$VSnumSamp/RDBshare$VS$VSnumTotal 
# caclulate quarter from date, and then make a domain of quarter combined with area
# allocate domains to FM & BV as well
RDBshare$SA$FTid <- RDBshare$SS$FTid[match(RDBshare$SA$SSid,RDBshare$SS$SSid)]
RDBshare$SA$date <- RDBshare$FT$FTdepDate[match(RDBshare$SA$FTid,RDBshare$FT$FTid)]
RDBshare$SA$quarter <- paste("Q",(as.numeric(substr(RDBshare$SA$date,6,7))-1) %/% 3 + 1,sep="")

#area is optional i haven't got it on my data
 RDBshare$SA$domain <- paste(RDBshare$SA$quarter,RDBshare$SA$SAarea)
RDBshare$SA$domain <- paste(RDBshare$SA$quarter)

RDBshare$FM$domain <- RDBshare$SA$domain[match(RDBshare$FM$SAid,RDBshare$SA$SAid)]
RDBshare$BV$domain <- RDBshare$FM$domain[match(RDBshare$BV$FMid,RDBshare$FM$FMid)]
 # RDBshare$SS$domain <- RDBshare$SA$domain[match(RDBshare$SS$SAid,RDBshare$SA$SAid)]
  # RDBshare$FT$domain <- RDBshare$SS$domain[match(RDBshare$SS$FTid,RDBshare$FT$FTid)]
 # RDBshare$VS$domain <- RDBshare$FT$domain[match(RDBshare$FT$FTid,RDBshare$VS$FTid)]

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

# set up the population totals ##add information from CL table !!! now is the test population data
popDat <- as.data.frame(matrix(c(16,39,86,146,393,6070,39376,7070),byrow=F,nrow=4,ncol=2))
names(popDat) <- c("tripNum","liveWt")
rownames(popDat) <-  c("Q1","Q2","Q3","Q4")

# first do Number-at-Length
stages <- list("FT","SS","SA","FM")
test <- getIncProb(RDBshare,stages)

estL <- tapply(test$FM$FMnumAtUnit/test$FM$incProb,list(test$FM$FMclass,test$FM$domain),sum)
estX <- tapply(test$SA$SAtotalWtLive/test$SA$incProbPrev,list(test$SA$domain),sum)/1e3
popX <- popDat[match(names(estX),rownames(popDat)),"liveWt"]

# calculate the ratio estimates for numbers at length

 estLR <- estL*matrix(rep(popX/estX,dim(estL)[1]),byrow=T,ncol=dim(estL)[2])
# #plus BV number at age
 stages <- list("FT","SS","SA","FM","BV")
 test <- getIncProb(RDBshare,stages)
# calculate a Horvitz Thompson estimate for total numbers at length by domain
# assuming srs within the domain - this is valid for the RDBshare data
 
 estA <- tapply(1/test$BV$incProb,list(test$BV$BVvalue,test$BV$domain),sum)
# calculate a HT estimate for total landed weight using the sampled landed weights
# assuming srs etc as before (This is the same as the calculation above)
 estX <- tapply(test$SA$SAtotalWtLive/test$SA$incProbPrev,list(test$SA$domain),sum)/1e3
 #calculate the ratio estimates for numbers at age
 estAR <- estA*matrix(rep(popX/estX,dim(estA)[1]),byrow=T,ncol=dim(estA)[2])
# 
# compare this to the naive calculation used for data calls
# test <- RDBshare
# test$BV$SAid <- test$FM$SAid[match(test$BV$FMid,test$FM$FMid)]
# test$BV$incProb <- test$BV$BVincProb*test$SA$SAincProb[match(test$BV$SAid,test$SA$SAid)]
# testA <- tapply(1/test$BV$incProb,list(test$BV$BVvalue,test$BV$domain),sum)
# sumX <- tapply(RDBshare$SA$SAtotalWtLive,list(RDBshare$SA$domain),sum)/1e3
# testAR <- testA*matrix(rep(popX/sumX,dim(estA)[1]),byrow=T,ncol=dim(estA)[2])
# 

#plus VS (Hierachy1)
stages <- list("VS","FT","FO","SS","SA","FM")
test <- getIncProb(RDBshare,stages)
estA <- tapply(test$FM$FMnumAtUnit/test$FM$incProb,list(test$FM$domain),sum)
estX <- tapply(test$SA$SAtotalWtLive/test$SA$incProbPrev,list(test$SA$domain),sum)/1e3
popX <- popDat[match(names(estX),rownames(popDat)),"liveWt"]
estVS <- estA*matrix(rep(popX/estX,dim(estA)[1]),byrow=T,ncol=dim(estA)[2])
