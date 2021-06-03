##################################################################################################################### 
## WKRATIO: Portuguese data testing
## National discard raising procedure using RDBES data format (Hierarchy 3)
## Ana Cláudia Fernandes, 31may - 4jun 
#####################################################################################################################

## No national CE table available - "Logbook effort data" prepared according to RDBES format 
## Biological data: subset for blue whiting (WHB)

# Load our functions
	source("RDBES_Functions.R")

# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
	options(scipen=500) # big number of digits

## Loads data
	
### Effort data prepared	(CE table)
	load('.../Data/effTable.rdata')

### HIERARCHY 3 - Data subset for WHB
	load('.../Data/H3_WHB.rdata')

## data formats

	myExchangeFileH3$FM$FMnumberAtUnit <- as.numeric(as.character(myExchangeFileH3$FM$FMnumberAtUnit))
	myExchangeFileH3$SA$SAnumberSampled <- as.numeric(myExchangeFileH3$SA$SAnumberSampled)
	myExchangeFileH3$SA$SAnumberTotal <- as.numeric(myExchangeFileH3$SA$SAnumberTotal)
	myExchangeFileH3$SA$SAtotalWeightLive <- as.numeric(myExchangeFileH3$SA$SAtotalWeightLive)
	myExchangeFileH3$SA$SAsampleWeightLive <- as.numeric(myExchangeFileH3$SA$SAsampleWeightLive)
	myExchangeFileH3$SA$SAtotalWeightMeasured <- as.numeric(myExchangeFileH3$SA$SAtotalWeightMeasured)
	myExchangeFileH3$SA$SAsampleWeightMeasured <- as.numeric(myExchangeFileH3$SA$SAsampleWeightMeasured)
	myExchangeFileH3$SS$SSnumberSampled <- as.numeric(myExchangeFileH3$SS$SSnumberSampled)
	myExchangeFileH3$SS$SSnumberTotal <- as.numeric(myExchangeFileH3$SS$SSnumberTotal)

	myExchangeFileH3$FO$FOnumberSampled <- 1
	myExchangeFileH3$FO$FOnumberTotal <- 1

	myExchangeFileH3$FT$FTnumberSampled <- as.numeric(myExchangeFileH3$FT$FTnumberSampled)
	myExchangeFileH3$FT$FTnumberTotal <- as.numeric(myExchangeFileH3$FT$FTnumberTotal)

	myExchangeFileH3$VS$VSinclusionProb <- 0.1923077 ## ~ vessel coverage

# ## FROM LIZ CODE IN THE GITHUB ('WKRATIO estimates.R') #################################################################
## calculate inclusion probabilities assuming SRS within strata
 
	myExchangeFileH3$SA$SAinclusionProb <- myExchangeFileH3$SA$SAnumberSampled/myExchangeFileH3$SA$SAnumberTotal 
	myExchangeFileH3$SA[is.na(myExchangeFileH3$SA$SAinclusionProb),]$SAinclusionProb <- 1
	myExchangeFileH3$SS$SSinclusionProb <- myExchangeFileH3$SS$SSnumberSampled/myExchangeFileH3$SS$SSnumberTotal 
	myExchangeFileH3$FO$FOinclusionProb <- myExchangeFileH3$FO$FOnumberSampled/myExchangeFileH3$FO$FOnumberTotal 
	myExchangeFileH3$FT$FTinclusionProb <- myExchangeFileH3$FT$FTnumberSampled/myExchangeFileH3$FT$FTnumberTotal 

# caclulate quarter from date, and then make a domain of quarter combined with area
# allocate domains to FM as well
	myExchangeFileH3$SA$FOid <- myExchangeFileH3$SS$FOid[match(myExchangeFileH3$SA$SSid,myExchangeFileH3$SS$SSid)]
	myExchangeFileH3$SA$date <- myExchangeFileH3$FO$FOendDate[match(myExchangeFileH3$SA$FOid,myExchangeFileH3$FO$FOid)]
	myExchangeFileH3$SA$quarter <- paste("Q",(as.numeric(substr(myExchangeFileH3$SA$date,6,7))-1) %/% 3 + 1,sep="")
	myExchangeFileH3$SA$SAarea <- '27.9.a'
	myExchangeFileH3$SA$domain <- paste(myExchangeFileH3$SA$quarter,myExchangeFileH3$SA$SAarea)
	myExchangeFileH3$FM$domain <- myExchangeFileH3$SA$domain[match(myExchangeFileH3$FM$SAid,myExchangeFileH3$SA$SAid)]
	myExchangeFileH3$BV$domain <- myExchangeFileH3$FM$domain[match(myExchangeFileH3$BV$FMid,myExchangeFileH3$FM$FMid)]

# a function to calculate inclusion probabilities for the units at the final stage
# of sampling given all the other stages
	getIncProb <- function(RDB,stages){
	  #browser()
	  nStages <- length(stages)
	  if (any(stages %in% c("FM"))) {
		RDB[["FM"]][["FMinclusionProb"]] <- 1
	  }
	#browser() 
	 RDB[[stages[[1]]]][["inclusionProb"]] <- RDB[[stages[[1]]]][[paste(stages[[1]],"inclusionProb",sep="")]]
	  for (i in 2:(nStages)) {
	  #browser()
		indx <- RDB[[stages[[i]]]][[paste(stages[[i-1]],"id",sep="")]]
		indxPrev <- RDB[[stages[[i-1]]]][[paste(stages[[i-1]],"id",sep="")]]
		RDB[[stages[[i]]]][["incProbPrev"]] <- RDB[[stages[[i-1]]]][[paste("inclusionProb",sep="")]][match(indx,indxPrev)]
		RDB[[stages[[i]]]][["inclusionProb"]] <- RDB[[stages[[i]]]][["incProbPrev"]]*RDB[[stages[[i]]]][[paste(stages[[i]],"inclusionProb",sep="")]]
	  }
	  return(RDB)
	}

# first do Number-at-Length (H5)
	stages <- list("VS","FT","FO","SS","SA","FM")
	myExchangeFileH3 <- getIncProb(myExchangeFileH3,stages)

###################################################################################################################################

### Discards estimation	
	
## To calculate discards per hour in each trip (DpueTrp)
	trip_info <- myExchangeFileH3$FT %>% transmute(FTid, FTarrivalDate, FTquarter= quarters(as.Date(FTarrivalDate)),FishingDays=as.numeric((as.Date(FTarrivalDate)-as.Date(myExchangeFileH3$FT$FTdepartureDate))+1), FTnumberOfHauls=as.numeric(FTnumberOfHauls), FTnumberTotal=as.numeric(FTnumberTotal), FTnumberSampled=as.numeric(FTnumberSampled), FTinclusionProb, incProbPrev, inclusionProb, VSid)
	trip_info <- droplevels(trip_info[trip_info$FishingDays<4,])
	haul_info <- myExchangeFileH3$FO %>% transmute(FOid, FOendDate, FOduration=as.numeric(FOduration), FOarea, FOmetier5, FOsampled, incProbPrev, inclusionProb, FTid )
	haul_info$FishingDays <- trip_info$FishingDays[match(haul_info$FTid, trip_info$FTid)]

	samp <- myExchangeFileH3$SA[,c('SAid','SAspeciesCode','SAspeciesCodeFAO','SAcatchCategory','SAmetier5','SAtotalWeightLive','SAsampleWeightLive','SAnumberTotal','SAnumberSampled','SAinclusionProb','SAtotalWeightMeasured',
				'SAsampleWeightMeasured', 'quarter', 'incProbPrev', 'inclusionProb', 'SSid', 'FOid')]

	samp$FTid <- haul_info$FTid[match(samp$FOid, haul_info$FOid)]
	samp$SAtotalWeightMeasured <- samp$SAtotalWeightMeasured/1000 # to have weight in Kg
	
	haul_info_dis <- merge(haul_info[haul_info$FOsampled=='Y',], samp[samp$SAcatchCategory=='Dis',c('FOid','SAtotalWeightMeasured')], by='FOid', all.x=T)
	haul_info_dis$FOduration <- round(haul_info_dis$FOduration/60,2) ## duration in hours
	haul_info_dis$FishingDays <- trip_info$FishingDays[match(haul_info_dis$FTid, trip_info$FTid)]
	haul_info_dis$dpue <- round(haul_info_dis$SAtotalWeightMeasured/haul_info_dis$FOduration,2)
	haul_info_dis[is.na(haul_info_dis$dpue),]$dpue <- 0; haul_info_dis[is.na(haul_info_dis$SAtotalWeightMeasured),]$SAtotalWeightMeasured <- 0
	
## Estimation of DPUE at trip level
## DpueTrp, total duration and total discards for sampled hauls
	DpueTrp <- data.frame(haul_info_dis %>% group_by(FOmetier5,FishingDays,FTid) %>% summarise(DpueTrp=sum(SAtotalWeightMeasured)/sum(FOduration), 	
				totDuration=sum(FOduration), totDisc=sum(SAtotalWeightMeasured)))
		
## To raise discards
	totDurationTrp <- data.frame(haul_info_dis %>% group_by(FTid) %>% summarise(totDurationTrp=sum(FOduration)))
	trip_info_dis <- merge(trip_info, DpueTrp[,c('FTid','totDuration','totDisc','DpueTrp')], by='FTid', all.x=T)
	trip_info_dis$totDurationTrp <- totDurationTrp$totDurationTrp[match(trip_info_dis$FTid, totDurationTrp$FTid)]
## Get metier of the trip from FO table (P.S.- in OTB fleet all hauls from a trip have the same metier5...)
	trip_info_dis$FOmetier5 <- haul_info$FOmetier5[match(trip_info_dis$FTid, haul_info$FTid)]

## mean DPUE and variance estimation					
	meanDpueTrp <- tapply(trip_info_dis$totDisc, list(trip_info_dis$FOmetier5, trip_info_dis$FishingDays), sum, na.rm=T)/tapply(trip_info_dis$totDuration, list(trip_info_dis$FOmetier5, trip_info_dis$FishingDays), sum, na.rm=T)

# variance of mean DPUE
	discTrp <- tapply(trip_info_dis$totDisc, list(trip_info_dis$FTid), sum, na.rm=T)
	durTrp <- tapply(trip_info_dis$totDuration, list(trip_info_dis$FTid), sum, na.rm=T)
	nDaysTrp <- tapply(trip_info_dis$FishingDays, list(trip_info_dis$FTid), unique)
	metierTrp <- tapply(trip_info_dis$FOmetier5, list(trip_info_dis$FTid), unique)
	meanDpueTrp2 <-  tapply((discTrp/durTrp)^2, list(metierTrp, nDaysTrp), na.rm=TRUE, sum)
	nTrp <- tapply(trip_info_dis$FTid, list(trip_info_dis$FOmetier5, trip_info_dis$FishingDays), function(x){length(unique(x))})

	varDpueTrp <- (meanDpueTrp2-nTrp*meanDpueTrp^2)/((nTrp-1)*nTrp)	#
	
## Discard estimates for total duration by fishing days: popDiscPerDaysWght
	## Total 'Fishing duration' from "CE table"
	popDurationPerDays <- tapply(effTable$CEoffVesFishHour, list(effTable$CEfishTech, effTable$CEoffFishDay), sum, na.rm=TRUE)
	
	## Discard estimates by fishing days
	popDiscPerDays <- (popDurationPerDays * meanDpueTrp)/1000

	## Variance estimates for total duration by fishing days (popVarPerDays)		
	popVarPerDays <- (popDurationPerDays^2 * varDpueTrp)/1000000

	## Total discards and variance, by fleet	
	popDiscTotalMetier <- apply(popDiscPerDays, c(1), sum, na.rm=TRUE)
	popVarTotalMetier <- apply(popVarPerDays, c(1), sum, na.rm=TRUE)
