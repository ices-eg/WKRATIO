#' Estimates inclusion probabilities
#' @description
#'  Estimates inclusion probabilities on SA table from catch weights and individual weights
#'  of age determined fish.
#'  In effect considers age sample fish as a SRSWOR selection from haul
#'  These inclusion probabilities may not be applicable for estimation with other parameters
#' @details
#'  Halts with error of any SA entries are missing values for SAsampleWeightLive or SAtotalWeightLive
#' @param RDBESsamples RDBES data as exemplified in \code{\link{samples}}
#' @return RDBESsamples with SAinclusionProb set
#' @export
fixInclusionProb <- function(RDBESsamples){

  weights <- RDBESsamples$BV[RDBESsamples$BV$BVtype == "WeightLive",c("SAid", "BVfishId", "BVvalue")]
  ages <- RDBESsamples$BV[RDBESsamples$BV$BVtype == "Age",c("SAid", "BVfishId")]
  agedweights <- merge(ages, weights)
  sampleWeights <- stats::aggregate(list(iWeight=as.numeric(agedweights$BVvalue)), by=list(SAid=agedweights$SAid), sum)
  RDBESsamples$SA <- merge(RDBESsamples$SA, sampleWeights)
  stopifnot(all(!is.na(RDBESsamples$SA$SAsampleWeightLive)))
  stopifnot(all(!is.na(RDBESsamples$SA$SAtotalWeightLive)))
  stopifnot(all(!is.na(RDBESsamples$SA$iWeight)))
  RDBESsamples$SA$SAinclusionProb <- RDBESsamples$SA$iWeight / as.numeric(RDBESsamples$SA$SAtotalWeightLive)

  RDBESsamples$SA$iWeight <- NULL
  return(RDBESsamples)
}

#' Adds any strata missing from ratios table
#' @param ratios data.frame with columns SDid (int), stratum (chr), age (int), ratio (num)
#' @param landings CL table with the column 'stratum' added
#' @return 'ratios' with rows added for missing strata (NA for SDid and ratio)
#' @export
addMissingStratas <- function(ratios, landings){

  allStrata <- unique(landings$stratum)
  stopifnot(all(ratios$stratum %in% allStrata))
  missingStrata <- allStrata[!(allStrata %in% ratios$stratum)]
  join <- merge(data.frame(age=min(ratios$age):max(ratios$age)), data.frame(stratum=missingStrata))
  return(merge(ratios, join, all=T))
}

#' Impute ratios
#' @description
#'  Impute ratios to strata
#' @details
#'  The ratio for a stratum will be replaced by the one from a different stratum according to the list
#'  'map'. After execution the ratio for stratum A will be \code{map[A]}, whether the ratio for A was unkwon or not
#'
#'  Halts with error if the ratio for \code{map[A]} is NA.
#' @param ratios data.frame with columns SDid (int), stratum (chr), age (int), ratio (num)
#' @param map list mapping a stratum to another stratum with known ratio
#' @export
imputeRatios <- function(ratios, map){
  ratios <- ratios[order(ratios$stratum, ratios$age),]
  for (m in names(map)){
    stopifnot(all(!is.na(ratios$ratio[ratios$stratum == map[m]])))
    stopifnot(nrow(ratios[ratios$stratum==m,c("SDid","ratio")]) == nrow(ratios[ratios$stratum==map[m],c("SDid","ratio")]))
    ratios[ratios$stratum==m,c("SDid","ratio")] <- ratios[ratios$stratum==map[m],c("SDid","ratio")]
  }
  return(ratios)
}

#' Median ratio imputation
#' @description
#'  Assign to unsampled strata the median ratio of that age groupof other strata
#' @param ratio data.frame with columns SDid (int), stratum (chr), age (int), ratio (num)
#' @return 'ratio' with ratios for unsampled strata imputed
#' @export
medianRatioImputation <- function(ratio){
  medianR <- stats::aggregate(list(median=ratio$ratio), by=list(age=ratio$age), FUN=function(x){stats::median(x, na.rm=T)})

  medianR$SDid <- max(ratio$SDid, na.rm=T) + 1
  ratio$SDid[is.na(ratio$SDid)] <- max(ratio$SDid, na.rm=T) + 1
  newR <- merge(ratio, medianR, all=T)
  stopifnot(nrow(newR) == nrow(ratio))
  newR$ratio[is.na(newR$ratio)] <- newR$median[is.na(newR$ratio)]
  newR$median <- NULL

  return(newR)
}

#' Max variance imputation
#' @description
#'  Assign to unsampled strata the maximal variance among the sampled strata
#' @param ratio_var data.frame with columns SDid (int), stratum (chr), age (int), variance (num)
#' @return 'ratio_var' with ratios for unsampled strata imputed
#' @export
maxVarianceImputation <- function(ratio_var){
  maxV <- stats::aggregate(list(maxVar=ratio_var$variance), by=list(age=ratio_var$age), FUN=function(x){max(x, na.rm=T)})

  maxV$SDid <- max(ratio_var$SDid, na.rm=T) + 1
  ratio_var$SDid[is.na(ratio_var$SDid) | is.na(ratio_var$variance)] <- max(ratio_var$SDid, na.rm=T) + 1

  newVar <- merge(ratio_var, maxV, all=T)
  stopifnot(nrow(newVar) == nrow(ratio_var))
  newVar$variance[is.na(newVar$variance)] <- newVar$maxVar[is.na(newVar$variance)]
  newVar$maxVar <- NULL

  return(newVar)
}

#' Annotate post-stratification landings
#' @description
#'  Applies a post-stratification scheme that matches the one produced by \code{\link{postStratifyMacPelSamples}}
#' @param landings CL table
#' @return 'landings' with the column 'stratum' added
#' @export
postStratifyMacPelLandings <- function(landings){
  gear <- unlist(lapply(landings$CLmetier6, FUN=function(x){unlist(strsplit(x, "_"))[[1]]}))
  stopifnot(all(!is.na(gear)))

  stopifnot(all(!is.na(landings$CLquarter)))
  quarter <- paste("Q", landings$CLquarter, sep="")

  stopifnot(all(!is.na(landings$CLarea)))
  area <- landings$CLarea

  landings$stratum <- paste(gear, quarter, area, sep="/")

  return(landings)
}

#' Annotate post-stratification samples
#' @description
#'  Applies a post-stratification scheme that matches the one produced by \code{\link{postStratifyMacPelLandings}}
#' @param samples RDBES samples
#' @return 'samples' with the columns 'FOstratification' and 'FOstratumName' altered
#' @export
postStratifyMacPelSamples <- function(samples){

  stopifnot(all(!is.na(samples$FO$FOgear)))
  gear <- samples$FO$FOgear

  stopifnot(all(!is.na(samples$FO$FOstartDate)))
  quarter <- quarters(as.POSIXct(samples$FO$FOstartDate))

  stopifnot(all(!is.na(samples$FO$FOarea)))
  area <- samples$FO$FOarea

  samples$FO$FOstratumName <- paste(gear, quarter, area, sep="/")
  samples$FO$FOstratification <- "Y"

  return(samples)
}
