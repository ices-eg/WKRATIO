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
