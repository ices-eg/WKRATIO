#' Estimates inclusion probabilities
#' @description
#'  Estimates inclusion probabilities on SA table from catch weights and sample weights
#' @details
#'  Halts with error of any SA entries are missing values for SAsampleWeightLive or SAtotalWeightLive
#' @param samples RDBES data as exemplified in \code{\link{samples}}
#' @return samples with SAinclusionProb set
#' @export
fixInclusionProb <- function(samples){
  stopifnot(all(!is.na(samples$SA$SAsampleWeightLive)))
  stopifnot(all(!is.na(samples$SA$SAtotalWeightLive)))
  samples$SA$SAinclusionProb <- as.numeric(samples$SA$SAsampleWeightLive) / as.numeric(samples$SA$SAtotalWeightLive)
  return(samples)
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
