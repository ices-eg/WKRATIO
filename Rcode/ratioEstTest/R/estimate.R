
#' Calculate number at age
#' @description
#'  Calculates number of fish at each age in a provided age range,
#'  for each sample (SAid).
#' @details
#'  halts with error if fish is selected in stratified manner
#'  or if any ages are missing.
#' @param BV BV table
#' @param minAge minimal age to include in estimation, define from data if NULL
#' @param maxAge minimal age to include in estimation, define from data if NULL
#' @return data.frame with columns 'SAid', 'age' and 'count', all integers
#' @export
numAtAgeBV <- function(BV, minAge=NULL, maxAge=NULL){
  ages <- BV[BV$BVtype == "Age",]
  stopifnot(all(ages$BVstratification == "N"))
  stopifnot(all(!is.na(ages$BVvalue)))
  ages$BVvalue <- as.integer(ages$BVvalue)

  if (is.null(minAge)){
    minAge <- min(ages$BVvalue)
  }
  if (is.null(maxAge)){
    maxAge <- max(ages$BVvalue)
  }

  join <- merge(ages[,c("SAid", "BVvalue")], data.frame(age=minAge:maxAge))
  join$atAge <- join$BVvalue == join$age
  agecounts <- stats::aggregate(list(count=join$atAge), by=list(SAid=join$SAid, age=join$age), sum, drop=F)

  return(agecounts)

}

#' Horvitz-Thompson estimator for a sample of SA entries
#' @description
#'  Estimates total for a given species selection
#' @details
#'  Haults with error if SA table has missing values for some inclusion probabilites.
#'  Haults with error if SA table stratified entries.
#'  Haults with error if SA table has missing values for species codes or contain more than one species code.
#' @param SA SA table
#' @param sampleTotals data.frame with columns 'SSid', 'age' and 'count', all integers
#' @return data.frame with columns 'SSid' (int), 'age' (int), and 'total' (num)
#' @export
numAtAgeSpeciesSelectionHT <- function(SA, sampleTotals){
  stopifnot(all(!is.na(SA$SAinclusionProb)))
  stopifnot(all(SA$SAstratification == "N"))
  stopifnot(all(!is.na(SA$SAspeciesCode)))
  stopifnot(length(unique(SA$SAspeciesCode)) == 1)
  sa <- merge(SA[,c("SSid", "SAid", "SAinclusionProb")], sampleTotals)
  sa$weightedTotals <- (1/sa$SAinclusionProb) * sa$count
  return(stats::aggregate(list(total=sa$weightedTotals), by=list(SSid=sa$SSid, age=sa$age), FUN=sum, drop=F))
}

#' Calculate number at age from species-selection totals
#' @description
#'  Calculate number at age from species-selection totals.
#'  Only support census-application of species lists
#' @details
#'  Haults with error if any species lists are sampled from a set of species lists
#' @param SS SS table
#' @param SStotals number at age for species selection (SS)
#' @return data.frame with columns 'FOid' (int), 'age' (int), and 'total' (num)
#' @export
numAtAgeHaulCensus <- function(SS, SStotals){
  stopifnot(all(SS$SSselectionMethod=="CENSUS"))
  #stopifnot(length(unique(SS$FOid)) == length(unique(paste(SS$FOid, SS$SSid))))
  fos <- merge(SS[,c("FOid", "SSid")], SStotals)
  fototals <- stats::aggregate(list(total=fos$total), by=list(FOid=fos$FOid, age=fos$age), sum, drop=F)
  return(fototals)
}

#' Calculate total weight for species selection from SA entires
#' @description
#'  Calculates total for a given species selection
#' @details
#'  Haults with error if SA table stratified entries.
#'  Haults with error if SA table has missing values for species codes or contain more than one species code.
#' @param SA SA table
#' @return data.frame with columns 'SSid' (int), and 'weight' (num)
#' @export
totalWeightSpeciesSelection <- function(SA){
  stopifnot(all(SA$SAstratification == "N"))
  stopifnot(all(!is.na(SA$SAspeciesCode)))
  stopifnot(length(unique(SA$SAspeciesCode)) == 1)
  return(stats::aggregate(list(weight=as.numeric(SA$SAtotalWeightLive)), by=list(SSid=SA$SSid), FUN=sum, drop=F))
}

#' Calculate haul weight from species-selection totals
#' @description
#'  Calculate haul weihgt from species-selection totals.
#'  Only support census-application of species lists
#' @details
#'  Haults with error if any species lists are sampled from a set of species lists
#' @param SS SS table
#' @param SStotals total weight for each sample (SA)
#' @return data.frame with columns 'FOid' (int), 'age' (int), and 'total' (num)
#' @export
totalWeightHaulHT <- function(SS, SStotals){
  stopifnot(all(SS$SSselectionMethod=="CENSUS"))
  #stopifnot(length(unique(SS$FOid)) == length(unique(paste(SS$FOid, SS$SSid))))
  fos <- merge(SS[,c("FOid", "SSid")], SStotals)
  fototals <- stats::aggregate(list(weight=fos$weight), by=list(FOid=fos$FOid), sum, drop=F)
  return(fototals)
}

#' Estimates ratio of number at age and weight
#' @description
#'  Estimates ratio of number at age and weight
#'  when samples are selected SRSWOR and population totals are not available.
#' @details
#'  The function allows estimation from a sample of different kinds of sampling units.
#'  The 'sampleTable' is a table representing one of the leves in the RDBES hierarchy and has necessary information about stratification etc.
#'  The 'numAtAge' table and the 'totalWeight' table contains estimate of the number at age and the catch weight for each of the unit in the sample.
#'  In addition they contain a column identifying the sample unit. E.g 'FOid' if 'sampleTable' is an FO-table, or 'LEid' if the sampleTable is an 'LE'-table
#'
#'  Halts with error if the sample is a clustered sample
#' @param sampleUnitType character identifying the kind of sample units in the sample, follows the record type sin RDBES, e.g. "FO" for an FO-table.
#' @param sampleTable table with information about the sample (e.g. FO-table in the RBDES data model)
#' @param numAtAge data.frame with columns: 'age' (int), 'total' (num, estimated total for sample unit), and a column identifying the sample unit (see details).
#' @param totalWeight data.frame with columns: 'total' (num, estimated total for sample unit), and a column identifying the sample unit (see details).
#' @param parentIdname name of column sampleUnitType that identifies the parent sample in the RDBES hierachy.
#' @return data.frame with columns parentIdname (int), stratum (chr), age (int), ratio (num)
#' @export
ratio_wo_N <- function(sampleUnitType, sampleTable, numAtAge, totalWeight, parentIdname){

  sid <- paste(sampleUnitType, "id", sep="")
  stratificationFlag <- paste(sampleUnitType, "stratification", sep="")
  stratificationColumn <- paste(sampleUnitType, "stratumName", sep="")
  clusteringFlag <- paste(sampleUnitType, "clustering", sep="")

  stopifnot(all(sampleTable[[stratificationFlag]][sampleTable[[stratificationColumn]] == "U"] == "N"))
  stopifnot(all(sampleTable[[stratificationColumn]][sampleTable[[stratificationFlag]] == "Y"] != "U"))
  stopifnot(parentIdname %in% names(sampleTable))
  stopifnot(all(sampleTable[[clusteringFlag]] == "N"))

  numAtAge <- merge(sampleTable, numAtAge, by=sid)
  totalWeight <- merge(sampleTable, totalWeight, by=sid)

  strataTotalNumAtAge <- stats::aggregate(list(numAtAge=numAtAge$total), by=list(age=numAtAge$age, parent=numAtAge[[parentIdname]], stratum=numAtAge[[stratificationColumn]]), sum, drop=F)
  strataWeight <- stats::aggregate(list(weight=totalWeight$weight), by=list(parent=totalWeight[[parentIdname]], stratum=totalWeight[[stratificationColumn]]), sum, drop=F)

  strataRatio <- merge(strataTotalNumAtAge, strataWeight)
  strataRatio$ratio <- strataRatio$numAtAge / strataRatio$weight
  names(strataRatio)[[1]] <- parentIdname

  return(strataRatio[,c(parentIdname, "stratum", "age", "ratio")])

}

#' Estimates total number at age for each strata and sampling scheme.
#' @details
#'  Haults with error if 'SDid' is not a column of 'ratios'
#'  Haults with error of 'stratum' is not a column of 'landings'
#'  Haults with error if ratios are not provided for all strata or if ratios are provided for strata not in cencus ('landings')
#'  Haults with error if there are more than one species code in the landings
#' @param ratios data.frame with columns SDid (int), stratum (chr), age (int), ratio (num)
#' @param landings CL table with the column 'stratum' added
#' @return data.frame with columns SDid (int), stratum (chr), age (int), numAtAge (num)
#' @export
ratio_estimate_strata <- function(ratios, landings){

  stopifnot("SDid" %in% names(ratios))
  stopifnot("stratum" %in% names(landings))
  stopifnot(all(landings$stratum %in% ratios$stratum))
  stopifnot(all(ratios$stratum %in% landings$stratum))
  stopifnot(length(unique(landings$CLspeciesCode))==1)

  land <- stats::aggregate(list(landedWeight=landings$CLofficialWeight), by=list(stratum=landings$stratum), sum)
  land <- merge(land, ratios)
  land$numAtAge <- (land$landedWeight*1000) * land$ratio

  return(land[,c("SDid", "stratum", "age", "numAtAge")])
}

#' Estimates grand total
#' @description
#'  Estimates grand total number at age from strata totals
#' @param strata_totals data.frame with columns SDid (int), stratum (chr), age (int), numAtAge (num)
#' @return data.frame with columns age (int) and numAtAge (num)
#' @export
total_stratified <- function(strata_totals){

  stopifnot(all(!is.na(strata_totals$age)))
  schemecounts <- stats::aggregate(list(schemes=strata_totals$SDid), by=list(stratum=strata_totals$stratum, age=strata_totals$age), function(x){length(unique(x))})
  if (any(schemecounts$schemes > 1)){
    stop("Overlapping sampling schemes not supported. Some age is reported in the same strata with different SDid")
  }

  total <- stats::aggregate(list(numAtAge=strata_totals$numAtAge), by=list(age=strata_totals$age), sum)
  return(total)

}

#' Estimate variance of ratio
#' @description
#'  Approximate estimation for the variance of the ratio between numberAtAge and weight
#'  Adapted from Hankin, Mohr and Newman, Eq. 7.19
#'
#'  The 'sampleTable' is a table representing one of the leves in the RDBES hierarchy and has necessary information about stratification etc.
#'  The 'numAtAge' table and the 'totalWeight' table contains estimate of the number at age and the catch weight for each of the unit in the sample.
#'  In addition they contain a column identifying the sample unit. E.g 'FOid' if 'sampleTable' is an FO-table, or 'LEid' if the sampleTable is an 'LE'-table
#'
#'  Halts with error if the sample is a clustered sample
#' @param sampleUnitType character identifying the kind of sample units in the sample, follows the record type sin RDBES, e.g. "FO" for an FO-table.
#' @param sampleTable table with information about the sample (e.g. FO-table in the RBDES data model)
#' @param numAtAge data.frame with columns: 'age' (int), 'total' (num, estimated total for sample unit), and a column identifying the sample unit (see details).
#' @param totalWeight data.frame with columns: 'total' (num, estimated total for sample unit), and a column identifying the sample unit (see details).
#' @param parentIdname name of column sampleUnitType that identifies the parent sample in the RDBES hierachy.
#' @param fpc (N-n)/N, where N is population size and n is sample size. Defaults to 1, which is applicable when N >> n
#' @return data.frame with columns parentname (int), stratum (chr), age (int), and variance (num)
#' @export
ratio_variance_wo_N <- function(sampleUnitType, sampleTable, numAtAge, totalWeight, parentIdname, fpc=1){
  ratios <- ratio_wo_N(sampleUnitType, sampleTable, numAtAge, totalWeight, parentIdname)

  sampleSize <- stats::aggregate(list(sSize=sampleTable$FOstratumName), by=list(parent=sampleTable[[parentIdname]], stratum=sampleTable$FOstratumName), length)

  vars <- merge(numAtAge, ratios)
  vars <- merge(vars, totalWeight)

  vars$estTot <- vars$ratio * vars$weight
  vars$estDiff <- vars$total - vars$estTot
  vars$sqDiff <- vars$estDiff**2

  # Eq. 7.17
  varPrStratum <- stats::aggregate(list(variance=vars$sqDiff), by=list(parent=vars[[parentIdname]], stratum=vars$stratum, age=vars$age), sum)
  varPrStratum <- merge(varPrStratum, sampleSize)
  varPrStratum$variance <- varPrStratum$variance * (varPrStratum$sSize / (varPrStratum$sSize-1)) * fpc
  names(varPrStratum)[1] <- parentIdname

  # Eq. 7.19
  mw <- sum(totalWeight$weight)
  varPrStratum$variance <- varPrStratum$variance / (mw**2)
  varPrStratum$sSize <- NULL

  return(varPrStratum)
}

#' Variance for ratio estimates of totals
#' @description
#'  Estimates variance for totals in strata estimated by ratio estimator.
#' @details
#'  Haults with error if 'SDid' is not a column of 'ratios'
#'  Haults with error of 'stratum' is not a column of 'landings'
#'  Haults with error if ratios are not provided for all strata or if ratios are provided for strata not in cencus ('landings')
#'  Haults with error if there are more than one species code in the landings
#' @param ratio_variances variance of ratios data.frame with columns SDid (int), stratum (chr), age (int), variance (num)
#' @param landings CL table with the column 'stratum' added
#' @return ratio_variances with variance replaced with the variance for the strata total
#' @export
ratio_variance_strata <- function(ratio_variances, landings){

  stopifnot("SDid" %in% names(ratio_variances))
  stopifnot("stratum" %in% names(landings))
  stopifnot(all(landings$stratum %in% ratio_variances$stratum))
  stopifnot(all(ratio_variances$stratum %in% landings$stratum))
  stopifnot(length(unique(landings$CLspeciesCode))==1)

  land <- stats::aggregate(list(weight=landings$CLofficialWeight), by=list(stratum=landings$stratum), sum)

  strata_vars <- merge(ratio_variances, land[,c("stratum", "weight")])
  strata_vars$variance <- strata_vars$variance * (strata_vars$weight*1000)**2
  strata_vars$weight <- NULL
  return(strata_vars)
}

#' Estimates variance for grand total
#' @description
#'  Estimates variance for grand total number at age from strata variances
#' @param strata_variances data.frame with columns SDid (int), stratum (chr), age (int), variance (num)
#' @return data.frame with columns age (int) and variance (num)
#' @export
variance_stratified <- function(strata_variances){
  stopifnot(all(!is.na(strata_variances$age)))
  schemecounts <- stats::aggregate(list(schemes=strata_variances$SDid), by=list(stratum=strata_variances$stratum, age=strata_variances$age), function(x){length(unique(x))})
  if (any(schemecounts$schemes > 1)){
    stop("Overlapping sampling schemes not supported. Some age is reported in the same strata with different SDid")
  }

  totalVar <- stats::aggregate(list(variance=strata_variances$variance), by=list(age=strata_variances$age), sum)
  return(totalVar)
}



