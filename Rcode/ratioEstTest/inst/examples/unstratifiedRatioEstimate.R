library(ratioEstTest)

sampleData <- samples
landingsData <- landings$CL
landingsData <- landingsData[landingsData$CLspeciesFaoCode == "MAC",]


#
# data prosessing
#
sampleData$SA <- sampleData$SA[!is.na(sampleData$SA$SAtotalWeightLive),]
sampleData$SA <- sampleData$SA[sampleData$SA$SAstratification=="N",]

landingsData$stratum <- "U"
sampleData <- fixInclusionProb(sampleData)

#
# design-based stuff
#
sampleTotals <- numAtAgeBV(sampleData$BV)
ssTotals <- numAtAgeSpeciesSelectionHT(sampleData$SA, sampleTotals)
haulTotals <- numAtAgeHaulCensus(sampleData$SS, ssTotals)
ssWeight <- totalWeightSpeciesSelection(sampleData$SA)
haulWeight <- totalWeightHaulHT(sampleData$SS, ssWeight)

#
# ratio estimation
#
ratios <- ratio_wo_N("FO", sampleData$FO, haulTotals, haulWeight, "SDid")
est <- ratio_estimate_strata(ratios, landingsData)

ratio_vars <- ratio_variance_wo_N("FO", sampleData$FO, haulTotals, haulWeight, "SDid")
strata_vars <- ratio_variance_strata(ratio_vars, landingsData)

#
# design-based stuff again
#
total <- total_stratified(est)
totalVar <- variance_stratified(strata_vars)

plotNumAtAge(total, totalVar)
