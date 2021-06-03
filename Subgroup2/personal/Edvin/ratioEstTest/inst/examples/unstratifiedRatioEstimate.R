library(ratioEstTest)

sampleData <- samples
landingsData <- landings$CL

#
# preprosessing
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
ratios <- ratio_wo_N("FO", samples$FO, haulTotals, haulWeight, "SDid")
est <- ratio_estimate_strata(ratios, landingsData)

#
# design-based stuff again
#
total <- total_stratified(est)

plotNumAtAge(total)
