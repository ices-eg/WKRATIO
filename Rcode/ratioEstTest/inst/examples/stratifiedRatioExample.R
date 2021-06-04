library(ratioEstTest)

sampleData <- samples
landingsData <- landings$CL
landingsData <- landingsData[landingsData$CLspeciesFaoCode == "MAC",]

#
# data processing
#

sampleData$SA <- sampleData$SA[!is.na(sampleData$SA$SAtotalWeightLive),]
sampleData$SA <- sampleData$SA[sampleData$SA$SAstratification=="N",]

landingsData <- postStratifyMacPelLandings(landingsData)
sampleData <- fixInclusionProb(sampleData)
sampleData <- postStratifyMacPelSamples(sampleData)

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
ratio_vars <- ratio_variance_wo_N("FO", sampleData$FO, haulTotals, haulWeight, "SDid")

#
# data processing
#

ratios <- addMissingStratas(ratios, landingsData)
ratio_vars <- addMissingStratas(ratio_vars, landingsData)
plot_ratio_coverage(ratio_vars, landingsData, 8) #inspection of plot indicate that more than 95% of landed catch is sampled. The rest will be imputed by the following lines
ratios <- medianRatioImputation(ratios)
ratio_vars <- maxVarianceImputation(ratio_vars)

#
# ratio estimation again
#
est <- ratio_estimate_strata(ratios, landingsData)
strata_vars <- ratio_variance_strata(ratio_vars, landingsData)

#
# design-based stuff again
#
total <- total_stratified(est)
totalVar <- variance_stratified(strata_vars)

plotNumAtAge(total, totalVar)
