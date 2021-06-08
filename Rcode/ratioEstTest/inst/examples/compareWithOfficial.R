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

#
# compare with official estimate
#
total$method <- "ratio"
officialTotalMac$method <- "ECA"
totalVar$method <- "ratio"
officialVarMac$method <- "ECA"

total <- rbind(total, officialTotalMac)
totalVar <- rbind(totalVar, officialVarMac)

combined <- merge(total, totalVar)
combined$lower <- combined$numAtAge - 2*sqrt(combined$variance)
combined$upper <- combined$numAtAge + 2*sqrt(combined$variance)


ggplot2::ggplot(combined, ggplot2::aes_string(x="age", y="numAtAge", fill="method")) +
  ggplot2::geom_bar(position="dodge", stat="identity") +
  ggplot2::geom_errorbar(position=ggplot2::position_dodge(width=0.9), ggplot2::aes_string(x="age", ymin="lower", ymax="upper")) +
  ggplot2::ylab("number +/- 2 SD") +
  ggplot2::ggtitle("Commercial catch MAC 2019")
