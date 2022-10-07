data(samples)

context("Test numAtAgeBV")
naa <- numAtAgeBV(samples$BV)
expect_equal(ncol(naa), 3)
expect_true(all(c("SAid", "age", "count") %in% names(naa)))

naa <- numAtAgeBV(samples$BV, 2,4)
expect_equal(nrow(naa), 3*length(unique(samples$BV$SAid[samples$BV$BVtype=="Age"])))
expect_equal(sum(naa$count), sum(samples$BV$BVvalue[samples$BV$BVtype=="Age"] %in% 2:4))


samples$SA <- samples$SA[!is.na(samples$SA$SAtotalWeightLive),]
samples$SA <- samples$SA[samples$SA$SAstratification=="N",]
samples <- fixInclusionProb(samples)
context("Test numAtAgeSpeciesSelectionHT")
ssTotals <- numAtAgeSpeciesSelectionHT(samples$SA, naa)

context("Test numAtAgeHaulCensus")
haulTotals <- numAtAgeHaulCensus(samples$SS, ssTotals)

context("Test totalWeightSpeciesSelection")
ssWeight <- totalWeightSpeciesSelection(samples$SA)
context("Test totalWeightHaulHT")
haulWeight <- totalWeightHaulHT(samples$SS, ssWeight)

context("Test ratio_wo_N")
ratios <- ratio_wo_N("FO", samples$FO, haulTotals, haulWeight, "SDid")

land <- landings$CL
land <- land[land$CLspeciesFaoCode == "MAC",]
land$stratum <- "U"

context("Test ratio_estimate_strata")
est <- ratio_estimate_strata(ratios, land)

context("Test total_stratified")
total <- total_stratified(est)

context("Test ratio_variance_wo_N")
ratio_vars <- ratio_variance_wo_N("FO", samples$FO, haulTotals, haulWeight, "SDid")
expect_true(all((sqrt(ratio_vars$variance) / ratios$ratio)<1))

context("Test ratio_variance_strata")
strata_vars <- ratio_variance_strata(ratio_vars, land)
expect_equal(nrow(strata_vars), nrow(est))
estTab <- merge(strata_vars, est)
expect_true(all((sqrt(estTab$variance) / estTab$numAtAge)<1))

variance_stratified(strata_vars)
totalVar <- variance_stratified(strata_vars)
expect_equal(nrow(totalVar), length(unique(totalVar$age)))

context("Test stratfied ratio estimate")
fos <- samples$FO
fos$FOstratification = "Y"
fos$FOstratumName[1:10] <- "s1"
fos$FOstratumName[10:nrow(fos)] <- "s2"
ratios <- ratio_wo_N("FO", fos, haulTotals, haulWeight, "SDid")
land <- landings$CL
land <- land[land$CLspeciesFaoCode == "MAC",]
land$stratum <- "s1"
land$stratum[1:300] <- "s2"

est <- ratio_estimate_strata(ratios, land)
totalS <- total_stratified(est)

ratios <- rbind(ratios, ratios[1:2,])
ratios$SDid[1:2] <- 2
est <- ratio_estimate_strata(ratios, land)

expect_error(total_stratified(est))


