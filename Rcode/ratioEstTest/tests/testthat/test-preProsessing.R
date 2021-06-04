context("Test fixIncProb")
sa <- samples
sa$SA <- sa$SA[!is.na(sa$SA$SAtotalWeightLive),]
sa <- fixInclusionProb(sa)

context("Test Add missing stratas")

ll <- landings
ll$stratum <- unlist(lapply(landings$CL$CLmetier6, function(x){paste(unlist(strsplit(x, "_"))[1:2],collapse="_")}))
testRatios <- readRDS(system.file("testresources", "testRatiosStratified.rds", package="ratioEstTest"))

ratios <- addMissingStratas(testRatios, ll)

context("Test imputeRatios")
imputedRatios <- imputeRatios(ratios, list(SDN_DEF="OTB_SPF", MIS_MIS="PS_SPF"))
expect_equal(imputedRatios$ratio[imputedRatios$stratum=="SDN_DEF"], imputedRatios$ratio[imputedRatios$stratum=="OTB_SPF"])

context("Test postStratifyMacPelLandings")
ll <- landings$CL
ll <- postStratifyMacPelLandings(ll)

sa <- samples
sa <- postStratifyMacPelSamples(sa)

expect_true(all(sa$FO$FOstratumName %in% ll$stratum))

