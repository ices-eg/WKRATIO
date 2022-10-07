
#' Plot number at age
#' @param numAtAge data.frame with the columns 'age' (int) and 'numAtAge' (num)
#' @param variance data.frame with columns age (int) and variance (num)
#' @examples
#'  data(officialTotalMac)
#'  data(officialVarMac)
#'  plotNumAtAge(officialTotalMac, officialVarMac)
#' @export
plotNumAtAge <- function(numAtAge, variance=NULL){

  pl <- ggplot2::ggplot(numAtAge, ggplot2::aes_string(x="age", y="numAtAge")) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::ylab("number") +
    ggplot2::xlab("age")

  if (!is.null(variance)){
    vv <- merge(variance, numAtAge)
    vv$sd <- sqrt(vv$variance)
    stopifnot(nrow(vv)==nrow(numAtAge))
    vv$lower <- vv$numAtAge - vv$sd
    vv$upper <- vv$numAtAge + vv$sd

    pl <- pl +
      ggplot2::geom_errorbar(data=vv, ggplot2::aes_string(x="age", ymin="lower", ymax="upper")) +
      ggplot2::ylab("number +/- SD")

  }

  pl
}

#' Plots ratio strata coverage
#' @description
#'  Plots the total landing and the standard error of the ratio of number at age to catchweight in each strata
#' @param ratio_vars variance of estimate of ratio: data.frame with columns 'straum' (chr), 'age' (int), 'SDid' (int) and 'variance' (num)
#' @param landings CE table with the column 'stratum' added
#' @param age age to show variance for
#' @param cumulativeWeightCutoff threshold for cumulative weight. Smallest strata are excluded from plot after this threshold
#' @export
plot_ratio_coverage <- function(ratio_vars, landings, age, cumulativeWeightCutoff=.99){
  stopifnot(all(ratio_vars$stratum %in% landings$stratum))
  stopifnot(all(landings$stratum %in% ratio_vars$stratum))

  ratio_vars <- ratio_vars[ratio_vars$age == age,]

  strataTotalWeight <- stats::aggregate(list(totalWeight=landings$CLofficialWeight),
                                        by=list(stratum=landings$stratum), sum)

  strataTotalWeight <- strataTotalWeight[order(strataTotalWeight$totalWeight, decreasing = T),]
  strataTotalWeight$cumulativeFraction <- cumsum(strataTotalWeight$totalWeight) / sum(strataTotalWeight$totalWeight)
  strataTotalWeight <- strataTotalWeight[strataTotalWeight$cumulativeFraction < cumulativeWeightCutoff,]


  combined <- merge(strataTotalWeight, ratio_vars)
  combined <- combined[order(combined$totalWeight, decreasing = T),]



  combined$stratumFactor <- factor(combined$stratum, levels=combined$stratum, ordered = T)
  combined$SD <- sqrt(combined$variance)
  combined$unsampled <- is.na(combined$variance)

  ggplot2::ggplot(combined) +
    ggplot2::geom_col(ggplot2::aes_string(x="stratumFactor", y="totalWeight", fill="SD", color="unsampled")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, vjust=1)) +
    ggplot2::xlab("stratum") +
    ggplot2::ylab("landed weight (kg)") +
    ggplot2::labs(fill="sd (N/g)") +
    ggplot2::scale_fill_gradient(low=scales::muted("red"),high="#fce7e6", na.value = "white") +
    ggplot2::ggtitle(paste("sd for ratio: N@age:", age, " / weight", sep=""))

}

