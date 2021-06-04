
#' Plot number at age
#' @param numAtAge data.frame with the columns 'age' (int) and 'numAtAge' (num)
#' @param variance data.frame with columns age (int) and variance (num)
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
