
#' Plot number at age
#' @param numAtAge data.frame with the columns 'age' (int) and 'numAtAge' (num)
#' @export
plotNumAtAge <- function(numAtAge){

  ggplot2::ggplot(numAtAge, ggplot2::aes_string(x="age", y="numAtAge")) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::ylab("number") +
    ggplot2::xlab("age")
}
