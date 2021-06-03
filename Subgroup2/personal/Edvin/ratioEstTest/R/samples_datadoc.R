#' Samples example
#'
#' Example data - Norwegian catch of mac.27.nea sampled in 2019 in RDBES format
#' Data was obtained from RDBES format as of the 2020 test data called.
#' The exchange format was parsed with the parser at:
#' https://github.com/davidcurrie2001/MI_RDBES_ExchangeFiles/blob/d38b75fe6a902892943cf01100cdbb7e9b13c21a/ReadExchangeFileExample_WKRDBEST2.R
#' and some fields like FO$SDid has been corrected afterwards.
#'
#' The data is not complete wrp non-response or missing obeservations.
#' For instance the BV table lists selection methods as CENSUS for all entries, but some FishIds have no age.
#'
#'
#' @docType data
#'
#' @usage data(samples)
#'
#' @format list of 8 data frames. Variables documented in https://github.com/ices-tools-dev/RDBES
#'
#' @keywords datasets
#'
#' @examples
#' data(samples)
#'
"samples"
