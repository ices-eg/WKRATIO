% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate.R
\name{numAtAgeHaulCensus}
\alias{numAtAgeHaulCensus}
\title{Calculate number at age from species-selection totals}
\usage{
numAtAgeHaulCensus(SS, SStotals)
}
\arguments{
\item{SS}{SS table}

\item{SStotals}{number at age for species selection (SS)}
}
\value{
data.frame with columns 'FOid' (int), 'age' (int), and 'total' (num)
}
\description{
Calculate number at age from species-selection totals.
Only support census-application of species lists
}
\details{
Haults with error if any species lists are sampled from a set of species lists
}
