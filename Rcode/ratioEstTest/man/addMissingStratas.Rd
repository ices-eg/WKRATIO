% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preProsessing.R
\name{addMissingStratas}
\alias{addMissingStratas}
\title{Adds any strata missing from ratios table}
\usage{
addMissingStratas(ratios, landings)
}
\arguments{
\item{ratios}{data.frame with columns SDid (int), stratum (chr), age (int), ratio (num)}

\item{landings}{CL table with the column 'stratum' added}
}
\value{
'ratios' with rows added for missing strata (NA for SDid and ratio)
}
\description{
Adds any strata missing from ratios table
}
