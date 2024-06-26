% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_feature_plots.R
\name{ssvFeatureUpset}
\alias{ssvFeatureUpset}
\title{ssvFeatureUpset}
\usage{
ssvFeatureUpset(
  object,
  return_UpSetR = FALSE,
  nsets = NULL,
  nintersects = 15,
  order.by = "freq",
  ...
)
}
\arguments{
\item{object}{will be passed to \code{\link{ssvMakeMembTable}} for conversion
to membership matrix}

\item{return_UpSetR}{If TRUE, return the UpSetR object, The default is FALSE
and results in a ggplotified version compatible with cowplot etc.}

\item{nsets}{Number of sets to look at}

\item{nintersects}{Number of intersections to plot. If set to NA, all
intersections will be plotted.}

\item{order.by}{How the intersections in the matrix should be ordered by.
Options include frequency (entered as "freq"), degree, or both in any
order.}

\item{...}{Additional parameters passed to \code{\link[UpSetR]{upset}} in the
UpSetR  package.}
}
\value{
ggplot version of UpSetR plot
}
\description{
Uses the UpSetR package to create an \link[UpSetR:upset]{UpSetR::upset} plot of region overlaps.
}
\examples{
data(CTCF_in_10a_overlaps_gr)
ssvFeatureUpset(list(1:3, 2:6))
ssvFeatureUpset(CTCF_in_10a_overlaps_gr)
ssvFeatureUpset(S4Vectors::mcols(CTCF_in_10a_overlaps_gr)[,2:3])
}
