% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_signal_operations.R
\name{centerGRangesAtMax}
\alias{centerGRangesAtMax}
\title{Centers query GRanges at maximum signal in prof_dt.}
\usage{
centerGRangesAtMax(
  prof_dt,
  qgr,
  x_ = "x",
  y_ = "y",
  by_ = "id",
  width = 1,
  view_size = NULL
)
}
\arguments{
\item{prof_dt}{a GRanges or data.table as returned by ssvFetch*.}

\item{qgr}{the GRanges used to query ssvFetch* as the qgr argument.}

\item{x_}{positional variable.  Should almost always be the default, "x".}

\item{y_}{the signal value variable.  Likely the default value of "y" but
could be "y_norm" if append_ynorm was applied to data.}

\item{by_}{region identifier variable. Should almost always be the default,
"id".}

\item{width}{Desired width of final regions.  Default is 1.}

\item{view_size}{the size in \code{x_} to consider for finding the max
of \code{y_}.
if length(view_size) == 1, range will be c(-view_size, view_size).
if length(view_size) > 1, range will be range(view_size).
default value of NULL uses complete range of x.}
}
\value{
a GRanges with same mcols as qgr that has been centered based on
signal in prof_dt and with regions of specified width.
}
\description{
Centers query GRanges at maximum signal in prof_dt.
}
\examples{
data(CTCF_in_10a_overlaps_gr)
data(CTCF_in_10a_profiles_gr)
data(CTCF_in_10a_profiles_dt)
centerGRangesAtMax(CTCF_in_10a_profiles_dt, CTCF_in_10a_overlaps_gr)
centerGRangesAtMax(CTCF_in_10a_profiles_gr, CTCF_in_10a_overlaps_gr)

centerGRangesAtMax(CTCF_in_10a_profiles_gr, CTCF_in_10a_overlaps_gr, view_size = 50)

}
