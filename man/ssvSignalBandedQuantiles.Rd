% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_signal_plots.R
\name{ssvSignalBandedQuantiles}
\alias{ssvSignalBandedQuantiles}
\title{plot profiles from bigwigs}
\usage{
ssvSignalBandedQuantiles(
  bw_data,
  y_ = "y",
  x_ = "x",
  by_ = "fake",
  hsv_reverse = FALSE,
  hsv_saturation = 1,
  hsv_value = 1,
  hsv_grayscale = FALSE,
  hsv_hue_min = 0,
  hsv_hue_max = 0.7,
  hsv_symmetric = FALSE,
  n_quantile = 18,
  quantile_min = 0.05,
  quantile_max = 0.95,
  return_data = FALSE
)
}
\arguments{
\item{bw_data}{a GRanges or data.table of bigwig signal.
As returned from \code{\link{ssvFetchBam}} and \code{\link{ssvFetchBigwig}}}

\item{y_}{the variable name in bw_data for y axis in plot}

\item{x_}{the variable name in bw_data for x axis in plot}

\item{by_}{the variable name in bw_data to facet on}

\item{hsv_reverse}{logical, should color scale be reversed? default FALSE}

\item{hsv_saturation}{numeric value from 0 to 1. Saturation for color scale. default 1}

\item{hsv_value}{numeric value from 0 to 1. Value for color scale. default 1}

\item{hsv_grayscale}{logical, if TRUE gray() is used instead of rainbow(). default FALSE}

\item{hsv_hue_min}{numeric [0, hsv_hue_max) hue min of color scale}

\item{hsv_hue_max}{numeric (hsv_hue_min, 1] hue max of color scale}

\item{hsv_symmetric}{if TRUE, colorscale is symmetrical, default FALSE.}

\item{n_quantile}{number of evenly size quantile bins}

\item{quantile_min}{the lowest quantile start}

\item{quantile_max}{the highest quantile end}

\item{return_data}{logical.  If TRUE, return value is no longer ggplot and
is instead the data used to generate that plot. Default is FALSE.}
}
\value{
ggplot object using ribbon plots to show quantile distributions
}
\description{
plot profiles from bigwigs
}
\examples{
data(CTCF_in_10a_profiles_gr)
#rainbow colors
qgr = CTCF_in_10a_profiles_gr
ssvSignalBandedQuantiles(qgr)
#grayscale
ssvSignalBandedQuantiles(qgr, hsv_grayscale = TRUE,
    hsv_symmetric = TRUE, hsv_reverse = TRUE)
#using "by_" per sample
ssvSignalBandedQuantiles(qgr, hsv_grayscale = TRUE,
    hsv_symmetric = TRUE, hsv_reverse = TRUE, by_ = "sample")
#adding spline smoothing
splined = applySpline(qgr, n = 10,
    by_ = c("id", "sample"))
ssvSignalBandedQuantiles(splined, n_quantile = 50,
    quantile_min = .25, quantile_max = .75,
    hsv_symmetric = TRUE, hsv_reverse = TRUE, by_ = "sample")
}
