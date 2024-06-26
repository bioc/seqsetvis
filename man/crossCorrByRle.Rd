% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_fragLen.R
\name{crossCorrByRle}
\alias{crossCorrByRle}
\title{Calculate cross correlation by using shiftApply on read coverage Rle}
\usage{
crossCorrByRle(
  bam_file,
  query_gr,
  max_dupes = 1,
  fragment_sizes = 50:300,
  read_length = NULL,
  flip_strand = FALSE,
  ...
)
}
\arguments{
\item{bam_file}{character. Path to .bam file, must have index at .bam.bai.}

\item{query_gr}{GRanges.  Regions to calculate cross correlation for.}

\item{max_dupes}{integer.  Duplicate reads above this value will be removed.}

\item{fragment_sizes}{integer.  fragment size range to search for maximum
correlation.}

\item{read_length}{integer. Any values outside fragment_range that must be
searched.  If not supplied will be determined from bam_file.  Set as NA
to disable this behavior.}

\item{flip_strand}{boolean. if TRUE strands that reads align to are swapped.
This is typically only necessary if there was a mismatch between library
chemistry and aligner settings. Default is FALSE.}

\item{...}{arguments passed to ScanBamParam}
}
\value{
named list of results
}
\description{
Calculate cross correlation by using shiftApply on read coverage Rle
}
\examples{
data(CTCF_in_10a_overlaps_gr)
bam_f = system.file("extdata/test.bam",
    package = "seqsetvis", mustWork = TRUE)
query_gr = CTCF_in_10a_overlaps_gr[1:2]
crossCorrByRle(bam_f, query_gr[1:2], fragment_sizes = seq(50, 300, 50))
}
