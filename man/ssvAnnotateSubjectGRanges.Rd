% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_subject_GR.R
\name{ssvAnnotateSubjectGRanges}
\alias{ssvAnnotateSubjectGRanges}
\alias{ssvAnnotateSubjectGRanges,GRanges-method}
\alias{ssvAnnotateSubjectGRanges,list-method}
\alias{ssvAnnotateSubjectGRanges,GRangesList-method}
\title{ssvAnnotateSubjectGRanges}
\usage{
ssvAnnotateSubjectGRanges(
  annotation_source,
  subject_gr,
  annotation_name = NULL,
  multi_resolver_FUN = "default"
)

\S4method{ssvAnnotateSubjectGRanges}{GRanges}(
  annotation_source,
  subject_gr,
  annotation_name = NULL,
  multi_resolver_FUN = "default"
)

\S4method{ssvAnnotateSubjectGRanges}{list}(
  annotation_source,
  subject_gr,
  annotation_name = NULL,
  multi_resolver_FUN = "default"
)

\S4method{ssvAnnotateSubjectGRanges}{GRangesList}(
  annotation_source,
  subject_gr,
  annotation_name = NULL,
  multi_resolver_FUN = "default"
)
}
\arguments{
\item{annotation_source}{A single GRanges, a list of GRanges, or a
GRangesList}

\item{subject_gr}{The base GRanges to add annotation mcols to.}

\item{annotation_name}{Optional name for single GRanges. Required for list
inputs if list does not have names.}

\item{multi_resolver_FUN}{Optional function to resolve multiple overlapping
annotation source regions per subject region. This function must accept 2
arguments. \code{x} is the values in a single mcol attribute and \code{variable.name}
is the name of variable. A single value must be returned or an error will
be generated. The default of "default" can handle numeric, logical,
character, and factor types.}
}
\value{
GRanges with the same regions as \code{subject_gr} but with addtional
mcols added from annotation_source.
}
\description{
ssvAnnotateSubjectGRanges
}
\examples{
library(GenomicRanges)
data(CTCF_in_10a_narrowPeak_grs)
np_grs = CTCF_in_10a_narrowPeak_grs
olap_gr = ssvOverlapIntervalSets(np_grs)
# annotating with a signle GRanges is OK
ssvAnnotateSubjectGRanges(np_grs$MCF10A_CTCF, olap_gr)
# provide a name if that's useful
ssvAnnotateSubjectGRanges(np_grs$MCF10A_CTCF, olap_gr,
  annotation_name = "MCF10A")
# a named list adds each annotation
ssvAnnotateSubjectGRanges(np_grs, olap_gr)
# overriding list names is an option
ssvAnnotateSubjectGRanges(np_grs, olap_gr, LETTERS[1:3])
# GRangeList are handled like a standard list
ssvAnnotateSubjectGRanges(GRangesList(np_grs), olap_gr, LETTERS[1:3])
}
