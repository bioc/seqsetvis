% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_signal_plots.R
\name{add_cluster_annotation}
\alias{add_cluster_annotation}
\title{add_cluster_annotation}
\usage{
add_cluster_annotation(
  cluster_ids,
  p = NULL,
  xleft = 0,
  xright = 1,
  rect_colors = c("black", "gray"),
  text_colors = rev(rect_colors),
  show_labels = TRUE,
  label_angle = 0,
  row_ = "id",
  cluster_ = "cluster_id"
)
}
\arguments{
\item{cluster_ids}{Vector of cluster ids for each item in heatmap. Should be
sorted by plot order for heatmap.}

\item{p}{Optionally an existing ggplot to add annotation to.}

\item{xleft}{left side of cluster annotation rectangles. Default is 0.}

\item{xright}{right side of cluster annotation rectangles. Default is 1.}

\item{rect_colors}{colors of rectangle fill, repeat to match number of
clusters. Default is c("black", "gray").}

\item{text_colors}{colors of text, repeat to match number of clusters.
Default is reverse of rect_colors.}

\item{show_labels}{logical, shoud rectangles be labelled with cluster
identity.  Default is TRUE.}

\item{label_angle}{angle to add clusters labels at.  Default is 0, which is
horizontal.}

\item{row_}{variable name mapped to row, likely id or gene name for ngs data. Default is "id" and works with ssvFetch* outputs.}

\item{cluster_}{variable name to use for cluster info. Default is "cluster_id".}
}
\value{
A ggplot with cluster annotations added.
}
\description{
adds rectangle boxes proportional to cluster sizes of heatmap with optional
labels.
}
\examples{
data(CTCF_in_10a_profiles_dt)
#simplest uses
add_cluster_annotation(factor(c(rep("A", 3), "B")))
p = ggplot() + coord_cartesian(xlim = c(0,10))
add_cluster_annotation(factor(c(rep("A", 3), "B")), p)

#intended use with ssvSignalHeatmap
clust_dt = ssvSignalClustering(CTCF_in_10a_profiles_dt, nclust = 3)
assign_dt = unique(clust_dt[, .(id, cluster_id)])[order(id)]
p_heat = ssvSignalHeatmap(clust_dt, show_cluster_bars = FALSE)
add_cluster_annotation(assign_dt$cluster_id, p_heat,
  xleft = -500, xright = -360, rect_colors = rainbow(3), text_colors = "gray")

#when colors are named, the names are used rather that just the order
rect_colors = safeBrew(assign_dt$cluster_id)
text_colors = safeBrew(assign_dt$cluster_id, "greys")
p_clusters = add_cluster_annotation(assign_dt$cluster_id,
  rect_colors = rect_colors, text_colors = text_colors)
#specialized use as plot outside of heatmap
p1 = assemble_heatmap_cluster_bars(plots = list(p_clusters, p_heat), rel_widths = c(1, 3))

#when colors are named, the names are used rather that just the order
#these plots will be identical even though order of colors changes.
rect_colors = rect_colors[c(2, 3, 1)]
text_colors = text_colors[c(3, 1, 2)]
p_clusters = add_cluster_annotation(assign_dt$cluster_id,
  rect_colors = rect_colors, text_colors = text_colors)
#specialized use as plot outside of heatmap
p2 = assemble_heatmap_cluster_bars(plots = list(p_clusters, p_heat), rel_widths = c(1, 3))

cowplot::plot_grid(p1, p2, ncol = 1)

}
