% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heatmap.R
\name{plotHeatmapSRD}
\alias{plotHeatmapSRD}
\title{plotHeatmapSRD}
\usage{
plotHeatmapSRD(df)
}
\arguments{
\item{df}{A DataFrame.}
}
\value{
Returns a heatmap.
}
\description{
Heatmap is generated based on the pairwise distance - measured in SRD - of the columns. 
Each column is set as reference once, then SRD values are calculated for the other columns.
}
\examples{
srdInput <- data.frame(
A=c(32, 52, 44, 44, 47),
B=c(73, 75, 65, 76, 70),
C=c(60, 59, 57, 55, 60),
D=c(35, 24, 44, 83, 47),
E=c(41, 52, 46, 50, 65))

plotHeatmapSRD(srdInput)
}
\author{
Attila Gere \email{gereattilaphd@gmail.com}, Linus Olsson \email{linusmeol@gmail.com}
}
