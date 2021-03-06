% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drawSpecPPM.R
\name{drawSpecPPM}
\alias{drawSpecPPM}
\title{Plot NMR spectra from a spectra data matrix}
\usage{
drawSpecPPM(Y.spec, X.ppm, LeftIndex = -1, RightIndex = -1,
  groupFactor = NULL, useLog = FALSE, maxHeight = -1, minHeight = -1,
  nAxisPos = 4, xlab = NULL, ylab = NULL, title = NULL, ticks = NULL,
  ROI = NULL, ROI.ppm = NULL, roiWidth = 100, roiWidth.ppm = NULL,
  legend.extra.x = 2, legend.extra.y = 2, legendpos = NULL,
  colourstyle = "ggplot", manual.colours = NULL, lwd = 1,
  noLegend = FALSE)
}
\arguments{
\item{Y.spec}{(required) The raw spectra in matrix format (1 sample per row) or numeric vector (in case of 1 spectrum)}

\item{X.ppm}{(required) The vector with the ppm values}

\item{LeftIndex}{The starting index of the ppm values for plotting. default = -1 indicates the first ppm (the largest) value is the start of the plot}

\item{RightIndex}{The stopping index for plotting. default = -1 indicates the last ppm value (the smallest) is the end of the plot}

\item{groupFactor}{The groupFactors. If provided different colors will be used for each group.}

\item{useLog}{If set to 'TRUE' the spectra will be log10 transformed (default = FALSE).}

\item{maxHeight}{The maximal height of the plot (default = -1, this indicates no maximal value).}

\item{minHeight}{The minimal height of the plot (default = -1, this indicates no minimal value).}

\item{nAxisPos}{The number of equally spaced tickmarks.}

\item{xlab}{The label on the x axis.}

\item{ylab}{The label on the y axis.}

\item{title}{The title of the plot.}

\item{ticks}{Position tick manually by providing ppm values.}

\item{ROI}{If provided (with an index value, not a ppm value) only this region of interest will be plotted. (supply no ROI or ROI.ppm values, for the full spectrum, or specify only 1, either ROI or ROI.ppm).}

\item{ROI.ppm}{If provided (a ppm value, not an index value) only this region of interest will be plotted. (supply no ROI or ROI.ppm values, for the full spectrum, or specify only 1, either ROI or ROI.ppm).}

\item{roiWidth}{The width of the ROI (region of interest) plot in index points/measurement points. The plot will span from ROI/ROI.ppm - roiWidth to ROI/ROI.ppm + roiWidth. (only supply roiWidth or roiWidth.ppm if needed).}

\item{roiWidth.ppm}{The width of the ROI (region of interest) plot in ppm. The plot will span from ROI/ROI.ppm - roiWidth.ppm to ROI/ROI.ppm + roiWidth.ppm. (only supply roiWidth or roiWidth.ppm if needed).}

\item{legend.extra.x}{Increase (or decrease) the horizontal speace in the legend, this is useful for exporting larger figures.}

\item{legend.extra.y}{Increase (or decrease) the vertical speace in the legend, this is useful for exporting larger figures.}

\item{legendpos}{The position of the legend (standard R legend positioning, default = 'topleft').}

\item{colourstyle}{The colours used in the plot, either standard R or ggplot colours (default).}

\item{manual.colours}{Provide specific colours to be used in the plot.}

\item{lwd}{The linewidth.}

\item{noLegend}{If set to TRUE no legend will be plotted (default = FALSE).}
}
\value{
an R plot
}
\description{
This function plots NMR spectra (so with the largest ppm values on the left) with a number of plotting options
}
\examples{
data(Winedata)
Spectra = Winedata$spectra 
ppm.wine = Winedata$ppm
wine.color = Winedata$wine.color 
drawSpecPPM(Y.spec=Spectra, X.ppm=ppm.wine, groupFactor = wine.color, 
title = 'Raw wine data spectra')


}
\author{
Charlie Beirnaert, \email{charlie.beirnaert@uantwerpen.be}
}

