% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_rsm.R
\name{gg_rsm}
\alias{gg_rsm}
\title{Contour plot(s) of a fitted linear model in ggplot2}
\usage{
gg_rsm(
  rsm_model,
  form,
  filled = FALSE,
  decode = FALSE,
  n_columns = 2,
  stroke = 0.15,
  size = 4,
  ...
)
}
\arguments{
\item{rsm_model}{Model of class "rsm"}

\item{form}{A formula, or a list of formulas}

\item{filled}{Determine if the surface plots should be filled by viridis color palette. Default is FALSE}

\item{decode}{This has an effect only if x is an rsm object or other model object that supports coded.data.
In such cases, if decode is TRUE, the coordinate axes are transformed to their decoded values.}

\item{n_columns}{number of columns for grid layout. Default is 2}

\item{stroke}{width of stroke relative to the size of the text. Ignored if less than zero. Only applied if contour plots are filled}

\item{size}{size of text for contour lines. Only applied if contour plots are filled}

\item{...}{Other arguments passed on to contour(). For help with more arguments see ?rsm::contour.lm}
}
\value{
A grid of contour plot(s) of a fitted linear model in 'ggplot2'
}
\description{
Contour plot(s) of a fitted linear model in ggplot2
}
\examples{
heli.rsm <- rsm::rsm(ave ~ SO(x1, x2, x3, x4),
                     data = rsm::heli)

gg_rsm(heli.rsm,form = ~x1+x3+x4,at = rsm::xs(heli.rsm),n_columns=3)
gg_rsm(heli.rsm,form = ~x2+x3+x4,at = rsm::xs(heli.rsm),n_columns=3,filled = TRUE)
}
