% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function.test.R
\name{function.test}
\alias{function.test}
\title{Function.test}
\usage{
function.test(x, y, sample = "monthly", station, origin_x = NULL,
  origin_y = NULL, valmin = NA, ...)
}
\arguments{
\item{x, y}{x,y}

\item{sample}{charchter string. Defaul is \code{"monthly"}}

\item{station}{weather station code}

\item{origin_x, origin_y}{origins for \code{x,y}}

\item{valmin}{minimum value admitted. Default is \code{NA}. It is used in case of precipitation amount.}

\item{...}{further arguments for tests}
}
\description{
This function is dynamically written in R to test RMAWGEN or RGENERATE or RGENERATEPREC outputs!!!
}

