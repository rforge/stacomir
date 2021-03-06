% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_ge_weight.R
\docType{methods}
\name{choice_c,report_ge_weight-method}
\alias{choice_c,report_ge_weight-method}
\alias{choice_c.report_ge_weight}
\title{command line interface for \link{report_ge_weight-class}}
\usage{
\S4method{choice_c}{report_ge_weight}(object, dc, anneedebut, anneefin,
  selectedvalue, silent = FALSE)
}
\arguments{
\item{object}{An object of class \link{report_ge_weight-class}}

\item{dc}{A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,ref_dc-method}}

\item{anneedebut}{The starting the first year, passed as charcter or integer}

\item{anneefin}{the finishing year, must be > anneedebut (minimum one year in august to the next in august)}

\item{selectedvalue}{A character to select and object in the \link{ref_list-class}}

\item{silent}{Boolean, if TRUE, information messages are not displayed}
}
\value{
An object of class \link{report_ge_weight-class}
The choice_c method fills in the data slot for classes \link{ref_dc-class} \link{ref_year-class}
\link{ref_coe-class} \link{ref_list-class}
}
\description{
command line interface for \link{report_ge_weight-class}
}
\author{
Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
}
