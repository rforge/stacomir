% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_ge_weight.R
\docType{methods}
\name{connect,report_ge_weight-method}
\alias{connect,report_ge_weight-method}
\alias{connect.report_ge_weight}
\title{connect method for report_Poids_moyen}
\usage{
\S4method{connect}{report_ge_weight}(object)
}
\arguments{
\item{object}{An object of class \link{report_ge_weight-class}}
}
\value{
report_Poids_Moyen request corresponding to user choices, mean weight
 w is calculated as car_valeur_quantitatif/lot_effectif. These coefficients are stored in the database,
and the connect method loads them from the table using the \link{ref_coe-class}
}
\description{
connect method for report_Poids_moyen
}
\note{
dates for the request are from august to august (a glass eel season)
}
\author{
Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
}
