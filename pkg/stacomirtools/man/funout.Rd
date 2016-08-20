\name{funout}
\alias{funout}
\title{Function loaded in this package to avoid errors, if the package is called without stacomiR}
\usage{
  funout(text,arret=FALSE,wash=FALSE)
}
\arguments{
  \item{text}{The text to display}
  \item{arret}{If true calls the program to stop and the message to be displayed}
  \item{wash}{Only used when called from within stacomiR, and there is a widget interface, kept there for consistency }
}
\description{
  This function will be replaced by a longer function using gWidgets if the package stacomiR is loaded. It
  is provided there to avoid to pointing to an undefined global function. Normally the program tests for the existence of
  and environment envir_stacomi which indicates that the messages are to be displayed in the gWidget interface, so this code 
  is to avoid notes in R.check.
}
\author{
  Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
}

