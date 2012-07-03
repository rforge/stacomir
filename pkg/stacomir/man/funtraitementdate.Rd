\name{funtraitementdate}
\alias{funtraitementdate}
\title{This function extracts temporal characteristics from a dataframe}
\usage{
  funtraitementdate(data, nom_coldt, annee = TRUE,
    mois = TRUE, quinzaine = FALSE, semaine = TRUE,
    jour_an = FALSE, jour_mois = TRUE, heure = FALSE)
}
\arguments{
  \item{data}{a data frame containing a Date or POSIXt
  column}

  \item{nom_coldt}{}

  \item{annee}{}

  \item{mois}{}

  \item{quinzaine}{}

  \item{semaine}{}

  \item{jour_an}{}

  \item{jour_mois}{}

  \item{heure}{}
}
\value{
  data
}
\description{
  This function extracts temporal characteristics from a
  dataframe
}
\author{
  Cedric Briand \email{cedric.briand00@gmail.com}
}