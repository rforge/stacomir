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

  \item{nom_coldt}{the name of the column containing date or POSIXt entry to be processed}

  \item{annee}{logical do you want a column describing year to be added to the dataframe}

  \item{mois}{logical, add column with month}

  \item{quinzaine}{logical, add column with 15 days}

  \item{semaine}{logical, add column with weeks}

  \item{jour_an}{logical, add column with day of year}

  \item{jour_mois}{logical, add column with day of month}

  \item{heure}{logical, add column with hour}
}
\value{
  data
}
\description{
  This function extracts temporal characteristics from a  dataframe
}
\author{
  Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
}