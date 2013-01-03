\name{PasDeTemps-class}
\docType{class}
\alias{PasDeTemps}
\alias{PasDeTemps-class}
\alias{currentDateDebut,PasDeTemps-method}
\alias{currentDateDebut}
\alias{currentDateFin,PasDeTemps-method}
\alias{currentDateFin}
\alias{DateFin,PasDeTemps-method}
\alias{DateFin}
\alias{LesPasDeTemps}
\alias{ValeurPasDeTemps}
\alias{getAnnees,PasDeTemps-method}
\alias{getAnnees}
\alias{getdateDebut,PasDeTemps-method}
\alias{getdateDebut}
\alias{getLibellesPas,PasDeTemps-method}
\alias{getLibellesPas}
\alias{getnoPasCourant,PasDeTemps-method}
\alias{getnoPasCourant}
\alias{setdateDebut,PasDeTemps-method}
\alias{setdateDebut}
\alias{suivant,PasDeTemps-method}
\alias{suivant}
\alias{validite_PasDeTemps}
\alias{dureePas}
\title{Class "PasDeTemps"}
\description{Describes a time step}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PasDeTemps", dateDebut="POSIXt",dureePas=numeric(),nbPas=numeric(),noPasCourant=integer())}.
 \describe{
    \item{\code{dateDebut}:}{Object of class \code{"POSIXt"}  Starting date }
    \item{\code{dureePas}:}{Object of class \code{"numeric"}  Step length }
    \item{\code{nbPas}:}{Object of class \code{"numeric"}  Number of steps }
    \item{\code{noPasCourant}:}{Object of class \code{"integer"}  Number of the current step }
  }
}

\section{Slots}{
	 \describe{
    \item{\code{dateDebut}:}{Object of class \code{"POSIXt"}  Starting date }
    \item{\code{dureePas}:}{Object of class \code{"numeric"}  Step length }
    \item{\code{nbPas}:}{Object of class \code{"numeric"} Number of steps }
    \item{\code{noPasCourant}:}{Object of class \code{"integer"}  Number of the current step }
  }
}
\section{Methods}{
  \describe{
    \item{choix}{\code{signature(objet = "PasDeTemps")}: Enables to initialize a PasDeTemps object}
    \item{currentDateDebut}{\code{signature(objet = "PasDeTemps")}: Returns the current starting date}
    \item{currentDateFin}{\code{signature(objet = "PasDeTemps")}: Returns the current ending date}
    \item{DateFin}{\code{signature(objet = "PasDeTemps")}: Returns the ending date}
    \item{getAnnees}{\code{signature(objet = "PasDeTemps")}: Returns years in the PasDeTemps}
    \item{getdateDebut}{\code{signature(objet = "PasDeTemps")}: Returns the starting date}
    \item{getLibellesPas}{\code{signature(objet = "PasDeTemps")}: Returns the steps labels}
    \item{getnoPasCourant}{\code{signature(objet = "PasDeTemps")}: Returns the number of the current step}
    \item{setdateDebut}{\code{signature(objet = "PasDeTemps")}: Set the starting date}
    \item{suivant}{\code{signature(objet = "PasDeTemps")}: Goes to the next step}
	 }
}

\author{cedric.briand00@gmail.com}

\seealso{
	\code{\linkS4class{PasDeTempsJournalier}}
}

\examples{
showClass("PasDeTemps")
}
\keyword{classes}
