\name{PasDeTempsJournalier-class}
\Rdversion{1.1}
\docType{class}
\alias{PasDeTempsJournalier}
\alias{PasDeTempsJournalier-class}
\alias{choix,PasDeTempsJournalier-method}

\title{Class "PasDeTempsJournalier" }
\description{ Representation of a PasDeTemps object with a step length equal to one day. It receives an heritance from PasDeTemps }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PasDeTempsJournalier", dateDebut="POSIXt",dureePas=numeric(),nbPas=numeric(),noPasCourant=integer())}.
 \describe{
    \item{\code{dateDebut}:}{Object of class \code{"POSIXt"} ~ Starting date }
    \item{\code{dureePas}:}{Object of class \code{"numeric"} ~ Step length }
    \item{\code{nbPas}:}{Object of class \code{"numeric"} ~ Number of steps }
    \item{\code{noPasCourant}:}{Object of class \code{"integer"} ~ Number of the current step }
  }
}

\section{Slots}{
	 \describe{
    \item{\code{dateDebut}:}{Object of class \code{"POSIXt"} ~ Starting date }
    \item{\code{dureePas}:}{Object of class \code{"numeric"} ~ Step length }
    \item{\code{nbPas}:}{Object of class \code{"numeric"} ~ Number of steps }
    \item{\code{noPasCourant}:}{Object of class \code{"integer"} ~ Number of the current step }
  }
}
\section{Extends}{
Class \code{"\linkS4class{PasDeTemps}"}, directly.
}
\section{Methods}{
  \describe{
    \item{choix}{\code{signature(objet = "PasDeTempsJournalier")}: Enables to initialize a PasDeTempsJournalier }
	 }
}

\author{cedric.briand00@gmail.com}

\seealso{
	\code{\linkS4class{PasDeTemps}}
}
\examples{
showClass("PasDeTempsJournalier")
}
\keyword{classes}
