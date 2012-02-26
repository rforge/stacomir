\name{PasDeTempsChar-class}
\docType{class}
\alias{PasDeTempsChar}
\alias{PasDeTempsChar-class}
\alias{coerce,PasDeTempsChar,PasDeTemps-method}
\alias{validite_PasDeTempsChar}

\title{Class "PasDeTempsChar"}
\description{ Character to represent a PasDeTemps}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PasDeTempsChar", ...)}
}
\section{Slots}{
	 \describe{
    \item{\code{dateDebut}:}{Object of class \code{"POSIXt"} ~ The starting date }
    \item{\code{dureePas}:}{Object of class \code{"character"} ~ The length of a time step }
    \item{\code{nbPas}:}{Object of class \code{"numeric"} ~ The number of time step }
    \item{\code{noPasCourant}:}{Object of class \code{"integer"} ~ The current number of the step }
  }
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "PasDeTempsChar", to = "PasDeTemps")}: ... }
	 }
}

\author{cedric.briand00@gmail.com}

\seealso{
  \code{\linkS4class{PasDeTemps}}
}
\examples{
showClass("PasDeTempsChar")
}
\keyword{classes}
