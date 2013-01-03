\name{Refperiode-class}
\docType{class}
\alias{Refperiode-class}
\alias{Refperiode}

\title{Class "Refperiode" referential class}
\description{	Refperiode referential class to choose a period}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Refperiode", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} providing correspondance between period and their english names }
  }
}
\section{Methods}{
  \describe{
    \item{getvalue}{\code{signature(objet = "Refperiode")}: ... }
	 }
}
\note{
  pgval are used by seq.POSIXT
}
\examples{
showClass("Refperiode")
new("Refperiode")
}
\keyword{classes}
