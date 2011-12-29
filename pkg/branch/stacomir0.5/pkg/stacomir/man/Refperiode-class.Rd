\name{Refperiode-class}
\Rdversion{1.1}
\docType{class}
\alias{Refperiode-class}
\alias{getvalue,Refperiode-method}

\title{Class "Refperiode" referential class}
\description{	Refperiode referential class to choose a period}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Refperiode", ...)}.
	 ~~ describe objects here ~~ 
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

\examples{
showClass("Refperiode")
new("refperiode")
see .r files for further examples
}
\keyword{classes}
