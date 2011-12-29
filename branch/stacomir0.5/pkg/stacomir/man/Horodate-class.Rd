\name{Horodate}
\Rdversion{1.1}
\docType{class}
\alias{Horodate}
\alias{choix,Horodate-method}
\alias{getanneeprec,Horodate-method}
\alias{getHorodate,Horodate-method}
\alias{setHorodate,Horodate-method}

\title{Class "Horodate"}
\description{ Horodate }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Horodate")}
}
\section{Slots}{
	 \describe{
    \item{\code{horodate}:}{Object of class \code{"POSIXt"} ~ The date}
  }
}
\section{Methods}{
  \describe{
    \item{choix}{\code{signature(objet = "Horodate")}: Enable to initialize an horodate}
    \item{getanneeprec}{\code{signature(objet = "Horodate")}: Returns the previous year}
    \item{getHorodate}{\code{signature(objet = "Horodate")}: Returns the following year}
    \item{setHorodate}{\code{signature(objet = "Horodate")}: Initialize an horodate}
	 }
}

\author{cedric.briand@lavilaine.com}

\examples{
showClass("Horodate")
}
\keyword{classes}
