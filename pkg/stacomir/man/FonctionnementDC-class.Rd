\name{FonctionnementDC}
\Rdversion{1.1}
\docType{class}
\alias{FonctionnementDC}
\alias{charge,FonctionnementDC-method}
\alias{connect,FonctionnementDC-method}

\title{Class "FonctionnementDC"}
\description{ Overview of the active/inactive working period for a control device}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("FonctionnementDC", data="data.frame",dc="RefDC",horodate="Horodate",requete="RequeteODBCwheredate")}.
	\describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ Datas concerning working period of a control device }
    \item{\code{dc}:}{Object of class \code{"RefDC"} ~ The control device }
    \item{\code{horodate}:}{Object of class \code{"Horodate"} ~ The horodate of the balance }
    \item{\code{requete}:}{Object of class \code{"RequeteODBCwheredate"} ~ The ODBC query }
  }
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ Datas concerning working period of a control device }
    \item{\code{dc}:}{Object of class \code{"RefDC"} ~ The control device }
    \item{\code{horodate}:}{Object of class \code{"Horodate"} ~ The horodate of the balance }
    \item{\code{requete}:}{Object of class \code{"RequeteODBCwheredate"} ~ The ODBC query }
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "FonctionnementDC")}: Loads the informations in the environment and execute the 'connect' function }
    \item{connect}{\code{signature(objet = "FonctionnementDC")}: Execute the appropriate query to load data from the control device}
	 }
}

\author{cedric.briand@lavilaine.com}


\seealso{
	\code{\link{FonctionnementDF}}
}
\examples{
showClass("FonctionnementDC")
}
\keyword{classes}
