\name{FonctionnementDF}
\Rdversion{1.1}
\docType{class}
\alias{FonctionnementDF}
\alias{charge,FonctionnementDF-method}
\alias{connect,FonctionnementDF-method}

\title{Class "FonctionnementDF"}
\description{ Overview of the active/inactive period of working for a clearing device}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("FonctionnementDF", data="data.frame",df="RefDF",horodate="Horodate",requete="RequeteODBCwheredate")}.
\describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ Datas concerning working period of a control device }
    \item{\code{df}:}{Object of class \code{"RefDF"} ~ The clearing device }
    \item{\code{horodate}:}{Object of class \code{"Horodate"} ~ The horodate of the balance }
    \item{\code{requete}:}{Object of class \code{"RequeteODBCwheredate"} ~ The ODBC query }
  } 
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ Datas concerning working period of a control device }
    \item{\code{df}:}{Object of class \code{"RefDF"} ~ The clearing device}
    \item{\code{horodate}:}{Object of class \code{"Horodate"} ~ The horodate of the balance }
    \item{\code{requete}:}{Object of class \code{"RequeteODBCwheredate"} ~ The ODBC query }
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "FonctionnementDF")}: Loads the informations in the environment and execute the 'connect' function }
    \item{connect}{\code{signature(objet = "FonctionnementDF")}: Execute the appropriate query to load data from the clearing device}
	 }
}

\author{cedric.briand@lavilaine.com }

\seealso{
	\code{\link{FonctionnementDC}}
}
\examples{
showClass("FonctionnementDF")
}
\keyword{classes}
