\name{ConnexionODBC-class}
\Rdversion{1.1}
\docType{class}
\alias{ConnexionODBC-class}
\alias{connect,ConnexionODBC-method}
\alias{ConnexionODBC}

\title{Class "ConnexionODBC"}
\description{Mother class for connection, opens the connection but does not shut it}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ConnexionODBC", ...)}.
	 \describe{
    \item{\code{baseODBC}:}{Object of class \code{"vector"} The database }
    \item{\code{silent}:}{Object of class \code{"logical"} The mode }
    \item{\code{etat}:}{Object of class \code{"character"} The state }
    \item{\code{connexion}:}{Object of class \code{"ANY"} The connection }
  }
}
\section{Slots}{
	 \describe{
    \item{\code{baseODBC}:}{Object of class \code{"vector"} The database }
    \item{\code{silent}:}{Object of class \code{"logical"} The mode }
    \item{\code{etat}:}{Object of class \code{"character"} The state }
    \item{\code{connexion}:}{Object of class \code{"ANY"} The connection }
  }
}
\section{Methods}{
  \describe{
    \item{connect}{\code{signature(objet = "ConnexionODBC")}: Connection to the database }
	 }
}

\author{cedric.briand@lavilaine.com}

\examples{
showClass("ConnexionODBC")
}
\keyword{classes}
