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
\note{
Opens the connection but does not close it.
This function is intended to be used with stacomiR package, where the error message are collected from the database
It has also been programmed to work without the stacomiR package, as it will test for the existence of envir_stacomi environment.
}
\author{cedric.briand00@gmail.com}

\examples{
showClass("ConnexionODBC")
\dontrun {
 # this is the mother class, you don't have to use it, please use requeteODBC and daughter class instead
 objet<-new("ConnexionODBC")
 objet@baseODBC<-c("myodbcconnexion","myusername","mypassword")
 objet@silent<-FALSE
 objet<-connect(objet)
 odbcClose(objet@connexion)
}
}
\keyword{classes}
