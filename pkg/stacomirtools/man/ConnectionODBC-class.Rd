\name{ConnectionODBC-class}
\docType{class}
\alias{ConnectionODBC-class}
\alias{ConnectionODBC}

\title{Class "ConnectionODBC"}
\description{Mother class for connection, opens the connection but does not shut it}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ConnectionODBC", ...)}.
	 \describe{
    \item{\code{baseODBC}:}{Object of class \code{"vector"} The database }
    \item{\code{silent}:}{Object of class \code{"logical"} The mode }
    \item{\code{etat}:}{Object of class \code{"character"} The state }
    \item{\code{connection}:}{Object of class \code{"ANY"} The connection }
  }
}
\section{Slots}{
	 \describe{
    \item{\code{baseODBC}:}{Object of class \code{"vector"} The database }
    \item{\code{silent}:}{Object of class \code{"logical"} The mode }
    \item{\code{etat}:}{Object of class \code{"character"} The state }
    \item{\code{connection}:}{Object of class \code{"ANY"} The connection }
  }
}
\section{Methods}{
  \describe{
    \item{connect}{\code{signature(objet = "ConnectionODBC")}: Connection to the database }
	 }
}
\note{
Opens the connection but does not close it.
This function is intended to be used with stacomiR package, where the error message are collected from the database
It has also been programmed to work without the stacomiR package, as it will test for the existence of envir_stacomi environment.
}
\author{cedric.briand"at"eptb-vilaine.fr}

\examples{
showClass("ConnectionODBC")
\dontrun{
 # this is the mother class, you don't have to use it, please use requeteODBC and daughter class instead
 objet<-new("ConnectionODBC")
 objet@baseODBC<-c("myODBCconnection","myusername","mypassword")
 objet@silent<-FALSE
 objet<-connect(objet)
 odbcClose(objet@connection)
}
}
\keyword{classes}
