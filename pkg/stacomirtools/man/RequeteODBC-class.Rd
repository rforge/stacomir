\name{RequeteODBC-class}
\Rdversion{1.1}
\docType{class}
\alias{RequeteODBC}
\alias{connect,RequeteODBC-method}

\title{Class "RequeteODBC"}
\description{ODBC Query. This class enables to retrieve data from the database. 
This class is inherited by RequeteODBCwhere and RequeteODBCwheredate
}
\note{
  Inherits from ConnexionODBC
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RequeteODBC", sql=character(), query=data.frame())}.
	 \describe{
    \item{\code{sql}:}{Object of class \code{"character"} ~ The query}
    \item{\code{query}:}{Object of class \code{"data.frame"} ~ The result of the query}
	}
}
\section{Slots}{
	 \describe{
    \item{\code{sql}:}{Object of class \code{"character"} ~ The "SELECT ..." part of the query}
    \item{\code{query}:}{Object of class \code{"data.frame"} ~ The result of the query}
    \item{\code{baseODBC}:}{Object of class \code{"vector"} ~ The name, user and password of the database}
    \item{\code{silent}:}{Object of class \code{"logical"} ~ True if the query must be executed silently, FALSE}
    \item{\code{etat}:}{Object of class \code{"character"} ~ The state of the query (Connecting, successful,...) }
    \item{\code{connexion}:}{Object of class \code{"ANY"} ~ The connexion}
  }
}
\section{Extends}{
Class \code{"\linkS4class{ConnexionODBC}"}, directly.
}
\section{Methods}{
  \describe{
    \item{connect}{\code{signature(objet = "RequeteODBC")}: Connexion to the database }
	 }
}

\author{cedric.briand00@gmail.com}

\seealso{
	\code{\linkS4class{ConnexionODBC}}
	\code{\linkS4class{RequeteODBCwhere}}
	\code{\linkS4class{RequeteODBCwheredate}}
}
\examples{
showClass("RequeteODBC")
}
\keyword{classes}
