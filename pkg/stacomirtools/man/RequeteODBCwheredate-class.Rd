\name{RequeteODBCwheredate-class}
\Rdversion{1.1}
\docType{class}
\alias{RequeteODBCwheredate}
\alias{connect,RequeteODBCwheredate-method}

\title{Class "RequeteODBCwheredate"}
\description{Query with WHERE condition and overlaping dates clause.}
\note{
  Inherits from RequeteODBCwhere and uses its connect
  method with a new SetAs
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RequeteODBCwheredate", datedebut="POSIXlt",datefin="POSIXlt",colonnedebut="character",colonnefin="character")}.
	 \describe{
    \item{\code{datedebut}:}{Object of class \code{"POSIXlt"} ~ The starting date}
    \item{\code{datefin}:}{Object of class \code{"POSIXlt"} ~ The ending date}
    \item{\code{colonnedebut}:}{Object of class \code{"character"} ~ The name begin column}
    \item{\code{colonnefin}:}{Object of class \code{"character"} ~ The name end column}
	}	
}
\section{Slots}{
	 \describe{
    \item{\code{datedebut}:}{Object of class \code{"POSIXlt"} ~ The starting date}
    \item{\code{datefin}:}{Object of class \code{"POSIXlt"} ~ The ending date}
    \item{\code{colonnedebut}:}{Object of class \code{"character"} ~ The name of the begin column}
    \item{\code{colonnefin}:}{Object of class \code{"character"} ~ The name of the end column}
    \item{\code{where}:}{Object of class \code{"character"} ~ The WHERE clause}
    \item{\code{and}:}{Object of class \code{"vector"} ~ The AND clause}
    \item{\code{order_by}:}{Object of class \code{"character"} ~ The ORDER BY clause}
    \item{\code{sql}:}{Object of class \code{"character"} ~ The SELECT clause}
    \item{\code{query}:}{Object of class \code{"data.frame"} ~ The result of the query}
    \item{\code{baseODBC}:}{Object of class \code{"vector"} ~ The database}
    \item{\code{silent}:}{Object of class \code{"logical"} ~ The mode}
    \item{\code{etat}:}{Object of class \code{"character"} ~ The state}
    \item{\code{connexion}:}{Object of class \code{"ANY"} ~ The connexion}
  }
}
\section{Extends}{
Class \code{"\linkS4class{RequeteODBCwhere}"}, directly.
Class \code{"\linkS4class{RequeteODBC}"}, by class "RequeteODBCwhere", distance 2.
Class \code{"\linkS4class{ConnexionODBC}"}, by class "RequeteODBCwhere", distance 3.
}
\section{Methods}{
  \describe{
    \item{connect}{\code{signature(objet = "RequeteODBCwheredate")}: Connexion to the database}
	 }
}

\author{cedric.briand00@gmail.com}

\seealso{
	\code{\linkS4class{ConnexionODBC}}
	\code{\linkS4class{RequeteODBC}}
	\code{\linkS4class{RequeteODBCwhere}}
}
\examples{
showClass("RequeteODBCwheredate")
}

