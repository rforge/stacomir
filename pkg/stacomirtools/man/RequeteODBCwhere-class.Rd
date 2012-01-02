\name{RequeteODBCwhere}
\Rdversion{1.1}
\docType{class}
\alias{RequeteODBCwhere}
\alias{connect,RequeteODBCwhere-method}

\title{Class "RequeteODBCwhere"}
\description{SQL Query with WHERE and ORDER BY clauses.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RequeteODBCwhere", where=character(),and=vector(),order_by=character())}.
	 \describe{
	 \item{\code{where}:}{Object of class \code{"character"} ~ The "WHERE" part of the query}
	 \item{\code{and}:}{Object of class \code{"vector"} ~ The "AND" part of the query}
     \item{\code{order_by}:}{Object of class \code{"character"} ~ The "ORDER BY" part of the query}
	}
}
\section{Slots}{
	 \describe{
    \item{\code{where}:}{Object of class \code{"character"} ~ The "WHERE" part of the query}
    \item{\code{and}:}{Object of class \code{"vector"} ~ The "AND" part of the query}
    \item{\code{order_by}:}{Object of class \code{"character"} ~ The "ORDER BY" part of the query}
    \item{\code{sql}:}{Object of class \code{"character"} ~ The "SELECT *..." part}
    \item{\code{query}:}{Object of class \code{"data.frame"} ~ The result of the query}
	\item{\code{baseODBC}:}{Object of class \code{"vector"} ~ The name, user and password of the database}
    \item{\code{silent}:}{Object of class \code{"logical"} ~ TRUE if the query must be executed silently, FALSE else}
    \item{\code{etat}:}{Object of class \code{"character"} ~ The state of the query (Connecting, successful,...) }
    \item{\code{connexion}:}{Object of class \code{"ANY"} ~ The database connexion}
  }
}
\section{Extends}{
Class \code{"\linkS4class{RequeteODBC}"}, directly.
Class \code{"\linkS4class{ConnexionODBC}"}, by class "RequeteODBC", distance 2.
}
\section{Methods}{
  \describe{
    \item{connect}{\code{signature(objet = "RequeteODBCwhere")}: Connect to the database }
	 }
}

\author{cedric.briand00@gmail.com}

\seealso{
	\code{\linkS4class{ConnexionODBC}}
	\code{\linkS4class{RequeteODBC}}
	\code{\linkS4class{RequeteODBCwheredate}}
}
\examples{
showClass("RequeteODBCwhere")
}
\keyword{classes}
