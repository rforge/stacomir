\name{RequeteODBCwhere-class}
\alias{RequeteODBCwhere-class}
\docType{class}
\title{Class "RequeteODBCwhere"}

\description{SQL Query with WHERE and ORDER BY clauses.}
\note{
  Inherits from RequeteODBC the syntax is where="WHERE ..."
  and =vector("AND...","AND...") order_by="ORDER BY.."
  The query will syntax will be printed upon failure.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RequeteODBCwhere", where=character(),and=vector(),order_by=character())}.
	 \describe{
	\item{\code{select}:}{Object of class \code{"character"}  The "SELECT" part of the query}
    \item{\code{where}:}{Object of class \code{"character"}  The "WHERE" part of the query}
    \item{\code{and}:}{Object of class \code{"vector"}  The "AND" part of the query}
    \item{\code{order_by}:}{Object of class \code{"character"}  The "ORDER BY" part of the query}
    \item{\code{sql}:}{Object of class \code{"character"}  The query built by aggregating "select","where","and", and "order_by" slots}
    \item{\code{query}:}{Object of class \code{"data.frame"}  The result of the query}
    \item{\code{open}:}{Object of class \code{"logical"} Should the connection remain open, choosing this ensures more rapid multiple queries}
	\item{\code{baseODBC}:}{Object of class \code{"vector"}  The name, user and password of the database}
    \item{\code{silent}:}{Object of class \code{"logical"}  TRUE if the query must be executed silently, FALSE else}
    \item{\code{etat}:}{Object of class \code{"character"}  The state of the query (Connecting, successful,...) }
    \item{\code{connection}:}{Object of class \code{"ANY"}  The database connection}
  }
}

\section{Extends}{
Class \code{"\linkS4class{RequeteODBC}"}, directly.
Class \code{"\linkS4class{ConnectionODBC}"}, by class "RequeteODBC", distance 2.
}
\section{Methods}{
  \describe{
    \item{connect}{\code{signature(objet = "RequeteODBCwhere")}: Connect to the database }
	 }
}

\author{cedric.briand"at"eptb-vilaine.fr}

\seealso{
	\code{\linkS4class{ConnectionODBC}}
	\code{\linkS4class{RequeteODBC}}
	\code{\linkS4class{RequeteODBCwheredate}}
}
\examples{
showClass("RequeteODBCwhere")
\dontrun{
 test<-0
 objet=new("RequeteODBCwhere")
 objet@baseODBC=c("myodbcconnection","myusername","mypassword")
 objet@select= "select * from mytable limit 100"
 # assuming mycol, mycol1 and mycol2 are numeric
 objet@where=paste(" where mycol>",test,sep="")
 objet@and=paste(" and mycol2>",test," and mycol3<",test,sep="")
 objet@order_by=" order by mycol1" 
 objet<-connect(objet)
 # now objet@sql contains the syntax of the query. By changing the test variable, one can see how the
 # function might be usefull
 # objet@query contains the resulting data.frame
 }
}
\keyword{classes}
