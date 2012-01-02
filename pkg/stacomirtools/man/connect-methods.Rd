\name{connect-methods}
\docType{methods}
\alias{connect,RequeteODBC-method}
\alias{connect,RequeteODBCwhere-method}
\alias{connect,RequeteODBCwheredate-method}
\title{ Methods for Function connect}
\description{
see individual .r files for help and examples
}
\section{Methods}{
\describe{
\item{\code{signature(objet = "ConnexionODBC")}}{connect an odbc database,and eventually leaves it open for further queries}
\item{\code{signature(objet = "RequeteODBC")}}{ connect an odbc database,performs an sql request}
\item{\code{signature(objet = "RequeteODBCwhere")}}{ connect an odbc database,performs an sql request with where clause}
\item{\code{signature(objet = "RequeteODBCwheredate")}}{connect an odbc database,performs an sql request with where clause for an interval }
}}
\keyword{methods}
\examples{
## 
#objet<-new("RequeteODBCwhere")
#connect(objet)
}

