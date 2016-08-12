\name{connect-methods}
\alias{connect-methods}
\alias{connect}
\docType{methods}
\alias{connect,ConnectionODBC-method}
\alias{connect,RequeteODBC-method}
\alias{connect,RequeteODBCwhere-method}
\alias{connect,RequeteODBCwheredate-method}

\title{ Methods for Function connect}
\description{
see individual .r files for help and examples
}
\section{Methods}{
\describe{
\item{\code{signature(object = "ConnectionODBC")}}{connect an ODBC database,and eventually leaves it open for further queries,
the connection may send message in the native language if stacomiR package is in use}
\item{\code{signature(object = "RequeteODBC")}}{ connect an ODBC database,performs an sql request}
\item{\code{signature(object = "RequeteODBCwhere")}}{ connect an ODBC database,performs an sql request with where clause}
\item{\code{signature(object = "RequeteODBCwheredate")}}{connect an ODBC database,performs an sql request with where clause for an interval }
}}
\keyword{methods}
\examples{
 showMethods("connect")
## 
#object<-new("RequeteODBCwhere")
#connect(object)
}

