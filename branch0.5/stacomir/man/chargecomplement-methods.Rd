\name{chargecomplement-methods}
\docType{methods}
\alias{chargecomplement}
\alias{chargecomplement-methods}
\alias{chargecomplement,Refparqual-method}
\title{ Methods for Function chargecomplement}
\description{
Methods for function \code{chargecomplement} method chargecomplement
this method is called after selecting the object (data only counts one line)
and allows a request to obtain a complement, for instance possible values for a qualitative parameter
}
\section{Methods}{
\describe{

\item{\code{signature(objet = "Refparqual")}}{collexts possible values for a qualitative parameter, for instance for parameter sex would be values male & female }
}}
\examples{
\dontrun{
dc_selectionne=6
taxon_selectionne=2038
stade_selectionne="AGJ"
objet=new("Refparqual")
objet<-charge(objet)
chargecomplement(objet)
}
}
\keyword{methods}
\keyword{database}
