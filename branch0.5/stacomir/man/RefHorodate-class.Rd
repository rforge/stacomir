\name{RefHorodate-class}
\docType{class}
\alias{RefHorodate-class}
\alias{RefHorodate}
\alias{validite_RefHorodate}
\alias{getanneeprec,RefHorodate-method}
\alias{getanneeprec}
\alias{getRefHorodate,RefHorodate-method}
\alias{getRefHorodate}
\alias{setRefHorodate,RefHorodate-method}
\alias{setRefHorodate}
\title{Class "RefHorodate"}
\description{	choice of date with method to show current and previous year
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RefHorodate", \dots)}.
	  
}
\section{Slots}{
  \describe{
    \item{\code{horodate}:}{Object of class \code{"POSIXt"} }
  }
}
\section{Methods}{
  \describe{
    \item{choix}{\code{signature(objet = "RefHorodate")}: gui choice }
    \item{getanneeprec}{\code{signature(objet = "RefHorodate")}: get previous year}
    \item{getRefHorodate}{\code{signature(objet = "RefHorodate")}: get method for horodate }
    \item{setRefHorodate}{\code{signature(objet = "RefHorodate")}: set method for horodate }
	 }
}

\author{Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}}

\seealso{
    Other referential classes
	\code{\linkS4class{RefAnnee}}
	\code{\linkS4class{RefCheckBox}}
	\code{\linkS4class{RefChoix}}
	\code{\linkS4class{RefCoe}}		
	\code{\linkS4class{RefDC}}	
	\code{\linkS4class{RefDF}}
	\code{\linkS4class{RefListe}}
	\code{\linkS4class{Refpar}}	
	\code{\linkS4class{Refparqual}}
	\code{\linkS4class{Refparquan}}
	\code{\linkS4class{RefPoidsMoyenPeche}}
	\code{\linkS4class{RefStades}}
	\code{\linkS4class{RefStationMesure}}
	\code{\linkS4class{RefTaxon}}																				 
}
\examples{
showClass("RefHorodate")
}
\keyword{classes}
