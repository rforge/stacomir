\name{RefStationMesure-class}
\docType{class}
\alias{RefStationMesure-class}
\alias{RefStationMesure}
\title{Class "RefStationMesure"}
\description{Enables to load measure stations and to select one of them}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RefStationMesure", ...)}.
	\describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ Data concerning the measure station}
	}
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ Data concerning the measure station}
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "RefStationMesure")}: Loads the measure stations }
    \item{choix}{\code{signature(objet = "RefStationMesure")}: Enables to select one in the list }
	 }
}

\author{cedric.briand00@gmail.com}
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
showClass("RefStationMesure")
}
\keyword{classes}
