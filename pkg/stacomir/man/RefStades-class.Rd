\name{RefStades-class}
\docType{class}
\alias{RefStades-class}
\alias{RefStades}
\title{Class "RefStades"}
\description{Representation of a fish phase}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RefStades", data="data.frame")}.
	\describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ The phases available in the database}
  }
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ The phases available in the database}
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "RefStades")}: Loads the phases in the database}
    \item{charge_avec_filtre}{\code{signature(objet = "RefStades")}: Loads the phases in the database. The filter is the control device}
    \item{choix}{\code{signature(objet = "RefStades")}: Enables to the user to select the phase}
	 }
}
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
\author{cedric.briand00@gmail.com}

\examples{
showClass("RefStades")
}
\keyword{classes}
