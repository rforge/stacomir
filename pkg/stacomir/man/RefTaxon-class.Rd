\name{RefTaxon-class}
\Rdversion{1.1}
\docType{class}
\alias{RefTaxon-class}
\alias{RefTaxon}
\title{Class "RefTaxon"}
\description{Races of a fish, and enables to select one of them}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RefTaxon", ...)}.
	 \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ Races available in the database}
  }
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ Races available in the database}
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "RefTaxon")}: Loads races in the database}
    \item{charge_avec_filtre}{\code{signature(objet = "RefTaxon")}: Loads the races wich are concerned by a device (control or clearing) }
    \item{choice}{\code{signature(objet = "RefTaxon")}: Enables to select one of the available fish races}
	 }
}
\author{cedric.briand"at"eptb-vilaine.fr}
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
showClass("RefTaxon")
}
\keyword{classes}
