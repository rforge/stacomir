\name{Refparqual}
\Rdversion{1.1}
\docType{class}
\alias{Refparqual}
\alias{charge,Refparqual-method}
\alias{charge_avec_filtre,Refparqual-method}
\alias{chargecomplement,Refparqual-method}
\alias{choix,Refparqual-method}

\title{Class "Refparqual"}
\description{ Class enabling to load the list of qualitative parameters and to select one of them. It inherits from 'Refpar', uses its 'choix' method}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Refparqual", ...)}.
	 \describe{
    \item{\code{valqual}:}{Object of class \code{"data.frame"} ~ Values of qualitatives parameters}
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ Qualitatives parameters }
  }
}
\section{Slots}{
	 \describe{
    \item{\code{valqual}:}{Object of class \code{"data.frame"} ~ Values of qualitatives parameters}
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ Qualitatives parameters }
  }
}
\section{Extends}{
Class \code{"\linkS4class{Refpar}"}, directly.
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "Refparqual")}: Loads the qualitative parameters }
    \item{charge_avec_filtre}{\code{signature(objet = "Refparqual")}: Loads the qualitative parameters with a filter (quiery has WHERE part...)}
    \item{chargecomplement}{\code{signature(objet = "Refparqual")}: Loads the corresponding values for the selected qualitative parameter}
    \item{choix}{\code{signature(objet = "Refparqual")}: Enables to the user to choose the qualitative parameter}
	 }
}

\author{cedric.briand@lavilaine.com}

\author{cedric.briand@lavilaine.com}
\seealso{
    Other referential classes
	\code{\linkS4class{RefAnnee}}
	\code{\linkS4class{RefCheckBox}}
	\code{\linkS4class{RefChoix}}
	\code{\linkS4class{RefCoe}}		
	\code{\linkS4class{RefDC}}	
	\code{\linkS4class{RefDF}}
	\code{\linkS4class{RefListe}}
	\code{\linkS4class{RefPar}}	
	\code{\linkS4class{Refparqual}}
	\code{\linkS4class{Refparquan}}
	\code{\linkS4class{RefPoidsMoyenPeche}}
	\code{\linkS4class{RefStades}}
	\code{\linkS4class{RefStationMesure}}
	\code{\linkS4class{RefTaxon}}																				 
}
\examples{
showClass("Refparqual")
}
\keyword{classes}
