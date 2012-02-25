\name{RefDC-class}
\Rdversion{1.1}
\docType{class}
\alias{RefDC-class}
\alias{RefDC}

\title{Class "RefDC"}
\description{Description of a control device.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RefDC", dc_selectionne=integer(), ouvrage=integer(), data=data.frame())}.
	 \describe{
    \item{\code{dc_selectionne}:}{Object of class \code{"integer"}The selected device}
    \item{\code{ouvrage}:}{Object of class \code{"integer"}  The attached dam}
    \item{\code{data}:}{Object of class \code{"data.frame"}  Datas concerning the control device}
  }
}

\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "RefDC")}: Loads informations about the control device }
    \item{choix}{\code{signature(objet = "RefDC")}: Enables to the user to selected the device }
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
showClass("RefDC")
}
\keyword{classes}
