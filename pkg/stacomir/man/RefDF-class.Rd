\name{RefDF}
\Rdversion{1.1}
\docType{class}
\alias{RefDF}
\alias{charge,RefDF-method}
\alias{choix,RefDF-method}

\title{Class "RefDF"}
\description{Representation of a fishway,  Contains description data of all fishways from the database
  along with the selected fishway(df) (integer)}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RefDF", df_selectionne=integer(), ouvrage=integer(), data=data.frame())}.
	 \describe{
    \item{\code{df_selectionne}:}{Object of class \code{"integer"} The identifier of the fishway}
    \item{\code{ouvrage}:}{Object of class \code{"integer"} The attached dam}
    \item{\code{data}:}{Object of class \code{"data.frame"} Data concerning the fishway}
  }
}
\section{Slots}{
	 \describe{
    \item{\code{df_selectionne}:}{Object of class \code{"integer"}  The identifier of the fishway}
    \item{\code{ouvrage}:}{Object of class \code{"integer"}  The attached dam}
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ Data concerning the fishway work}
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "RefDF")}: Loads informations about the device}
    \item{choix}{\code{signature(objet = "RefDF")}: Enables the user to select a device}
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
	\code{\linkS4class{RefPar}}	
	\code{\linkS4class{Refparqual}}
	\code{\linkS4class{Refparquan}}
	\code{\linkS4class{RefPoidsMoyenPeche}}
	\code{\linkS4class{RefStades}}
	\code{\linkS4class{RefStationMesure}}
	\code{\linkS4class{RefTaxon}}																				 
}
\examples{
showClass("RefDF")
}
\keyword{classes}
