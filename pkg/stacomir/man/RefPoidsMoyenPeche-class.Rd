\name{RefPoidsMoyenPeche}
\Rdversion{1.1}
\docType{class}
\alias{RefPoidsMoyenPeche}

\title{Class "RefPoidsMoyenPeche"}
\description{Class which enables to load bunches of elver of experimental fishings}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RefPoidsMoyenPeche", data=data.frame(),datedebut="POSIXlt",datefin="POSIXlt")}.
	 \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ Datas}
    \item{\code{datedebut}:}{Object of class \code{"POSIXlt"} ~ Starting year}
    \item{\code{datefin}:}{Object of class \code{"POSIXlt"} ~ Ending year}
  }
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ Datas}
    \item{\code{datedebut}:}{Object of class \code{"POSIXlt"} ~ Starting year}
    \item{\code{datefin}:}{Object of class \code{"POSIXlt"} ~ Ending year}
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "RefPoidsMoyenPeche")}: Exectutes the appropriate query to lot datas in the database }
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
showClass("RefPoidsMoyenPeche")
}
\keyword{classes}
