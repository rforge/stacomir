\name{RefCoe}
\Rdversion{1.1}
\docType{class}
\alias{RefCoe}
\alias{charge,RefCoe-method}

\title{Class "RefCoe"}
\description{	Enables to load conversion coefficients quantity-number}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RefCoe")}.
	 
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ Datas}
    \item{\code{datedebut}:}{Object of class \code{"POSIXlt"} ~ Starting date}
    \item{\code{datefin}:}{Object of class \code{"POSIXlt"} ~ Ending date}
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "RefCoe")}: Enables to load conversion coefficient in the database }
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
showClass("RefCoe")
}
\keyword{classes}
