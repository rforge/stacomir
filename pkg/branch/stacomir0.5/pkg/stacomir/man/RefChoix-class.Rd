\name{RefChoix}
\Rdversion{1.1}
\docType{class}
\alias{RefChoix}
\alias{charge,RefChoix-method}
\alias{choix,RefChoix-method}

\title{Class "RefChoix"}
\description{RefChoix referential class allows to choose within several values with radiobuttons}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RefChoix", listechoix=character() ,label=character() ,selected=integer())}.
}
\section{Slots}{
	 \describe{
    \item{\code{listechoix}:}{Object of class \code{"character"}}
    \item{\code{label}:}{Object of class \code{"character"} ~ Item label }
    \item{\code{selected}:}{Object of class \code{"integer"} ~ Selected item }
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "RefChoix")}: Loads the date }
    \item{choix}{\code{signature(objet = "RefChoix")}: Enables to the user to select a date }
	 }
}

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
showClass("RefChoix")
}
\keyword{classes}
