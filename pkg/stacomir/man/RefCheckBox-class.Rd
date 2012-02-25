\name{RefCheckBox-class}
\docType{class}
\alias{RefCheckBox-class}
\alias{charge,RefCheckBox-method}
\alias{choix,RefCheckBox-method}

\title{Class "RefCheckBox" RefCheckBox }
\description{referential class allows to choose for several parms with checkbox}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RefCheckBox", ...)}. 
}
\section{Slots}{
  \describe{
    \item{\code{title}:}{Object of class \code{"character"} ~~ }
    \item{\code{labels}:}{Object of class \code{"character"} ~~ }
    \item{\code{checked}:}{Object of class \code{"logical"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "RefCheckBox")}: ... }
    \item{choix}{\code{signature(objet = "RefCheckBox")}: ... }
	 }
}
\references{ ~put references to the literature/web site here ~ }
\author{Cedric Briand \email{cedric.briand00@gmail.com}}
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
showClass("RefCheckBox")
}
\keyword{classes}
