\name{RefAnnee-class}
\docType{class}
\alias{RefAnnee-class}
\alias{RefAnnee}

\title{Class "RefAnnee"}
\description{RefAnnee referential class to choose years}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RefAnnee", data=data.frame(), annee_selectionnee=numeric())}.
	 
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"}  Every year in the database}
    \item{\code{annee_selectionnee}:}{Object of class \code{"numeric"}  The year selected by user}
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet="RefAnnee")}: Loads the availabe years in the database }
    \item{choice}{\code{signature(objet="RefAnnee")}: Displays a panel to enable the user to choose a year}
	 }
}
%\references{ ~put references to the literature/web site here ~ }
\author{cedric.briand"at"eptb-vilaine.fr}
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~
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
showClass("RefAnnee")
}
\keyword{classes}