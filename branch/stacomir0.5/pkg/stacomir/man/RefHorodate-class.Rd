\name{RefHorodate-class}
\Rdversion{1.1}
\docType{class}
\alias{RefHorodate-class}
\alias{choix,RefHorodate-method}
\alias{getanneeprec,RefHorodate-method}
\alias{getRefHorodate,RefHorodate-method}
\alias{setRefHorodate,RefHorodate-method}

\title{Class "RefHorodate" ~~~ }
\description{	 objet utilisee pour sa methode de chargement du choix dans la date
dans fonctionnement DC, avec methode de affichage annee en cours et annee precedente
de modifier facilement l'annee affichee par defaut en changeant decal
horodate="POSIXt"
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RefHorodate", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{horodate}:}{Object of class \code{"POSIXt"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{choix}{\code{signature(objet = "RefHorodate")}: ... }
    \item{getanneeprec}{\code{signature(objet = "RefHorodate")}: ... }
    \item{getRefHorodate}{\code{signature(objet = "RefHorodate")}: ... }
    \item{setRefHorodate}{\code{signature(objet = "RefHorodate")}: ... }
	 }
}

\author{Cedric Briand \email{cedric.briand@lavilaine.com}}

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
showClass("RefHorodate")
}
\keyword{classes}
