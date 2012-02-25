\name{RefListe-class}
\docType{class}
\alias{RefListe-class}
\alias{RefListe}

\title{Class "RefListe"}
\description{Enables to load a "RefChoix" object fom a list given by a "Bilan" object}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RefListe", listechoix, label)}.
	\describe{
    \item{\code{listechoix}:}{Object of class \code{"character"}}
    \item{\code{label}:}{Object of class \code{"character"}}
  }
}
\section{Slots}{
	 \describe{
    \item{\code{listechoix}:}{Object of class \code{"character"}}
    \item{\code{label}:}{Object of class \code{"character"}}
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "RefListe")}: Loads a RefList}
    \item{choix}{\code{signature(objet = "RefListe")}: Enables to initialize a RefList}
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
showClass("RefListe")
}
\keyword{classes}
