\name{Refpar-class}
\docType{class}
\alias{Refpar}
\alias{Refpar-class}
\alias{charge,Refpar-method}
\alias{charge_avec_filtre,Refpar-method}
\alias{choix,Refpar-method}

\title{Class "Refpar" }
\description{ Class enabling to load the list of parameters and select one of them}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Refpar", data)}.
	 \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ All the parameters stored in the database}
  }
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~ All the parameters stored in the database}
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "Refpar")}: Loads the parameters }
    \item{charge_avec_filtre}{\code{signature(objet = "Refpar")}: Loads the parameters with a filter}
    \item{choix}{\code{signature(objet = "Refpar")}: Enables to the user to select a parameter}
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
showClass("Refpar")
}
\keyword{classes}
