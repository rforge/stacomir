\name{Refparquan}
\Rdversion{1.1}
\docType{class}
\alias{Refparquan}
\alias{charge,Refparquan-method}
\alias{charge_avec_filtre,Refparquan-method}

\title{Class "Refparquan"}
\description{Class enabling to load the list of quantitative parameters and to select one of them. It inherits from 'Refpar', uses its 'choix' method}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Refparquan", data=data.frame())}.
	\describe{
		\item{\code{data}:}{Object of class \code{"data.frame"} ~ Quantitative parameters }
	}
}
\section{Slots}{
	 \describe{
		\item{\code{data}:}{Object of class \code{"data.frame"} ~ Quantitative parameters }
  }
}
\section{Extends}{
Class \code{"\linkS4class{Refpar}"}, directly.
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "Refparquan")}: Loads the quantitative parameters }
    \item{charge_avec_filtre}{\code{signature(objet = "Refparquan")}: Loads the quantitative parameters with a filter}
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
showClass("Refparquan")
}
\keyword{classes}
