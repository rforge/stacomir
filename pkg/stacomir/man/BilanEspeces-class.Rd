\name{BilanEspeces-class}
\Rdversion{1.1}
\docType{class}
\alias{BilanEspeces-class}
\alias{charge,BilanEspeces-method}
\alias{connect,BilanEspeces-method}

\title{Class "BilanEspeces" Bilan des especes presentes sur un DC}
\description{this class is used to make the assessment of all species, and their number, per month
it writes either an histogram or a pie chart of number per year/week/month 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BilanEspeces", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{dc}:}{Object of class \code{"RefDC"}}
    \item{\code{horodate}:}{Object of class \code{"RefHorodate"}}
    \item{\code{datedebut}:}{Object of class \code{"POSIXlt"}}
    \item{\code{datefin}:}{Object of class \code{"POSIXlt"}}
    \item{\code{data}:}{Object of class \code{"data.frame"}}
    \item{\code{liste}:}{Object of class \code{"RefListe"}}
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "BilanEspeces")}: charge method}
    \item{connect}{\code{signature(objet = "BilanEspeces")}: connect method }
	 }
}
\references{ \url{http://62.160.92.241:8066/tracstacomi/wiki/StacomiR} }
\author{Cedric Briand \email{cedric.briand@lavilaine.com}}
\seealso{
    Other Bilan Classes
	\code{\linkS4class{Bilan_lot}}
	\code{\linkS4class{Bilan_poids_moyen}}
	\code{\linkS4class{Bilan_stades_pigm}}	
	\code{\linkS4class{Bilan_taille}}
	\code{\linkS4class{BilanConditionEnv}}
	\code{\linkS4class{BilanEspeces}}
	\code{\linkS4class{BilanFonctionnementDC}}
	\code{\linkS4class{BilanFonctionnementDF}}	
	\code{\linkS4class{BilanMigration}}	
	\code{\linkS4class{BilanMigrationConditionEnv}}
	\code{\linkS4class{BilanMigrationInterAnnuelle}}
	\code{\linkS4class{BilanMigrationPar}}	
}
\examples{
showClass("BilanEspeces")
}
\keyword{classes}
