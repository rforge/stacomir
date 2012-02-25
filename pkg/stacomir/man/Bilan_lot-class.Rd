\name{Bilan_lot-class}
\docType{class}
\alias{Bilan_lot-class}
\alias{Bilan_lot}
\title{Class "Bilan_lot"}
\description{Bilan_lot Bilan class calls the content of the table tj_caracteristiquelot_car}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Bilan_lot", ...)}
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"}}
    \item{\code{dc}:}{Object of class \code{"RefDC"}}
    \item{\code{taxons}:}{Object of class \code{"RefTaxon"}}
    \item{\code{stades}:}{Object of class \code{"RefStades"}}
    \item{\code{par}:}{Object of class \code{"Refpar"}}
    \item{\code{horodate}:}{Object of class \code{"Horodate"} }
    \item{\code{requete}:}{Object of class \code{"RequeteODBCwheredate"}}
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "Bilan_lot")}:  connect method for Bilan_lot }
    \item{connect}{\code{signature(objet = "Bilan_lot")}:  charge method for Bilan_lot class }
	 }
}
\author{Cedric Briand \email{cedric.briand@lavilaine.com}}
\note{This class is displayed by interface_bilan_lot}
\seealso{
    Other Bilan Class
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
\references{ \url{http://trac.eptb-vilaine.fr:8066/tracstacomi} }
\examples{
showClass("Bilan_lot")
objet=new("Bilan_lot")
}
\keyword{classes}
