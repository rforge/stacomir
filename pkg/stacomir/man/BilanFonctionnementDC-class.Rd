\name{BilanFonctionnementDC-class}
\docType{class}
\alias{BilanFonctionnementDC-class}
\alias{BilanFonctionnementDC}


\title{Class "BilanFonctionnementDC" Bilan du fonctionnement du dispositif de comptage}
\description{The counting device is not always working. It may me stopped either following a monitoring protocol, or due to misfunction of the device,
 this class allows to draw graphics allowing an overview of the device operation }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BilanFonctionnementDC", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} }
    \item{\code{dc}:}{Object of class \code{"RefDC"}}
    \item{\code{horodate}:}{Object of class \code{"RefHorodate"}}
    \item{\code{requete}:}{Object of class \code{"RequeteODBCwheredate"}}
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "BilanFonctionnementDC")}: charge method for BilanFonctionnementDC }
    \item{connect}{\code{signature(objet = "BilanFonctionnementDC")}: connect method for BilanFonctionnementDC }
	 }
}
\references{ \url{http://w3.eptb-vilaine.fr:8080/tracstacomi} }
\author{ Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr} }
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
\examples{
showClass("BilanFonctionnementDC")
}
\keyword{classes}
\keyword{dynamic}