\name{Bilan_poids_moyen-class}
\Rdversion{1.1}
\docType{class}
\alias{Bilan_poids_moyen-class}
\alias{charge,Bilan_poids_moyen-method}
\alias{connect,Bilan_poids_moyen-method}
\alias{funtableBilan_poids_moyen}
\alias{fungraphBilan_poids_moyen}
\title{Class "Bilan_poids_moyen"}
\description{Bilan_poids_moyen class
Objectif faire un calcul des poids moyens et permettre de reintegrere les coefficents de conversion poids effectif
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{dc}:}{Object of class \code{"RefDC"} ~~ }
    \item{\code{anneedebut}:}{Object of class \code{"RefAnnee"} ~~ }
    \item{\code{anneefin}:}{Object of class \code{"RefAnnee"} ~~ }
    \item{\code{coe}:}{Object of class \code{"RefCoe"} ~~ }
    \item{\code{liste}:}{Object of class \code{"RefListe"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "Bilan_poids_moyen")}: charge method for Bilan_Poids_moyen}
    \item{connect}{\code{signature(objet = "Bilan_poids_moyen")}: connect method for Bilan_Poids_moyen}
	 }
  
}
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
\author{Cedric Briand \email{cedric.briand@lavilaine.com}}
\note{Un programme permet pour l'iav d'étendre le chargement des donnees de la base mortciv pour etendre la gamme des valeurs modelisees
For the charge method dates for the request are from august to august (a glass eel season)
}
\references{ \url{http://62.160.92.241:8066/tracstacomi/wiki/StacomiR} }
\examples{
showClass("Bilan_poids_moyen")
objet=new("Bilan_poids_moyen")
}
\keyword{classes}
