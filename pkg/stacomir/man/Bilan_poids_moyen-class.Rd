\name{Bilan_poids_moyen-class}
\docType{class}
\alias{Bilan_poids_moyen-class}
\alias{Bilan_poids_moyen}
\alias{charge,Bilan_poids_moyen-method}
\alias{connect,Bilan_poids_moyen-method}
\alias{funtableBilan_poids_moyen}
\alias{fungraphBilan_poids_moyen}
\title{Class "Bilan_poids_moyen"}
\description{Bilan_poids_moyen class
The objective is to calculate mean weight of glass eel which are counted from weight measurements and to reintegrate weight to number coefficients
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} data for bilan lot }
    \item{\code{dc}:}{Object of class \code{"RefDC"} refDC an instantiation of the counting device class}
    \item{\code{anneedebut}:}{Object of class \code{"RefAnnee"} refAnnee allows to choose year of beginning }
    \item{\code{anneefin}:}{Object of class \code{"RefAnnee"} refAnnee allows to choose year of ending }
    \item{\code{coe}:}{Object of class \code{"RefCoe"} class loading coefficient of conversion between quantity (weights or volumes of glass eel) and numbers }
    \item{\code{liste}:}{Object of class \code{"RefListe"} RefListe referential class choose within a list, here do you want subsamples or not }
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
\author{Cedric Briand \email{cedric.briand00@gmail.com}}
\note{
We have also tools available to import glass eel measurement from experimental fishing in the estuary
For the charge method dates for the request are from august to august (a glass eel season)
}
\references{ \url{http://trac.eptb-vilaine.fr:8066/tracstacomi} }
\examples{
showClass("Bilan_poids_moyen")
objet=new("Bilan_poids_moyen")
}
\keyword{classes}
\keyword{dynamic}