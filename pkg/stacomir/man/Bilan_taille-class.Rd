\name{Bilan_taille-class}
\Rdversion{1.1}
\docType{class}
\alias{Bilan_taille-class}
\alias{charge,Bilan_taille-method}
\alias{connect,Bilan_taille-method}

\title{Class "Bilan_taille" Bilan class }
\description{class Bilan_taille  Cette classe permet de charger les deux caracteristiques quantitatives de lot  : Taille et taille fourche.}
\author{Cedric Briand \email{cedric.briand@lavilaine.com}}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Bilan_taille", ...)}
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{dc}:}{Object of class \code{"RefDC"} ~~ }
    \item{\code{taxons}:}{Object of class \code{"RefTaxon"} ~~ }
    \item{\code{stades}:}{Object of class \code{"RefStades"} ~~ }
    \item{\code{parquan}:}{Object of class \code{"Refparquan"} ~~ }
    \item{\code{parqual}:}{Object of class \code{"Refparqual"} ~~ }
    \item{\code{horodate}:}{Object of class \code{"RefHorodate"} ~~ }
    \item{\code{requete}:}{Object of class \code{"RequeteODBC"} ~~ }
    \item{\code{datedebut}:}{Object of class \code{"POSIXlt"} ~~ }
    \item{\code{datefin}:}{Object of class \code{"POSIXlt"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "Bilan_taille")}: charge method for Bilan_taille class }
    \item{connect}{\code{signature(objet = "Bilan_taille")}: connect method for Bilan_taille class }
	 }
}
\author{Cedric Briand \email{cedric.briand@lavilaine.com}}
\note{ Explication du traitement :Dans les operations de piegeage, les anguilles peuvent etre passees au crible et separees en groupe
Ces groupes sont bases sur des courbes de selectivite en fonction de la maille, en gros la separation se fait au niveau de la L50 pour Arzal
Il est necessaire pour chaque taille individuelle l'appartenance à une caracteristique qualitative de lot (groupe de taille ex grandes anguillettes)
Cette appartenance est recherchee au niveau du lot par une requete  permettant l'affichage sur une meme ligne de deux caracteristiques differentes
Au niveau du lot pere si il existe, on fait une requete permettant de recuperer les caracteritiques qualitatives de lot du lot pere
Eventuellement, la taille a pu etre rentree au niveau du lot (pas du sous lot) sans qu'une appartenance à une caracteristique ne soit apparente.
Un traitement des donnees au chargement permet d'analyser si la taille correspond à un lot soit au niveau du lot soit au niveau du lot pere
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
\details{
Explication du traitement :
Dans les operations de piegeage, les anguilles peuvent etre passees au crible et separees en groupe
Ces groupes sont bases sur des courbes de selectivite en fonction de la maille, en gros la separation se fait au niveau de la L50 pour Arzal
Il est necessaire pour chaque taille individuelle l'appartenance à une caracteristique qualitative de lot (groupe de taille ex grandes anguillettes)
Cette appartenance est recherchee au niveau du lot par une requete  permettant l'affichage sur une meme ligne de deux caracteristiques differentes
Au niveau du lot pere si il existe, on fait une requete permettant de recuperer les caracteritiques qualitatives de lot du lot pere
Eventuellement, la taille a pu etre rentree au niveau du lot (pas du sous lot) sans qu'une appartenance à une caracteristique ne soit apparente.
Un traitement des donnees au chargement permet d'analyser si la taille correspond à un lot soit au niveau du lot soit au niveau du lot pere
}
\references{ \url{http://62.160.92.241:8066/tracstacomi/wiki/StacomiR} }
\examples{
showClass("Bilan_taille")
}
\keyword{classes}
