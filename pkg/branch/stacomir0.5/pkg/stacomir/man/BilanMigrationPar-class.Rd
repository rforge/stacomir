\name{BilanMigrationPar-class}
\Rdversion{1.1}
\docType{class}
\alias{BilanMigrationPar-class}

\title{Class "BilanMigrationPar"}
\description{calcul de migration integrant des caracteristiques qualitatives 
ou quantitatives de lots ou les deux  ex poids les anguilles avec classes de taille par pas de temps
developpement : je commence par lui passer deux param puis possiblite de ne pas en choisir dans la boite ="aucun"
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BilanMigrationPar", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
	 \describe{
    \item{\code{parquan}:}{Object of class \code{"Refparquan"} }
    \item{\code{parqual}:}{Object of class \code{"Refparqual"}  }
    \item{\code{echantillon}:}{Object of class \code{"RefChoix"}  }
    \item{\code{valeurs_possibles}:}{valeurs possibles d'un parametre qualitatif (discret)}
    \item{\code{dc}:}{Object of class \code{"RefDC"}  inherited from class \code{"BilanMigration"}}
    \item{\code{taxons}:}{Object of class \code{"RefTaxon"}  inherited from class \code{"BilanMigration"}}
    \item{\code{stades}:}{Object of class \code{"RefStades"}  inherited from class \code{"BilanMigration"}}
    \item{\code{pasDeTemps}:}{Object of class \code{"PasDeTempsJournalier"}  inherited from class \code{"BilanMigration"} }
    \item{\code{data}:}{Object of class \code{"data.frame"}  inherited from class \code{"BilanMigration"}}
    \item{\code{duree}:}{Object of class \code{"POSIXct"} inherited from class \code{"BilanMigration"}}
  }
}
\section{Extends}{
Class \code{"\linkS4class{BilanMigration}"}, directly.
}
\section{Methods}{
\describe{
    \item{charge}{\code{signature(objet = "BilanMigrationPar")}: does the verification and calculation, the calls to the database are done within a function}
    \item{graphe}{\code{signature(objet = "BilanMigrationPar"): method with a parameter choix to join several graphes called by several handlers (buttons)}
 
}
}}

\author{ Cedric Briand \email{cedric.briand@lavilaine.com}}

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
showClass("BilanMigrationPar")
}
\keyword{classes}
