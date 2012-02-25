\name{BilanMigrationPar-class}
\Rdversion{1.1}
\docType{class}
\alias{BilanMigrationPar-class}
\alias{BilanMigrationPar}
\title{Class BilanMigrationPar, Migration report along with quantitative and qualitative characteristics}
\description{Migration along with qualitative or quantitative characteristics 
or both  (e.g.) weight of eels according to the size class per period of time,
weight of fish according to gender...
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BilanMigrationPar", ...)}.
	they are loaded by the interface using interface_BilanMigrationPar function
}
\section{Slots}{
	 \describe{
    \item{\code{parquan}:}{Object of class \code{"Refparquan"} }
    \item{\code{parqual}:}{Object of class \code{"Refparqual"}  }
    \item{\code{echantillon}:}{Object of class \code{"RefChoix"} do we add subsample ? }
    \item{\code{valeurs_possibles}:}{possible values for a qualitative parameter }
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

\author{ Cedric Briand \email{cedric.briand00@gmail.com}}
\note{
program : default two parameter choice, checking box "aucun" will allow the program to ignore the parameter
}
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
\keyword{dynamic}