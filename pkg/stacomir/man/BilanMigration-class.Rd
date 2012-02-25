\name{BilanMigration-class}
\Rdversion{1.1}
\docType{class}
\alias{BilanMigration}
\alias{BilanMigration-class}
\alias{coerce,BilanMigration,BilanMigrationInterAnnuelle-method}

\title{Class "BilanMigration"}
\description{Balance of fish migrations}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BilanMigration", dc=new("RefDC"),taxons=("RefTaxon"),stades=("RefStades"),pasDeTemps=("PasDeTempsJournalier"),data=data.frame(), duree=new(POSIXct) )}.
	\describe{
    \item{\code{dc}:}{Object of class \code{"RefDC"}: the control device }
    \item{\code{taxons}:}{Object of class \code{"RefTaxon"}: the taxa of the fish}
    \item{\code{stades}:}{Object of class \code{"RefStades"} : the stage of the fish}
    \item{\code{pasDeTemps}:}{Object of class \code{"PasDeTempsJournalier"} : the time step constrained to daily value and 365 days}
    \item{\code{data}:}{Object of class \code{"data.frame"} : data}
    \item{\code{duree}:}{Object of class \code{"POSIXct"} : duration of the analysis}
  }
}
\section{Slots}{
	 \describe{
    \item{\code{dc}:}{Object of class \code{"RefDC"}  The control device }
    \item{\code{taxons}:}{Object of class \code{"RefTaxon"}  The race of the concerned fish}
    \item{\code{stades}:}{Object of class \code{"RefStades"}  The phase of the concerned fish}
    \item{\code{pasDeTemps}:}{Object of class \code{"PasDeTempsJournalier"}  The time step }
    \item{\code{data}:}{Object of class \code{"data.frame"} report data}
    \item{\code{duree}:}{Object of class \code{"POSIXct"}  Duration of the overview}
  }
}
\section{Methods}{
  \describe{
    \item{calcule}{\code{signature(objet = "BilanMigration")}: retrieves informations from the database }
    \item{coerce}{\code{signature(from = "BilanMigration", to = "BilanMigrationInterAnnuelle")}: ... }
	 }
}

\author{Cedric Briand \email{cedric.briand00@gmail.com}}

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
showClass("BilanMigration")
bilanMigration= new("BilanMigration")
}
\keyword{classes}
\keyword{dynamic}