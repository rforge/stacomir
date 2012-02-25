\name{BilanMigrationInterAnnuelle-class}
\docType{class}
\alias{BilanMigrationInterAnnuelle-class}
\alias{BilanMigrationInterAnnuelle}
\alias{charge,BilanMigrationInterAnnuelle-method}
\alias{coerce,BilanMigration,BilanMigrationInterAnnuelle-method}
\alias{connect,BilanMigrationInterAnnuelle-method}
\alias{supprime,BilanMigrationInterAnnuelle-method}

\title{Class "BilanMigrationInterAnnuelle"}
\description{Several year overview of fish migrations. It enables to compare migration for one year with statistics (min, max, mean) from several year, and to extract data
This class need prior call from bilanMigration to fill in the t_bilanmigrationjour_bjo as it loads the data in this table
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BilanMigrationInterAnnuelle", dc=new("RefDC"), data=data.frame(), taxons=new("RefTaxon"),stades=new("RefStades"), anneeDebut=new("RefAnnee"), anneeFin=new("RefAnnee")) }.
}
\section{Slots}{
	 \describe{
    \item{\code{dc}:}{Object of class \code{"RefDC"} The counting device of the fishway}
    \item{\code{taxons}:}{Object of class \code{"RefTaxon"} The fish taxa}
    \item{\code{stades}:}{Object of class \code{"RefStades"} The fish stage}
    \item{\code{data}:}{Object of class \code{"data.frame"} migration data}
    \item{\code{anneeDebut}:}{Object of class \code{"RefAnnee"} starting date}
    \item{\code{anneeFin}:}{Object of class \code{"RefAnnee"} ending date}
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "BilanMigrationInterAnnuelle")}: Loads informations about the migrations}
    \item{coerce}{\code{signature(from = "BilanMigration", to = "BilanMigrationInterAnnuelle")}: ... }
    \item{connect}{\code{signature(objet = "BilanMigrationInterAnnuelle")}: Connexion to the database}
    \item{supprime}{\code{signature(objet = "BilanMigrationInterAnnuelle")}: Delete a record in the database}
	 }
}

\author{cedric.briand00@gmail.com}

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
showClass("BilanMigrationInterAnnuelle")
}
\keyword{classes}
\keyword{dynamic}