\name{BilanMigrationConditionEnv-class}
\Rdversion{1.1}
\docType{class}
\alias{BilanMigrationConditionEnv}
\alias{BilanMigrationConditionEnv-class}

\title{Class "BilanMigrationConditionEnv"}
\description{Enables to compute an annual overview of fish migration and environmental conditions in the same chart}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BilanMigrationConditionEnv", bilanMigration=new("BilanMigration"), bilanConditionEnv=new("BilanConditionEnv"))}.
	\describe{
    \item{\code{bilanMigration}:}{Object of class \code{"BilanMigration"}  The migration overview }
    \item{\code{bilanConditionEnv}:}{Object of class \code{"BilanConditionEnv"}  The environmental overview}
	}
}
\section{Slots}{
	 \describe{
    \item{\code{bilanMigration}:}{Object of class \code{"BilanMigration"} The migration overview }
    \item{\code{bilanConditionEnv}:}{Object of class \code{"BilanConditionEnv"}  The environmental overview}
  }
}
\section{Methods}{
  \describe{
    \item{calcule}{\code{signature(objet = "BilanMigrationConditionEnv")}: Retrieves all the needed information from the database }
	 }
}

\author{Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}}

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
\references{ \url{http://w3.eptb-vilaine.fr:8080/tracstacomi} }
\examples{
showClass("BilanMigrationConditionEnv")
}
\keyword{classes}
\keyword{dynamic}