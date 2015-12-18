\name{BilanFonctionnementDF-class}
\docType{class}
\alias{BilanFonctionnementDF-class}
\alias{BilanFonctionnementDF}

\title{Class "BilanFonctionnementDF" Report fishway work}
\description{ The DF (Dispositif de Franchissement) is a fishway. It may be automated and work only at certain times
 This report allows to see the detail of its work.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BilanFonctionnementDF")}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} data of the report }
    \item{\code{df}:}{Object of class \code{"RefDF"} fishway}
    \item{\code{horodate}:}{Object of class \code{"RefHorodate"} }
    \item{\code{requete}:}{Object of class \code{"RequeteODBCwheredate"} request }
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "BilanFonctionnementDF")}: ... }
    \item{connect}{\code{signature(objet = "BilanFonctionnementDF")}: ... }
	 }
}
\references{ ~put references to the literature/web site here ~ }
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
\examples{
showClass("BilanFonctionnementDF")
}
\keyword{classes}
\keyword{dynamic}