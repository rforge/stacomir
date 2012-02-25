\name{BilanEspeces-class}
\Rdversion{1.1}
\docType{class}
\alias{BilanEspeces-class}
\alias{BilanEspeces}
\alias{charge,BilanEspeces-method}
\alias{connect,BilanEspeces-method}

\title{Class "BilanEspeces" Report of the species present at a counting device for a given period}
\description{this class is used to make the assessment of all species, and their number, per month
it writes either an histogram or a pie chart of number per year/week/month 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BilanEspeces", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{dc}:}{Object of class \code{"RefDC"} the counting device}
    \item{\code{horodate}:}{Object of class \code{"RefHorodate"} date}
    \item{\code{datedebut}:}{Object of class \code{"POSIXlt"} beginning date}
    \item{\code{datefin}:}{Object of class \code{"POSIXlt"} ending date}
    \item{\code{data}:}{Object of class \code{"data.frame"} data for the report}
    \item{\code{liste}:}{Object of class \code{"RefListe"} lists the possible value of time period}
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "BilanEspeces")}: charge method}
    \item{connect}{\code{signature(objet = "BilanEspeces")}: connect method }
	 }
}
\references{ \url{http://trac.eptb-vilaine.fr:8066/tracstacomi} }
\author{Cedric Briand \email{cedric.briand00@gmail.com}}
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
\examples{
showClass("BilanEspeces")
}
\keyword{classes}
\keyword{dynamic}