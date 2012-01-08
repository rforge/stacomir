\name{Bilan_taille-class}
\Rdversion{1.1}
\docType{class}
\alias{Bilan_taille-class}
\alias{charge,Bilan_taille-method}
\alias{connect,Bilan_taille-method}

\title{Class "Bilan_taille" Bilan class }
\description{Bilan_taille class This class allows to load the following quantitative sample characteristics : full size, mostly used for yellow eels.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Bilan_taille", ...)}
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} data for the Bilan }
    \item{\code{dc}:}{Object of class \code{"RefDC"} the counting device }
    \item{\code{taxons}:}{Object of class \code{"RefTaxon"}  }
    \item{\code{stades}:}{Object of class \code{"RefStades"}  }
    \item{\code{parquan}:}{Object of class \code{"Refparquan"} quantitative parameter here fixed to size parameters }
    \item{\code{parqual}:}{Object of class \code{"Refparqual"} qualitative parameter for instance sex or pigment stage can be null }
    \item{\code{horodate}:}{Object of class \code{"RefHorodate"} date }
    \item{\code{requete}:}{Object of class \code{"RequeteODBC"} the connection }
    \item{\code{datedebut}:}{Object of class \code{"POSIXlt"} beginning }
    \item{\code{datefin}:}{Object of class \code{"POSIXlt"} ending date }
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "Bilan_taille")}: charge method for Bilan_taille class }
    \item{connect}{\code{signature(objet = "Bilan_taille")}: connect method for Bilan_taille class }
	 }
}
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
\details{
Eels trapped are usually screened and separated into groups, this is not good practise but still in use
The groups used in stacomi are based on the selectivity curve according to the mesh, roughly the separation takes place at the L50 for Arzal
So individuals might refer to one size class  (eg the size group of large eels)
This affiliation is sought at the subsample level by a query to display on a single line two different characteristics size and size group
Once the data are loaded, the program checks if the size class group is indicated at the sample or susample level
}
\references{ \url{http://trac.eptb-vilaine.fr:8066/tracstacomi} }
\examples{
showClass("Bilan_taille")
}
\keyword{classes}
\keyword{dynamic}