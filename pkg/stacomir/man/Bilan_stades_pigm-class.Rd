\name{Bilan_stades_pigm-class}
\Rdversion{1.1}
\docType{class}
\alias{Bilan_stades_pigm-class}
\alias{charge,Bilan_stades_pigm-method}
\alias{connect,Bilan_stades_pigm-method}
\title{Class "Bilan_stades_pigm" Bilan class} 
\description{The pigment stages analysis has been developed to allow to analyze the change in pigment stage structure for glass eel (Anguilla anguilla).
 The class uses the parameters calibrated by Briand et al. , 2005 to backcalculate the probable date when the glass eels arrived in the estuary (i.e. at a fully transparent stage VB.
The evolution of pigment stages is modeled with gamma functions which use a pigment time calculated from daily temperatures and salinities. 
LThe temperatures has a major influence on the glass eel pigment stage evolution. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Bilan_stades_pigm", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} data }
    \item{\code{datatempsal}:}{Object of class \code{"data.frame"} temperature and salinity data }
    \item{\code{tablestades}:}{Object of class \code{"data.frame"} pigment stage table }
    \item{\code{phi}:}{Object of class \code{"list"} pigmentation time }
    \item{\code{dates}:}{Object of class \code{"POSIXt"} dates }
    \item{\code{Vparm}:}{Object of class \code{"list"} parameters for pigmentation evolution }
    \item{\code{dc}:}{Object of class \code{"RefDC"} the counting device }
    \item{\code{stationMesure}:}{Object of class \code{"RefStationMesure"} the station measuring salintiy }
    \item{\code{horodate}:}{Object of class \code{"RefHorodate"} date }
    \item{\code{datedebut}:}{Object of class \code{"POSIXlt"} beginning date }
    \item{\code{datefin}:}{Object of class \code{"POSIXlt"} ending date }
    \item{\code{lmax}:}{Object of class \code{"RefChoix"} see \link{fnstade} }
    \item{\code{options}:}{Object of class \code{"RefCheckBox"}  }
    \item{\code{salinite}:}{Object of class \code{"RefTextBox"} constant value of the salinity used in the model }
    \item{\code{labelretro}:}{Object of class \code{"character"} \link{fungraphstades}}
    \item{\code{labelgraphstades}:}{Object of class \code{"character"} link{fungraphstades} }
    \item{\code{effectifs}:}{Object of class \code{"numeric"} link{fungraphstades} }
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "Bilan_stades_pigm")}: ... }
    \item{connect}{\code{signature(objet = "Bilan_stades_pigm")}: ... }
	 }
}
\references{
BRIAND C., FATIN D., CICCOTTI E. and LAMBERT P., 2005. A stage-structured model to predict the effect of temperature and salinity on glass eel Anguilla anguilla pigmentation development. J Fish Biol, 67, 995-1009.
\url{http://www3.interscience.wiley.com/journal/118686679/abstract} 
\url{http://www.eptb-vilaine.fr/site/index.php/publications-scientifiques/46-publications-migrateurs/60-dynamique-de-population-et-de-migration-des-civelles-en-estuaire-de-vilaine.} 
}
\author{Cedric Briand \email{cedric.briand00@gmail.com}}
\note{This class is displayed by interface_bilan_stades_pigm, 
The class uses temperature (from an abiotic measure station) and mean salinity to calculate the change towards one stage}
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
	\code{\link{fnstade}}	
}
}
\references{ \url{http://trac.eptb-vilaine.fr:8066/tracstacomi} }
\examples{
showClass("Bilan_stades_pigm")
}
\keyword{classes}
\keyword{dynamic}