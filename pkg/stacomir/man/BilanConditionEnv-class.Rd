\name{BilanConditionEnv-class}
\docType{class}
\alias{BilanConditionEnv-class}
\alias{BilanConditionEnv}

\title{class BilanConditionEnv simple output of one or several environmental conditions...}

\description{ Annual overview of environmental conditions. Enables to draw charts and write files.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BilanConditionEnv", horodate=new("Horodate"), stationMesure=new("RefStationMesure"), data=data.frame(), requete=new("RequeteODBCwheredate"))}.
}
\section{Slots}{
	 \describe{
    \item{\code{datedebut}:}{Object of class \code{"Horodate"} : Starting and ending date}
    \item{\code{datefin}:}{Object of class \code{"Horodate"} : Starting and ending date}    
    \item{\code{stationMesure}:}{Object of class \code{"RefStationMesure"} : The measure(s) station(s)}
    \item{\code{data}:}{Object of class \code{"data.frame"} : Environmental data}
    \item{\code{horodate}:}{Object of class \code{"RequeteODBCwheredate"} : Query to get informations about the environmental conditions }
  }
}
\section{Methods}{
  \describe{
    \item{charge}{\code{signature(objet = "BilanConditionEnv")}: Loads values in the database }
    \item{connect}{\code{signature(objet = "BilanConditionEnv")}: Executes the query to get informations}
	 }
}
%\references{ ~~put references to the literature/web site here ~~ }
\author{cedric.briand00@gmail.com}
%\note{ ~~further notes~~ }

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
showClass("BilanConditionEnv")
}
\keyword{classes}
\keyword{dynamic}