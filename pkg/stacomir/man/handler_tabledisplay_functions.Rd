\name{handler_tabledisplay_functions}
\alias{hbilanConditionEnvstat}
\alias{hbilanMigrationParstat}
\alias{htableBilanMigrationInterAnnuelle}
\alias{hTableBilanEspeces}
\alias{hTableBilanMigration}
\alias{funtableDC}
\alias{funtableDF}
\alias{funtableBilan_tail}
\alias{funtableBilan_poids_moyen}
\alias{funtableBilan_lot}
\title{function called by handler which display statistical results}
\usage{
 hbilanConditionEnvstat(h, ...)
}
\arguments{
  \item{h}{the handler}
  \item{...}{additional arguments passed to the function}
}
\description{
  The handler functions are linked to S4 classes, but due to gwidget programming cannot be programmed as S4 methods
  function called by handler which display table or stats either as .csv or html
  the stats are stored in the \strong{Calcmig} folder which location can be changed by changing
  the calcmig.xml function in the \file {c:\\program files\\stacomi} folder
\enumerate{
\item \code{\linkS4class{BilanConditionEnv}}{ \code{hbilanConditionEnvstat}}
\item \code{\linkS4class{BilanMigrationPar}}{ \code{hbilanMigrationParstat} Calls the generic method \code{graphe,BilanMigrationPar-method} with choix=3}
\item \code{\linkS4class{BilanMigrationInterAnnuelle}}{ \code{htableBilanMigrationInterAnnuelle} }
\item \code{\linkS4class{BilanEspeces}}{ \code{hTableBilanEspeces} }
\item \code{\linkS4class{BilanMigration}}{ \code{hTableBilanMigration} calls functions \code{funstat} and \code{funtable} to perform calculations and store them in .csv and .html files}
\item \code{\linkS4class{BilanFonctionnementDC}}{ \code{funtableDC} calls functions \code{funstat} and \code{funtable} to perform calculations and store them in .csv and .html files}
\item \code{\linkS4class{BilanFonctionnementDF}}{ \code{funtableDF} calls functions \code{funstat} and \code{funtable} to perform calculations and store them in .csv and .html files}
\item \code{\linkS4class{Bilan_taille}}{ \code{funtableBilan_tail} }
\item \code{\linkS4class{Bilan_poids_moyen}}{ \code{funtableBilan_poids_moyen}function for showing table in gdf } 
\item \code{\linkS4class{Bilan_lot}}{ \code{funtableBilan_lot}function for showing table in gdf } 
}}
\author{
  Cedric Briand \email{cedric.briand00@gmail.com}
}

