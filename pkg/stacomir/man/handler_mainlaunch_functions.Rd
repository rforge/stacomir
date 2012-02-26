\name{handler_mainlaunch_functions}
\alias{hbilanMigrationcalc}
\alias{calchBilanEspecescalc}
\alias{hbilanMigrationConditionEnvcalc}
\alias{hbilanMigrationParcalc}
\alias{hBilanEspecescalc}
\alias{hcalculeBilanTaille}
\title{function called by handler which do the main calculations for a Bilan class}
\usage{
 hbilanMigrationcalc(h, ...)
 hBilanEspecescalc(h, ...)
 hbilanMigrationConditionEnvcalc(h, ...)
 ...
}
\arguments{
  \item{h}{the handler}
  \item{...}{additional arguments passed to the function}
}
\description{
  The handler functions are linked to S4 classes, but due to gwidget programming cannot be programmed as S4 methods
  function called by handler which perform calculations. See each bilan class for detail
  \enumerate{
 	 \item \code{\linkS4class{BilanMigration}}{ \code{hbilanMigrationcalc} calculates the migration report, write it in \env{envir_stacomi}
		and might treat appart sample quantities(If taxa is eel and stage is glass eel)}
 	 \item \code{\linkS4class{BilanEspeces}}{ \code{hBilanEspecescalc(h, ...)}}
 	 \item \code{\linkS4class{BilanMigrationConditionEnv}}{ \code{hbilanMigrationConditionEnvcalc(h, ...)}   handler for graph
      calculates migration report with environmental conditions, write it in envir_stacomi}
     \item \code{\linkS4class{BilanMigrationPar}}{ \code{hbilanMigrationParcalc(h, ...)}}
  }
}
\author{
  Cedric Briand \email{cedric.briand00@gmail.com}
}

