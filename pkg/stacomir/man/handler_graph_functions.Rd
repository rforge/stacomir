\name{handler_graph_functions}
\alias{hbilanConditionEnvgraph}
\alias{hbilanEspeces}
\alias{funbarchartDC}
\alias{funbarchartDF}
\alias{funboxDC}
\alias{funboxDF}
\alias{hCamembert}
\alias{hHistogramme}
\alias{hbilanMigrationConditionEnvgraph}
\alias{hbilanMigrationgraph}
\alias{hbilanMigrationPargraph}
\alias{hbilanMigrationPargraph2}
\alias{hfungraphstades}
\alias{hgraphBilanMigrationInterAnnuelle}
\alias{hgraphBilanMigrationInterAnnuelle2}
\alias{hgraphBilanMigrationInterAnnuelle3}
\alias{hgraphBilanMigrationInterAnnuelle4}
\alias{hgraphBilanMigrationInterAnnuelle5}
\alias{hgraphBilanMigrationInterAnnuelle7}
\title{function called by handler which displays a graph if environmental conditions are in the database during the selected period}
\usage{
  hbilanConditionEnvgraph(h, ...)
  funbarchartDC(h,...)
  ...
}
\arguments{
  \item{h}{the handler}
  \item{...}{additional arguments passed to the function}
}
\description{
  The handler functions are linked to S4 classes, but due to gwidget programming cannot be programmed as S4 methods
  \enumerate{
	  \item \code{\linkS4class{BilanConditionEnv}}{ \code{hbilanConditionEnvgraph} function called by handler which
	  displays a graph if environmental conditions are in the  database during the selected period}
	  \item \code{\linkS4class{BilanFonctionnementDC}}{\code{funbarchartDC} a barchart showing the work of a monitoring device}
	  \item \code{\linkS4class{BilanFonctionnementDF}}{\code{funbarchartDF} a barchart showing the work of the fishway}
	  \item \code{\linkS4class{BilanFonctionnementDC}}{\code{funboxDC} draws rectangles to show the work of a monitoring device }
	  \item \code{\linkS4class{BilanFonctionnementDF}}{\code{funboxDF} draws rectangles to show the work of a fishway }
 	  \item \code{\linkS4class{BilanEspeces}}{\code{hCamembert} builds pie charts showing the species present for a given period }
 	  \item \code{\linkS4class{BilanEspeces}}{\code{hHistogramme} builds and histogram showing the species present for a given period }
       \item \code{\linkS4class{BilanMigrationConditionEnv}}{\code{hbilanMigrationConditionEnvgraph} graph of both environmental conditions and migrations }
      \item \code{\linkS4class{BilanMigration}}{\code{hbilanMigrationgraph} calls the function \code{fungraph}to plot the results of bilanmigration
  and saves daily migrations into the database }
      \item \code{\linkS4class{BilanMigration}}{\code{hbilanMigrationgraph2} calls function \code{fungraph} and then draws a calculation of cumulated migration along the year,
      this plot might help seeing changes in seasonal migration patterns}
      \item \code{\linkS4class{BilanMigrationPar}}{\code{hbilanMigrationPargraph} Calls the generic method \code{graphe,BilanMigrationPar-method} with choix=1}
      \item \code{\linkS4class{BilanMigrationPar}}{\code{hbilanMigrationPargraph2} Calls the generic method \code{graphe,BilanMigrationPar-method} with choix=2}
      \item \code{\linkS4class{Bilan_stades_pigm}}{\code{hfungraphstades} Calls the function \code{fungraphstades}}
      
   }
}
\note{
  \enumerate{
  \item \code{\linkS4class{BilanEspeces}}{\code{hCamembert} <0 numbers are transformed into positive for counts}
  \item \code{\linkS4class{BilanMigration}}{\code{hbilanMigrationgraph} this function has been developped with daily time step and will not
     adapt to larger ones, the function calls \code{calcule,BilanMigration-method} and then for glass eel will call \code{\link{fungraph_civelle}} as there is 
     the special case of weight being measured there instead of numbers, for other species will call \code{\link{fungraph}}
}
  }
}
\author{
  Cedric Briand \email{cedric.briand00@gmail.com}
}

