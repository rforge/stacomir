\name{graph_handler_functions}
\alias{hbilanConditionEnvgraph}
\alias{funbarchartDC}
\alias{funbarchartDF}
\alias{funboxDC}
\alias{funboxDF}
\title{function called by handler which displays a graph if environmental conditions are in the database during the selected period}
\usage{
  hbilanConditionEnvgraph(h, ...)
  funbarchartDC(h,...)
  funbarchartDF(h, ...)
}
\arguments{
  \item{h}{the handler}

  \item{...}{additional arguments passed to the function}
}
\description{
  The handler functions are linked to S4 classes, but due to gwidget programming cannot be programmed as S4 methods
  \code{\linkS4class{BilanConditionEnv}}{ \code{hbilanConditionEnvgraph} function called by handler which
  displays a graph if environmental conditions are in the  database during the selected period}
  \code{\linkS4class{BilanFonctionnementDC}}{\code{funbarchartDC} a barchart showing the work of a monitoring device}
  \code{\linkS4class{BilanFonctionnementDF}}{\code{funbarchartDF} a barchart showing the work of the fishway}
  \code{\linkS4class{BilanFonctionnementDC}}{\code{funboxDC} draws rectangles to show the work of a monitoring device }
  \code{\linkS4class{BilanFonctionnementDC}}{\code{funboxDF} draws rectangles to show the work of a fishway }

}
\author{
  Cedric Briand \email{cedric.briand00@gmail.com}
}

