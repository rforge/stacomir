\name{fungraph_civelle}
\alias{fungraph_civelle}
\title{Graph function for glass eel migration. Differs from fungraph as it does not draw the ggplot graph for month}
\usage{
  fungraph_civelle(bilanMigration, table, duree, taxon,
    stade)
}
\arguments{
  \item{bilanMigration}{an objet of class \code{\linkS4class{BilanMigration}}}

  \item{table=tableau}{a data frame with the results}

  \item{duree}{a vector POSIXt}

  \item{taxon}{the species}

  \item{stade}{the stage}
}
\description{
  This graph will also plot numbers and bars according to whether the glass eel have been counted through weight or numbers
}
\author{
  Cedric Briand \email{cedric.briand00@gmail.com}
}

