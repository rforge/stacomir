\name{hbilanMigrationgraph}
\alias{hbilanMigrationgraph}
\title{handler du calcul hBilanMigrationgraph
appelle les fonctions fungraph pour faire le bilan des migrations
et permet la sauvegarde des bilans journaliers dans la base}
\usage{
  hbilanMigrationgraph(h, ...)
}
\arguments{
  \item{h}{}

  \item{...}{}
}
\description{
  handler du calcul hBilanMigrationgraph appelle les
  fonctions fungraph pour faire le bilan des migrations et
  permet la sauvegarde des bilans journaliers dans la base
}
\note{
  pb si autre chose que journalier les pas de temps ont été
  contraints à des pas de temps journaliers pour ce
  graphique
  calls calcule,BilanMigration-method and then for glass eel will call \code{\link{fungraph_civelle}} as there is 
the special case of weight being measured there instead of numbers, for other species will call \code{\link{fungraph}}
}
\author{
  Cedric Briand \email{cedric.briand00@gmail.com}
}

