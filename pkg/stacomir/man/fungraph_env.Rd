\name{fungraph_env}
\alias{fungraph_env}
\title{Function for class BilanMigrationEnv drawing both the response of environment variables...}
\usage{
  fungraph_env(tableau, duree, taxon, stade, stations)
}
\arguments{
  \item{tableau}{data issued from a bilanMigration}
  \item{duree}{a vector of class POSIXt}
  \item{taxon}{the species}
  \item{stade}{the stage}
  \item{stations}{one or several measure stations}
}
\description{
  graph function for BilanMigrationEnv, draws both the response of environment variables (temperature, moon phases...) and
  the migration for a species and a stage
}
\author{
  Cedric Briand \email{cedric.briand00@gmail.com}
}

