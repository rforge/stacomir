\name{connect}
\alias{connect}
\title{connect method for referential and bilan class}
\arguments{
  \item{h}{a handler}
}
\value{
  a connexion with slot filled

  bilanMigrationInterannuelle an instantianted object with
  values filled with user choice

  An object of BilanConditionEnv class

  An object of class Bilan_stades_pigm

  An object of class bilan_lot Bilan_lot

  bilan_taille with requete field filled

  Bilan_Poids_Moyen request corresponding to user choices

  bilanEspeces instance with request corresponding to the
  user choices
}
\description{


  connect method loads a request to the database and
  returns either an error or a data.frame

  connect method loads a request to the database and
  returns either an error or a data.frame

  connect method loads a request to the database and
  returns either an error or a data.frame

  connect method for BilanMigrationInterannuelle class

  connect method for BilanConditionEnv class

  connect method for Bilan_stades_pigm

  connect method for Bilan_lot

  connect method for class Bilan_taille

  connect method for Bilan_Poids_moyen

  connect method for BilanEspeces
}
\note{
  assign("showmerequest",1,envir=envir_stacomi) permet
  d'afficher toutes les requetes passant par la classe
  connect

  method modified from v0.2.1240 to use the connect method
  of the mother class

  method modified from v0.2.1240 to use the connect method
  of the mother class which in turn will use the method of
  the mother class

  dates for the request are from august to august (a glass
  eel season)
}
\examples{
objet<-new("RequeteODBCwhere")
objet
}
\author{
  Cedric Briand \email{cedric.briand00@gmail.com}

 
}

