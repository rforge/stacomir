\name{chargecsv}
\alias{chargecsv}
\title{chargecsv loads the informations stored in c:/program files/stacomi/calcmig.csv file}
\usage{
  chargecsv()
}
\value{
  a list with the datawd place and the baseODBC vector
}
\description{
  chargecsv loads the informations stored in c:/program  files/stacomi/calcmig.csv file
  be sure to configure your odbc link to the database, the name is the name of the first column of the calcmig.csv file
  uid, pwd are identifier and password to connect to the database, they should correspond to your own schema in the database
  pgwd, is the path to the R source if you plan not to use the compiler but develop from source https://r-forge.r-project.org/scm/?group_id=1019
  datawd, is the the directory where you want to place the outputs, mostly tables, from the program, default to ~//CalcmigData
 lang, is either one of French, English or Spanish
}
\author{
  Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
}

