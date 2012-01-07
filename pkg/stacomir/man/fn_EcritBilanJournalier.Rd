\name{fn_EcritBilanJournalier}
\alias{fn_EcritBilanJournalier}
\title{fn_EcritBilanJournier writes the daily migration in the t_bilanmigrationjournalier_bjo table}
\usage{
  fn_EcritBilanJournalier(bilanMigration)
}
\arguments{
  \item{bilanMigration}{}
}
\description{
  fn_EcritBilanJournier writes the daily migration in the
  t_bilanmigrationjournalier_bjo table
}
\note{
  the user is asked whether or not he wants to overwrite
  data, if no data are present in the database, the import
  is done anyway
}
\author{
  Cedric Briand \email{cedric.briand00@gmail.com}
}

