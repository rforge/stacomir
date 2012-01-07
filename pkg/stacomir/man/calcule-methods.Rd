\name{calcule-methods}
\docType{methods}
\alias{calcule-methods}
\alias{calcule,BilanMigration-method}
\alias{calcule,BilanMigrationConditionEnv-method}
\alias{calcule,BilanMigrationPar-method}
\alias{calcule,BilanTaille-method}
\title{ Methods for Function calcule }
\description{
 calcule is called when a class needs computation and not only data collected from the database, it 
 sometimes does the work of the charge method
}
\section{Methods}{
\describe{

\item{\code{signature(objet = "BilanMigration")}}{
to be refined, performs both query and calculation, the calcul calls for \link{funBilanMigrationAnnuel}
and \link{funBilanMigration} }
}

\item{\code{signature(objet = "BilanMigrationConditionEnv")}}{ collects object assigned to \envir{envir_stacomi} and loads the \link{BilanConditionEnv} 

\item{\code{signature(objet = "BilanMigrationPar")}}{collects object assigned to \envir{envir_stacomi} and checks for validity
}
\item{\code{signature(objet = "BilanTaille")}}{calls charge, does some computation on samples and subsamples and calls function funtraitementdate
}}
\keyword{methods}
\keyword{manip}