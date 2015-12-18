\name{connect-methods}
\docType{methods}
\alias{connect,RequeteODBC-method}
\alias{connect,RequeteODBCwhere-method}
\alias{connect,RequeteODBCwheredate-method}
\alias{connect,BilanConditionEnv-method}
\alias{connect,BilanEspeces-method}
\alias{connect,BilanFonctionnementDC-method}
\alias{connect,BilanFonctionnementDF-method}
\alias{connect,BilanMigrationInterAnnuelle-method}
\alias{connect,Bilan_lot-method}
\alias{connect,Bilan_poids_moyen-method}
\alias{connect,Bilan_stades_pigm-method}
\alias{connect,Bilan_taille-method}
\title{ connect method for referential and bilan class}
\description{
see individual .r files for help and examples
  connect method loads a request to the database and
  returns either an error or a data.frame
}
\section{Methods}{

\describe{

\item{\code{signature(objet = "BilanMigrationInterannuelle")}}{}

\item{\code{signature(objet = "BilanConditionEnv")}}{}

\item{\code{signature(objet = "Bilan_stades_pigm")}}{will try to get data for the temperature (refstation) only if retrocalcul is checked  by default it is note when launching}

\item{\code{signature(objet = "Bilan_taille")}}{}

\item{\code{signature(objet = "Bilan_Poids_Moyen")}}{}

\item{\code{signature(objet = "BilanEspeces")}}{}
}}

\keyword{methods}
