\name{charge_avec_filtre-methods}
\docType{methods}
\alias{charge_avec_filtre-methods}
\alias{charge_avec_filtre,Refpar-method}
\alias{charge_avec_filtre,Refparqual-method}
\alias{charge_avec_filtre,Refparquan-method}
\alias{charge_avec_filtre,RefStades-method}
\alias{charge_avec_filtre,RefTaxon-method}
\alias{charge_avec_filtre,RefMsg-method}
\title{Methods for Function charge_avec_filtre}
\description{
Methods for function \code{charge_avec_filtre} 
}
\section{Methods}{
\describe{

\item{\code{signature(objet = "Refpar")}}{ Loading method for Repar referential objects searching only those parameters existing for a DC, a Taxon, and a stade}

\item{\code{signature(objet = "Refparqual")}}{ Loading method for Reparqual referential objects searching only those parameters existing for a DC, a Taxon, and a stade }

\item{\code{signature(objet = "Refparquan")}}{ Loading method for Reparquan referential objects searching only those parameters existing for a DC, a Taxon, and a stade }

\item{\code{signature(objet = "RefStades")}}{ Loading method for RefStades referential objects searching only those stages existing for a DC and a Taxon}

\item{\code{signature(objet = "RefTaxon")}}{ Loading method for RefTaxon referential objects searching only those stages existing for a DC and a Taxon }

\item{\code{signature(objet = "RefMsg")}}{  Loading method for RefMsg referential objects searching ref.ts_messagerlang_mrl for the lines corresponding to lang, the language chosen for the interface}
}}
\keyword{methods}
\keyword{database}