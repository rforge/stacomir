\name{choix-methods}
\docType{methods}
\alias{choix-methods}
\alias{choix}
\alias{choix,PasDeTemps-method}
\alias{choix,PasDeTempsJournalier-method}
\alias{choix,RefAnnee-method}
\alias{choix,RefCheckBox-method}
\alias{choix,RefChoix-method}
\alias{choix,RefDC-method}
\alias{choix,RefDF-method}
\alias{choix,RefHorodate-method}
\alias{choix,RefListe-method}
\alias{choix,Refpar-method}
\alias{choix,Refparqual-method}
\alias{choix,RefStades-method}
\alias{choix,RefStationMesure-method}
\alias{choix,RefTaxon-method}
\alias{choix,RefTextBox-method}
\title{Methods for Function choix}
\description{
These methods are used within the graphical interface to load the referential elements of each
class (ex Reftaxon) and copy the user choice in the environment "envir_stacomi"
}
\arguments{
 according to the object, the argument for the function can be the following
  \item{objectBilan}{un object bilan}

  \item{is.enabled}{a boolean indincating # see if
  deprecated}

  \item{nomassign}{the name to be asssigned in
  envir_stacomi}

  \item{funoutlabel}{the label that appears in funout}

  \item{titleFrame}{title for the frame}

  \item{preselect}{the number of the year selected in the
  gdroplist (integer)}
}
\section{Methods}{
\describe{

\item{\code{signature(object = "PasDeTemps")}}{ choice method for class PasdeTemps, allows to choose the timestep }

\item{\code{signature(object = "PasDeTempsJournalier")}}{ choice method for daily timestep }

\item{\code{signature(object = "RefAnnee")}}{ choice method for RefAnnee referential objects assign the
  object in envir_stacomi }

\item{\code{signature(object = "RefCheckBox")}}{ choice method for RefCheckBox referential objects }

\item{\code{signature(object = "RefChoix")}}{ Choice method for Refchoix referential objects }

\item{\code{signature(object = "RefDC")}}{ choice method for RefDC }

\item{\code{signature(object = "RefDF")}}{ Choice method for DF referential objects }

\item{\code{signature(object = "RefHorodate")}}{ Choice method for RefHorodate referential objects }

\item{\code{signature(object = "RefListe")}}{ Choice method for RefListe referential objects }

\item{\code{signature(object = "Refpar")}}{ Choice method for Refpar referential objects }

\item{\code{signature(object = "Refparqual")}}{ Choice method for Refparqual referential objects }

\item{\code{signature(object = "RefStades")}}{  Choice method for RefStades referential objects }

\item{\code{signature(object = "RefStationMesure")}}{ Choice method for RefStationMesure referential object }

\item{\code{signature(object = "RefTaxon")}}{   Choice method for Reftaxon referential objects }

\item{\code{signature(object = "RefTextBox")}}{ Choice method for ReTextBox referential objects }
}}
\note{
  The choice method has for arguments a report (bilan) object
  (e.g) is called from a report Bilan(e.g Bilan_lot).
   By default,  the value of the objectbilan is null.
   When it is not the method calls the daughter widgets (e.g. the dc widget will call species) 
  and fills it with the method  \code{\link{charge_avec_filtre-methods}}
  \enumerate{
	\item \code{signature(object = "RefDC")}the choice method assigns an object of class refDF in the
  environment envir_stacomi.
	\item \code{signature(object = "RefStades")} the method tests if the load is called from within a
  "bilan" object, and loads par, parqual, or parquan objects accordingly.
	\item \code{signature(object = "RefPar")} the choice method assigns an object of class Refpar 
	named refpar in the environment envir_stacomi
	\item \code{signature(object = "RefParquan")} this method choix is also on sons objects Refparquan,
  hence the parameters,however it was redefined in refparqual to load the possible values of qualitative parameters
	\item \code{signature(object = "RefParqual")} the choice method assigns an object of class Refparqual named refparqual in the environment envir_stacomi this
  method rewrites the method from Refpar, as it integrates a request of the possible values of qualitative  parameters, 
  hence the parameters, however it was redefined in refparqual to load the possible values of qualitative
  parameters.
	\item \code{signature(object = "RefList")} 
  the choice method assigns an object of class refList named refListe in the environment envir_stacomi
}}
\keyword{methods}
\keyword{dynamic}
