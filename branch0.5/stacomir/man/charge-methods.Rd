\name{charge-methods}
\docType{methods}
\alias{charge-methods}
\alias{charge}
\alias{charge,Bilan_lot-method}
\alias{charge,Bilan_poids_moyen-method}
\alias{charge,Bilan_stades_pigm-method}
\alias{charge,Bilan_taille-method}
\alias{charge,BilanConditionEnv-method}
\alias{charge,BilanEspeces-method}
\alias{charge,BilanFonctionnementDC-method}
\alias{charge,BilanFonctionnementDF-method}
\alias{charge,BilanMigrationInterAnnuelle-method}
\alias{charge,RefAnnee-method}
\alias{charge,RefCheckBox-method}
\alias{charge,RefChoix-method}
\alias{charge,RefCoe-method}
\alias{charge,RefDC-method}
\alias{charge,RefDF-method}
\alias{charge,RefListe-method}
\alias{charge,Refpar-method}
\alias{charge,Refparqual-method}
\alias{charge,Refparquan-method}
\alias{charge,RefPoidsMoyenPeche-method}
\alias{charge,RefStades-method}
\alias{charge,RefStationMesure-method}
\alias{charge,RefTaxon-method}
\alias{charge,RefTextBox-method}
\alias{charge,RefMsg-method}
\title{Charge method for referential (ref) and report (bilan) object}
\description{
 This methods loads the data, it is in fact sometimes redundant with connect which should be
 reserved for instances of RequeteODBC and daughter classes. See individual .r files for help and examples
}
\section{Methods}{
\describe{

\item{\code{signature(objet = "Bilan_lot")}}{ loading method for Bilan_lot class }

\item{\code{signature(objet = "Bilan_poids_moyen")}}{ loading method for Bilan_poids_moyen class }

\item{\code{signature(objet = "Bilan_stades_pigm")}}{loading method for Bilan_stades_pigm class  }

\item{\code{signature(objet = "Bilan_taille")}}{ loading method for Bilan_taille }

\item{\code{signature(objet = "BilanConditionEnv")}}{ loading method for BilanCondtionEnv class }

\item{\code{signature(objet = "BilanEspeces")}}{  loading method for BilanEspeces verifies the content of
  objects and calls the connect method }

\item{\code{signature(objet = "BilanFonctionnementDC")}}{ loading method for BilanFonctionnementDC class  }

\item{\code{signature(objet = "BilanFonctionnementDF")}}{ loading method for BilanFonctionnementDF class }

\item{\code{signature(objet = "BilanMigrationInterAnnuelle")}}{ loading method for BilanMigrationInterannuelle class }

\item{\code{signature(objet = "RefAnnee")}}{  Loading method for RefAnnee referential objects, selects
  year avalaible in the t_operation_ope table }

\item{\code{signature(objet = "RefCheckBox")}}{ Loading method for ReCheckBox referential objects}

\item{\code{signature(objet = "RefChoix")}}{ Loading method for Rechoix referential objects}

\item{\code{signature(objet = "RefCoe")}}{ loads the coefficients for the period defined in class
  The slots datedebut and datefin have to be filled before
  using charge }

\item{\code{signature(objet = "RefDC")}}{ loads the counting devices of the control station }

\item{\code{signature(objet = "RefDF")}}{ Loading method for DF referential objects}

\item{\code{signature(objet = "RefListe")}}{ Loading method for Refliste referential objects}

\item{\code{signature(objet = "Refpar")}}{ Loading method for Repar referential objects }

\item{\code{signature(objet = "Refparqual")}}{ Loading method for Reparqual referential objects }

\item{\code{signature(objet = "Refparquan")}}{ Loading method for Reparquan referential objects }

\item{\code{signature(objet = "RefPoidsMoyenPeche")}}{ Loading method for RefPoidsMoyenPeche referential objects}

\item{\code{signature(objet = "RefStades")}}{ Loading method for RefStades referential objects}

\item{\code{signature(objet = "RefStationMesure")}}{ Loading method for RefStationMesure referential object }

\item{\code{signature(objet = "RefTaxon")}}{ Loading method for RefTaxon referential objects }

\item{\code{signature(objet = "RefTextBox")}}{ Loading method for ReTextBox referential objects }

\item{\code{signature(objet = "RefMsg")}}{ Loading method for RefMsg referential objects loads the  common table ts_messager_msr }
}}
\keyword{methods}
\keyword{database}