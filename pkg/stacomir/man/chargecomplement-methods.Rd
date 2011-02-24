\name{chargecomplement-methods}
\docType{methods}
\alias{chargecomplement-methods}
\alias{chargecomplement,Refparqual-method}
\title{ Methods for Function chargecomplement}
\description{
Methods for function \code{chargecomplement} method chargecomplement
cette methode est appellee apres la selection de l'objet (data ne contient plus qu'une seule ligne)
et permet de lancer une requete pour obtenir un complement, ici les valeurs possibles d'un parametre qualitatif
}
\section{Methods}{
\describe{

\item{\code{signature(objet = "Refparqual")}}{ }
}}
\keyword{methods}
\examples{
dc_selectionne=6
taxon_selectionne=2038
stade_selectionne="AGJ"
objet=new("Refparqual")
objet<-charge(objet)
chargecomplement(objet)
}

