\name{fnstade}
\alias{fnstade}
\title{fonction calculant a partir des lois gamma les coordonnees en x et y pour tracer un polygone, la fonction peut...}
\usage{fnstade(par1, par2, phicum, phidates, VB=FALSE, neg=TRUE, lmax=1)}
\description{fonction calculant à partir des lois gamma les coordonnees en x et y pour tracer un polygone, la fonction peut
fnstades peut etre utilisee pour tracer un polygone (neg=TRUE) ou simplement renvoyer les valeurs des fonctions gamma pour chaque stade}
\value{une liste avec x et y}
\author{Cedric Briand \email{cedric.briand@lavilaine.com}}
\arguments{\item{par1}{paramètre de la loi gamma du premier stade}
\item{par2}{paramètre de la loi gamma du second stade}
\item{phicum}{cumulated pigmentation times pour test : phicum=seq(0,20, length.out=100)}
\item{phidates}{dates}
\item{VB}{if TRUE, then calculation for stade VB which differs from the others}
\item{neg}{if FALSE la fonction renvoit le calcul des stades en fonction d'un temps pigmentaire}
\item{lmax}{parametre d'echelle de la fonction graphique, lmax=0 permet de tracer les valeurs reelles 
des abondances par stades au cours du temps, lmax=1 ou 0.8 permet de ramener à la meme echelle tous les stades}}
