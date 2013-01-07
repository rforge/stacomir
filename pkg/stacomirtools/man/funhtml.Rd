\name{funhtml}
\alias{funhtml}
\title{function used to print the html tables of output (see xtable documentation)}
\usage{
  funhtml(data,caption=NULL,top=TRUE,outfile=NULL,clipboard=FALSE,append=TRUE,digits=NULL,...)
}
\arguments{
  \item{data}{a data frame}
  \item{caption}{the caption}
  \item{top}{a logical, if true the caption is placed on top}
  \item{outfile}{the path to the file}
  \item{clipboard}{if clipboard TRUE, a copy to the clipboard is made}
  \item{append}{is the file appended to the previous one ?}
  \item{digits}{ the number of digits}
  \item{...}{ additional parameters to be passed to the function}
}
\value{
  an xtable
}
\description{
  see \pkg{xtable} for further description, an xtable is created and printed to html format
}
\author{
  Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
}

