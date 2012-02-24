\name{fun50}
\alias{fun50}
\title{used by \code{fundist} function, returns the value of obj where more than 50 percent of the distribution objc is reached}
\usage{
  fun50(obj, objc)
}
\description{
Allows to point the middle of the gamma distribution for each stage
}
\arguments{
  \item{obj}{the pigment time scale}

  \item{objc}{cumulated gamma curve}
}
\value{
  d50 the center of the distribution for the different stages
}
\seealso{
	\code{\linkS4class{Bilan_stades_pigm}}	
}
\author{
  Cedric Briand \email{cedric.briand00@gmail.com}
}

