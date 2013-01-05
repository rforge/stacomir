\name{trunc.POSIXt}
\alias{ceil.POSIXt}
\alias{trunc.POSIXt}
\alias{round.POSIXt}
\title{trunc.POSIXT and ceil.POSIXt function imported from Hmisc that cannot be loaded in Rforge}
\usage{
  \method{trunc}{POSIXt}(x, digits = c("secs", "mins", "hours", "days", "months", "years"),  \dots)
  \method{ceil}{POSIXt}(x, digits = c("secs", "mins", "hours", "days", "months", "years"),   \dots)
  \method{round}{POSIXt}(x, digits = c("secs", "mins", "hours", "days", "months", "years"),   \dots)
}
\arguments{
  \item{x}{date to be floored, ceilinged, truncated, or rounded}
  \item{digits}{unit to that is is rounded up or down to, called digit for compatibility with round or trunc generic method }
  \item{\dots}{further arguments to be passed to or from other methods}
}
\value{
  An object of class POSIXlt.
}
\description{
  functions imported from Hmisc 
}
\author{
  Charles Dupont
}

