\name{trunc.POSIXt}
\alias{ceil.POSIXt}
\alias{trunc.POSIXt}
\alias{round.POSIXt}
\title{trunc.POSIXT and ceil.POSIXt function imported from Hmisc that cannot be loaded in Rforge}
\usage{
  \method{trunc.POSIXt}{POSIXt}(x, units = c("secs", "mins", "hours", "days", "months", "years"),  \dots)
  \method{ceil.POSIXt}{POSIXt}(x, units = c("secs", "mins", "hours", "days", "months", "years"),   \dots)
  \method{round.POSIXt}{POSIXt}(x, units = c("secs", "mins", "hours", "days", "months", "years"),   \dots)
}
\arguments{
  \item{x}{date to be floored, ceilinged, truncated, or rounded}
  \item{units}{unit to that is is rounded up or down to. }
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

