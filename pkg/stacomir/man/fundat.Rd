\name{fundat}
\title{statistics per time period}
\value{
a data frame with mean, max, and min calculated for each timesplit
}
\description{
   function called for bilamMigrationInterannelle objects
   renames columns replaces nulls,  
   and calculates reports with time period larger than day
}
\arguments{
  \item{dat}{a data frame}
  \item{timesplit}{"week"  "2 week" "month" as provided to seq.POSIXT}
}
\seealso{
\code{\linkS4class{Bilan_poids_moyen}}
}
\usage{fundat(dat, timesplit)}
