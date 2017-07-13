\name{tab2df}
\alias{tab2df}
\title{Function to transform a ftable into dataframe but just keeping the counts, works with ftable of dim 2}
\usage{
  tab2df(tab)
}
\arguments{
  \item{tab}{a flat table}
}
\description{
  Function to transform a ftable into dataframe but just
  keeping the counts works with ftable of dim 2
}
\author{
  Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
}
\examples{
df <- data.frame("var1" = c("blue","red"), "var2" = c("nice","ugly"))
ftdf <- ftable(df)
tab2df(ftdf)
}
