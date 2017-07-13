\name{killfactor}
\alias{killfactor}
\title{very usefull function remove factor that appear, noticeably after loading with 'ODBC'}
\usage{
  killfactor(df)
}
\arguments{
  \item{df}{a data.frame}
}
\value{
  df
}
\description{
  function used to remove factors that appear, noticeably after loading with 'ODBC'
}
\author{
  Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
}
\examples{
df <- data.frame("var1" = c("blue","red"),"var2"=c("nice","ugly"))
df[,"var1"] <- as.factor(df[,"var1"])
df[,"var2"] <- as.factor(df[,"var2"])
df <- killfactor(df)
apply(df,1,function(x) is.factor(x)) # FALSE FALSE
}
