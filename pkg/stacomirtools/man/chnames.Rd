\name{chnames}
\alias{chnames}
\title{This function replaces the variable names in a data.frame}
\usage{
  chnames(object, old_variable_name, new_variable_name)
}
\arguments{
  \item{object}{a data frame}

  \item{old_variable_name}{a character vector with old variables names}

  \item{new_variable_name}{a character vector with new variables names}
}
\value{
  object
}
\description{
  This function replaces the variable names in a data.frame
}
\author{
  Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
}
\examples{
df <- data.frame("var1" = c("blue","red"), "var2" = c("nice","ugly"))
colnames(df) # "var1" "var2"
df <- chnames(object = df, old_variable_name = c("var1","var2"), 
"new_variable_name" = c("color","beauty"))
colnames(df) # "color"  "beauty"
# the following will return an error, as the variable wrong_name is not in variable names
\dontrun{
chnames(object = df, old_variable_name = c("wrong_name"), 
"new_variable_name" = c("color")))
}
}

