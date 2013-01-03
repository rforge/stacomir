\name{createmessage}
\docType{methods}
\alias{createmessage}
\alias{createmessage-method}
\title{createmessage method for RefMsg referential objects}
\value{
  An S4 object of class RefMsg
}
\description{
  createmessage method for RefMsg referential objects
}
\note{
  When coming from the database, " are now /", those at the
  beginning and end are turned into ", the others are
  single quote when they are to be pasted within the text
  as code example. The remainder "c("a","b","c") are
  rebuilt into vectors by the function
}
\author{
  Cedric Briand \email{cedric.briand00@gmail.com}
}

