\name{ggplot2usr-package}
\title{ggplot2 user interface}
\description{ggplot_user_interface.R}
\details{\tabular{ll}{
Package: \tab ggplot2usr\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2009-11-04\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

package is intented to be used by beginners who will be able to access to the different layers of the ggplot2 package
is yet far from providing the full capability of the ggplot2, but will help in building the first graphs and print their formula
: this package is still in developpement and full of bugs, so far intended only for showing the aim of the project
has been inially developped within a french project, which builds a database for migratory fishes control stations, along with several graphical tools
help the users to 'view' their data}
\alias{Ggploti}
\docType{package}
\author{Cedric Briand \email{cedric.briand00@gmail.com}}
\references{\url{http://had.co.nz/ggplot2/}}
\keyword{package}
\seealso{\url{http://had.co.nz/ggplot2/}}
\section{TODO}{include the scales, test under package.}
\examples{mtcars=mtcars # the dataframe used are listed in the base environment .GlobalEnv  
ggplot2usr(data="mtcars")}
