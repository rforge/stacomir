\name{fungraphstades}
\alias{fungraphstades}
\title{Main function for class Bilan_stades_pigm allowing to calculate and then draw the graphs}
\usage{
  fungraphstades(tablestades, retrocalcul = TRUE,
    datatempsal, points = TRUE, nb = TRUE,
    graphstades = TRUE, lmax = 1, labelretro,
    labelgraphstades, phi, maxVIA3 = 10, dates, Vparm,
    effectifs)
}
\arguments{
  \item{tablestades}{a data frame with stages VB VIA0 VIA1 VIA2 VIA3}

  \item{retrocalcul}{Logical TRUE or FALSE, do you want to retrocalculate when the glass eel have arrived in the estuary, in this case provide datatempsal,
  data for temperature and salinity}
 
  \item{datatempsal}{to draw the graph of recalculated dates of arrival, provide this data.frame, format graphique des durees en estuaire, format [,c("date","temperature","salinite")]}

  \item{points}{logical, do you want to draw the points on the cumulative pigmentation graph}

  \item{nb}{Do you want to write number in sample in the pigmentation stage graph}

  \item{graphstades}{do you want to draw the graph of cumulated stage}

  \item{lmax}{parameter for retrocalcul graph, see \link{fnstade} scale parameter for the graphical function, use 0.8 to avoid overlapping of the polygons from several samples or dates} 

  \item{labelretro}{label for retrocalcul graph, can be changed in the graphical interface}

  \item{labelgraphstades}{label for stage graph, can be changed in the graphical interface}

  \item{phi}{table of pigmentation time and dates format "\%d/\%m/\%Y"}

  \item{maxVIA3}{10, maximum value of pigment time for VIA3, limits the duration of this longer stage }

  \item{dates}{}

  \item{Vparm}{parameters for pigment stage function}
  
  \item{effectifs}{logical : do you want to display numbers on the graph}
}
\description{
  see R code for details
}
\author{
  Cedric Briand \email{cedric.briand00@gmail.com}
}

