\name{ggplot2usr}
\alias{ggplot2usr}
\title{Main launch function...}
\usage{ggplot2usr(data, envir=.GlobalEnv)}
\description{Main launch function}
\arguments{
\item{data}{the data frame displayed as a first choice by the ggplot2usr interface}
\item{envir}{the environment}}
\examples{
 data("mtcars") # the dataframe used are listed in the base environment .GlobalEnv 
 data("diamonds") # so that at least two dataframes appear in the list
 ggplot2usr(data="mtcars")
# at this stage you should see a frame with the following content
# BUTTON FRAME
# button geom<>stat => switches between geom and stat
# button Undo => you've done it wrong and want to start over with a simple geom_point
# checkbox Build layer => If checked this layer will be added to the plot
# button Graph => this is the button you will click to see the result (in a plot frame) and the formula used printed in the console  
# button data => updates the list of data and reloads it
# GEOM/STAT FRAME
# here you select the kind of layer you wish to use. Defaut aes and parameters will be loaded in the aes frame
# AES FRAME
# you have to choose the format of the required aesthetic for the layer
# for other aesthetics, you can choose either a variable from the dataframe (ex color<-cyl) or a parameter (ex :color<-blue),
# the choice of an aesthetic will override the choice of a parameter
# if the parameter differs from default, it will appear in the printed formula
# DATA
# lists all datasets in the user environment, if the column differ between two dataframes, a warning /
# confirmation will be issued.
# LAYOUT
# choice of the layout see \url{http://had.co.nz/ggplot2/}
# POSITION
# position \url{http://had.co.nz/ggplot2/}
}
\author{Cedric Briand \email{cedric.briand00@gmail.com}}
