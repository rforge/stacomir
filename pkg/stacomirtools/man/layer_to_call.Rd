\name{layer_to_call}
\alias{layer_to_call}
\title{Function used to build layer calls and print their formula...}
\usage{layer_to_call(layer_name, layer_type, position, param, aes)}
\description{Function used to build layer calls and write print their formula}
\details{transfers arguments " " into NULL and the others in variable of class call
a character vector is also returned to write the proper formula}
\value{list list("char"=layer expression to be printed, "call" = a call to the layer object}
\author{cedric}
\arguments{\item{layer_name}{name of the layer}
\item{layer_type}{currently only "geom" or "stat" for layer type object}
\item{position}{a position as used in ggplot2}
\item{param}{parameters for a geom}
\item{aes}{aestetics for a geom}}
