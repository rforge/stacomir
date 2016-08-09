# Nom fichier :        Ref_periode.R

#' Class "Refperiode" referential class
#' 
#' Refperiode referential class to choose a period
#' 
#' 
#' @name Refperiode-class
#' @aliases Refperiode-class Refperiode

#' @note pgval are used by seq.POSIXT
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Refperiode", ...)}.
#' @keywords classes
#' @slot data="data.frame" providing correspondance between period and their english names
#' @family Referential objects
#' @examples
#' 
#' showClass("Refperiode")
#' new("Refperiode")
setClass(Class="Refperiode",representation=
      representation(
          data="data.frame"          
      ),
		  prototype=prototype(
          data=data.frame("id"=c("jour","semaine","quinzaine","mois"),
          "pgval"=c("day","week","2 week","month"))
      )
)
#' Returns the POSIXt value of a given name
#' @returnType "character"
#' @return "a character to be used in seq.POSIXt
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @examples 
#'  getvalue(new("Refperiode"),"quinzaine")
setMethod("getvalue",signature=signature("Refperiode"), definition=function(objet,id,...)
  {return(as.character(objet@data[objet@data$id==id,"pgval"]))
  } ) 