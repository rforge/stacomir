# Nom fichier :        Ref_periode.R

#' Class "Refperiode" referential class
#' 
#' Refperiode referential class to choose a period
#' 
#' @note pgval are used by seq.POSIXT
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Refperiode", ...)}.
#' @keywords classes
#' @slot data="data.frame" providing correspondance between period and their english names
#' @family Referential objects
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
#' @param object An object of class \link{Refperiode-class}
#' @param id one of "jour", "semaine", "quinzaine", "mois"
#' @return "a character to be used in seq.POSIXt
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  getvalue(new("Refperiode"),"quinzaine")
#' }
#' @export
setMethod("getvalue",signature=signature("Refperiode"), definition=function(object,id)
  {return(as.character(object@data[object@data$id==id,"pgval"]))
  } ) 