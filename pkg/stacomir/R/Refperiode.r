# Nom fichier :        Ref_periode.R

#' @title Refperiode referential class to choose a period
#' @note pgval are used by seq.POSIXT
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @slot data="data.frame" providing correspondance between period and their english names
#' @expamples objet = new("Refperiode")
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
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @expamples 
#'  getvalue(new("Refperiode"),"quinzaine")
setMethod("getvalue",signature=signature("Refperiode"), definition=function(objet,id,...)
  {return(as.character(objet@data[objet@data$id==id,"pgval"]))
  } ) 