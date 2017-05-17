# Nom fichier :        RefListe   (classe)
# Description          Classe permettant charger un choice dans une liste utilisee par un objectBilan
#' Class "RefListe"
#' 
#' Enables to load a "RefChoix" object fom a list given by a "Bilan" object
#' 
#' 
#' @param liste choice="character" A vector of character to choose within a droplist
#' @param label="character" the title of the box
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefListe", listechoice, label)}.  \describe{
#' \item{list("listechoice")}{Object of class \code{"character"}}\item{:}{Object
#' of class \code{"character"}} \item{list("label")}{Object of class
#' \code{"character"}}\item{:}{Object of class \code{"character"}} }
#' @author cedric.briand"at"eptb-vilaine.fr

#' @keywords classes
#' @family Referential objects
setClass(Class="RefListe",representation= representation(listechoice="character",selectedvalue="character",label="character"))


#' Loading method for Refliste referential objects
#' @return An object of class RefListe
#' @param object An object of class \link{RefListe-class}
#' @param listechoice A character vector setting the possible values in which the user can select
#' @param label A label for Refliste
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("RefListe")
#' charge(object)
#' }
setMethod("charge",signature=signature("RefListe"),definition=function(object,listechoice,label) {
			object@listechoice=listechoice
			object@label=label
			return(object)
		})
#' Choice method for RefListe referential objects
#' @param object An object of class \link{RefListe-class}
#' @param is.enabled A boolean indicating whether the frame is enabled when first displayed, default to TRUE
#' @note the choice method assigns an object of class refList named refListe in the environment envir_stacomi
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples  
#' \dontrun{
#'  object=new("RefListe")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object<-charge(object,vecteur=c("choice1","choice2"),label="please choose")
#' choice(object)
#' }
setMethod("choice",signature=signature("RefListe"),definition=function(object,is.enabled=TRUE) {
			hlist=function(h,...){
				valeurchoisie=svalue(choice)
				object@selectedvalue<-object@listechoice[list_libelle%in%valeurchoisie]
				assign("refliste",object,envir_stacomi)
				funout(paste(object@label,"\n"))
			}
			frame_list<<-gframe(object@label)
			add(group,frame_list)
			list_libelle=fun_char_spe(object@listechoice)
			choice=gdroplist(items=list_libelle,container=frame_list,handler=hlist)
			enabled(frame_list)<-is.enabled
			gbutton("OK", container=frame_list,handler=hlist)
		})


#' Choice_c method for RefListe referential objects
#' @param object An object of class \link{RefListe-class}
#' @param selectedvalue TODO
#' @note the choice method assigns an object of class refList named refListe in the environment envir_stacomi
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples  
#' \dontrun{
#' object=new("RefListe")
#' object<-charge(object,vecteur=c("1","2"),label="please choose")
#' object<-choice_c(object)
#' }
setMethod("choice_c",signature=signature("RefListe"),definition=function(object,selectedvalue) {
			if (length(selectedvalue)>1) stop("valeurchoisie should be a vector of length 1")
			if (class (selectedvalue)=="numeric") selectedvalue<-as.character(selectedvalue)
			# the charge method must be performed before
			
			if ( !selectedvalue %in% object@listechoice ) {
				stop(stringr::str_c("The selected valeur,",selectedvalue," not in the list of possible values :",
								stringr::str_c(object@listechoice,collapse=",")))
			} else {
				object@selectedvalue<-selectedvalue
			}
			return(object)
		})
