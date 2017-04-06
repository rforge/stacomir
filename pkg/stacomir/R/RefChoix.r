#' Class "RefChoix"
#' 
#' RefChoix referential class allows to choose within several values with
#' radiobuttons
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefChoix", listechoice=character() ,label=character()
#' ,selected=integer())}.
#' @slot listechoice A character vector giving possible choices
#' @slot label A character, title of the box giving the possible choices
#' @slot selected An \code{Integer}  the initial selected value (as an index), first=1 used in gradio
#' @author cedric.briand"at"eptb-vilaine.fr
#' @family Referential objects
setClass(Class="RefChoix",representation= representation(listechoice="character",label="character",selected="integer"))

#' Loading method for Rechoice referential objects
#' 
#' @family Referential objects
#' @return An S4 object of class RefChoix
#' @param object An object of class RefChoix
#' @param vecteur A vector of name, see example code.
#' @param label Labels for the choices
#' @param selected An integer indicating which object is selected at launch
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#' object=new("RefChoix")
#' charge(object,vecteur=c("oui","non"),label="essai",selected=as.integer(1))
#' }
setMethod("charge",signature=signature("RefChoix"),definition=function(object,vecteur,label,selected) {
			object@listechoice=vecteur
			object@label=label
			object@selected=selected
			return(object)
		})
#' Choice method for Rechoice referential objects
#' 
#' Used by the graphical interface.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @param object An object of class \link{RefChoix-class}
#' @examples 
#' \dontrun{
#'  object=new("RefChoix")
#'  object<-charge(object,vecteur=c("oui","non"),label="essai",selected=as.integer(1))
#' win=gwindow(title="test refChoix")
#' group=ggroup(container=win,horizontal=FALSE)
#' choice(object) 
#' dispose(win)}
setMethod("choice",signature=signature("RefChoix"),definition=function(object) {
			hlist=function(h,...){
				valeurchoisie=svalue(choice)
				object@listechoice<-valeurchoisie
				assign("refchoice",object,envir_stacomi)
				funout(paste(object@label,"\n"))
			}
			frame_choice<-gframe(object@label)
			assign("frame_choice",frame_choice,envir=envir_stacomi)
			##=>selection de plusieurs caracteristiques
			add(group,frame_choice)
			list_libelle=fun_char_spe(object@listechoice)
			choice=gradio(items=list_libelle,selected=object@selected,horizontal=TRUE,container=frame_choice,handler=hlist)
		})

#' Choice_c method for Refchoix referential objects
#' @param object An object of class \link{RefListe-class}
#' @note the choice method assigns an object of class refList named refListe in the environment envir_stacomi
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples  
#' \dontrun{
#' object=new("RefListe")
#' object<-charge(object,vecteur=c("1","2"),label="please choose")
#' object<-choice_c(object)
#' }
setMethod("choice_c",signature=signature("RefChoix"),definition=function(object,selectedvalue) {
			
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