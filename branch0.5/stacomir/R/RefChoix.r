#' Class "RefChoix"
#' 
#' RefChoix referential class allows to choose within several values with
#' radiobuttons
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefChoix", listechoix=character() ,label=character()
#' ,selected=integer())}.
#' @slot listechoix A character vector giving possible choices
#' @slot label A character, title of the box giving the possible choices
#' @slot selected An \code{Integer}  the initial selected value (as an index), first=1 used in gradio
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso Other referential classes 
#' \code{\linkS4class{RefAnnee}}
#' \code{\linkS4class{RefCheckBox}} 
#' \code{\linkS4class{RefChoix}}
#' \code{\linkS4class{RefCoe}} 
#' \code{\linkS4class{RefDC}}
#' \code{\linkS4class{RefDF}} 
#' \code{\linkS4class{RefListe}}
#' \code{\linkS4class{Refpar}} 
#' \code{\linkS4class{Refparqual}}
#' \code{\linkS4class{Refparquan}} 
#' \code{\linkS4class{RefPoidsMoyenPeche}}
#' \code{\linkS4class{RefStades}} 
#' \code{\linkS4class{RefStationMesure}}
#' \code{\linkS4class{RefTaxon}}
#' @family Referential objects
#' @examples
#' 
#' showClass("RefChoix")
#' 
setClass(Class="RefChoix",representation= representation(listechoix="character",label="character",selected="integer"))

#' Loading method for Rechoix referential objects
#' 
#' 
#' @family Referential objects
#' @return An S4 object of class RefChoix
#' @param object An object of class RefChoix
#' @param vecteur A vector of name, see example code.
#' @param selected An integer indicating which object is selected at launch
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' #' @examples 
#'  object=new("RefChoix")
#' charge(object,vecteur=c("oui","non"),label="essai",selected=as.integer(1))
setMethod("charge",signature=signature("RefChoix"),definition=function(object,vecteur,label,selected) {
			object@listechoix=vecteur
			object@label=label
			object@selected=selected
			return(object)
		})
#' Choice method for Rechoix referential objects
#' 
#' Used by the graphical interface.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#'  object=new("RefChoix")
#'  object<-charge(object,vecteur=c("oui","non"),label="essai",selected=as.integer(1))
#' win=gwindow(title="test refChoix")
#' group=ggroup(container=win,horizontal=FALSE)
#' choix(object) 
#' dispose(win)
setMethod("choix",signature=signature("RefChoix"),definition=function(object) {
			hlist=function(h,...){
				valeurchoisie=svalue(choix)
				object@listechoix<-valeurchoisie
				assign("refchoix",object,envir_stacomi)
				funout(paste(object@label,"\n"))
			}
			frame_choix<<-gframe(object@label)
			
			##=>selection de plusieurs caracteristiques
			add(group,frame_choix)
			list_libelle=fun_char_spe(object@listechoix)
			choix=gradio(items=list_libelle,selected=object@selected,horizontal=TRUE,container=frame_choix,handler=hlist)
		})

