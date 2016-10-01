# Nom fichier :        RefListe   (classe)
# Description          Classe permettant charger un choice dans une liste utilisee par un objectBilan
#' Class "RefListe"
#' 
#' Enables to load a "RefChoix" object fom a list given by a "Bilan" object
#' 
#' 
#' @slot liste choice="character" A vector of character to choose within a droplist
#' @slot label="character" the title of the box
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefListe", listechoice, label)}.  \describe{
#' \item{list("listechoice")}{Object of class \code{"character"}}\item{:}{Object
#' of class \code{"character"}} \item{list("label")}{Object of class
#' \code{"character"}}\item{:}{Object of class \code{"character"}} }
#' @author cedric.briand"at"eptb-vilaine.fr

#' @keywords classes
#' @family Referential objects
setClass(Class="RefListe",representation= representation(listechoice="character",label="character"))


#' Loading method for Refliste referential objects
#' @return An object of class RefListe
#' @slot object An object of class \link{RefListe-class}
#' @slot vecteur A character vector
#' @slot label A label for Refliste
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("RefListe")
#' charge(object)
#' }
setMethod("charge",signature=signature("RefListe"),definition=function(object,vecteur,label) {
			object@listechoice=vecteur
			object@label=label
			return(object)
		})
#' Choice method for RefListe referential objects
#' @note the choice method assigns an object of class refList named refListe in the environment envir_stacomi
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples  
#' \dontrun{
#'  object=new("RefListe")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object<-charge(object)
#' choice(object)
#' }
setMethod("choice",signature=signature("RefListe"),definition=function(object,is.enabled=TRUE) {
			hlist=function(h,...){
					valeurchoisie=svalue(choice)
					object@listechoice<-object@listechoice[list_libelle%in%valeurchoisie]
					#dispose(car)
					assign("refliste",object,envir_stacomi)
					funout(paste(object@label,"\n"))
				}
				frame_list<<-gframe(object@label)
				# TODO a modifier en assign() si je dois passer plusieurs listes puis les supprimer, il faudra alors detruire les listes par leur nom
				add(group,frame_list)
				list_libelle=fun_char_spe(object@listechoice)
				choice=gdroplist(items=list_libelle,container=frame_list,handler=hlist)
				enabled(frame_list)<-is.enabled
				gbutton("OK", container=frame_list,handler=hlist)
				})
