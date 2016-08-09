# Nom fichier :        RefListe   (classe)
# Description          Classe permettant charger un choix dans une liste utilisée par un objetBilan
#' Class "RefListe"
#' 
#' Enables to load a "RefChoix" object fom a list given by a "Bilan" object
#' 
#' 
#' @name RefListe-class
#' @aliases RefListe-class RefListe

#' @slot liste choix="character" A vector of character to choose within a droplist
#' @slot label="character" the title of the box
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefListe", listechoix, label)}.  \describe{
#' \item{list("listechoix")}{Object of class \code{"character"}}\item{:}{Object
#' of class \code{"character"}} \item{list("label")}{Object of class
#' \code{"character"}}\item{:}{Object of class \code{"character"}} }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso Other referential classes \code{\linkS4class{RefAnnee}}
#' \code{\linkS4class{RefCheckBox}} \code{\linkS4class{RefChoix}}
#' \code{\linkS4class{RefCoe}} \code{\linkS4class{RefDC}}
#' \code{\linkS4class{RefDF}} \code{\linkS4class{RefListe}}
#' \code{\linkS4class{Refpar}} \code{\linkS4class{Refparqual}}
#' \code{\linkS4class{Refparquan}} \code{\linkS4class{RefPoidsMoyenPeche}}
#' \code{\linkS4class{RefStades}} \code{\linkS4class{RefStationMesure}}
#' \code{\linkS4class{RefTaxon}}
#' @keywords classes
#' @family Referential objects
#' @examples
#' 
#' showClass("RefListe")
#' 
setClass(Class="RefListe",representation= representation(listechoix="character",label="character"))


#' Loading method for Refliste referential objects
#' @returnType S4 object
#' @return An object of class RefListe
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @examples 
#'  objet=new("RefListe")
#' charge(objet)
setMethod("charge",signature=signature("RefListe"),definition=function(objet,vecteur,label) {
			objet@listechoix=vecteur
			objet@label=label
			return(objet)
		})
#' Choice method for RefListe referential objects
#' @note the choice method assigns an object of class refList named refListe in the environment envir_stacomi
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @examples  
#'  objet=new("RefListe")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' objet<-charge(objet)
#' choix(objet)
setMethod("choix",signature=signature("RefListe"),definition=function(objet,is.enabled=TRUE) {
			hlist=function(h,...){
					valeurchoisie=svalue(choix)
					objet@listechoix<-objet@listechoix[list_libelle%in%valeurchoisie]
					#dispose(car)
					assign("refliste",objet,envir_stacomi)
					funout(paste(objet@label,"\n"))
				}
				frame_list<<-gframe(objet@label)
				# TODO à modifier en assign() si je dois passer plusieurs listes puis les supprimer, il faudra alors detruire les listes par leur nom
				add(group,frame_list)
				list_libelle=fun_char_spe(objet@listechoix)
				choix=gdroplist(items=list_libelle,container=frame_list,handler=hlist)
				enabled(frame_list)<-is.enabled
				gbutton("OK", container=frame_list,handler=hlist)
				})
