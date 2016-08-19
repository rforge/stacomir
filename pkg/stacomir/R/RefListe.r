# Nom fichier :        RefListe   (classe)
# Description          Classe permettant charger un choice dans une liste utilis�e par un objetBilan
#' @title RefListe referential class choose within a list
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
#' @slot liste choice="character" A vector of character to choose within a droplist
#' @slot label="character" the title of the box
#' @expamples objet=new("RefListe")
setClass(Class="RefListe",representation= representation(listechoice="character",label="character"))


#' Loading method for Refliste referential objects
#' @returnType S4 object
#' @return An object of class RefListe
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
#' @expamples 
#'  objet=new("RefListe")
#' charge(objet)
setMethod("charge",signature=signature("RefListe"),definition=function(objet,vecteur,label) {
			objet@listechoice=vecteur
			objet@label=label
			return(objet)
		})
#' Choice method for RefListe referential objects
#' @note the choice method assigns an object of class refList named refListe in the environment envir_stacomi
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
#' @expamples  
#'  objet=new("RefListe")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' objet<-charge(objet)
#' choice(objet)
setMethod("choice",signature=signature("RefListe"),definition=function(objet,is.enabled=TRUE) {
			hlist=function(h,...){
					valeurchoisie=svalue(choice)
					objet@listechoice<-objet@listechoice[list_libelle%in%valeurchoisie]
					#dispose(car)
					assign("refliste",objet,envir_stacomi)
					funout(paste(objet@label,"\n"))
				}
				frame_list<<-gframe(objet@label)
				# TODO � modifier en assign() si je dois passer plusieurs listes puis les supprimer, il faudra alors detruire les listes par leur nom
				add(group,frame_list)
				list_libelle=fun_char_spe(objet@listechoice)
				choice=gdroplist(items=list_libelle,container=frame_list,handler=hlist)
				enabled(frame_list)<-is.enabled
				gbutton("OK", container=frame_list,handler=hlist)
				})
