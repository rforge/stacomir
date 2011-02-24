# Nom fichier :        RefChoix   (classe)

#' @title RefChoix referencial class allows to choose within several values with radiobuttons
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @slot listechoix="character" a character vector giving possible choices
#' @slot label="character" the title of the box giving the possible choices
#' @slot selected="integer"  the initial selected value (as an index) first=1 used in gradio
#' @example objet=new("RefChoix")
#' @method charge
#' @method choix
setClass(Class="RefChoix",representation= representation(listechoix="character",label="character",selected="integer"))

#' Loading method for Rechoix referential objects
#' @returnType S4 object
#' @return An S4 object of class RefChoix
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' #' @example 
#'  objet=new("RefChoix")
#' charge(objet,vecteur=c("oui","non"),label="essai",selected=as.integer(1))
setMethod("charge",signature=signature("RefChoix"),definition=function(objet,vecteur,label,selected) {
			objet@listechoix=vecteur
			objet@label=label
			objet@selected=selected
			return(objet)
		})
#' Choice method for Rechoix referential objects
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @example 
#'  objet=new("RefChoix")
#'  objet<-charge(objet,vecteur=c("oui","non"),label="essai",selected=as.integer(1))
#' win=gwindow(title="test refChoix")
#' group=ggroup(container=win,horizontal=FALSE)
#' choix(objet) 
#' dispose(win)
setMethod("choix",signature=signature("RefChoix"),definition=function(objet) {
			hlist=function(h,...){
				valeurchoisie=svalue(choix)
				objet@listechoix<-valeurchoisie
				assign("refchoix",objet,envir_stacomi)
				funout(paste(objet@label,"\n"))
			}
			frame_choix<<-gframe(objet@label)
			
			##=>selection de plusieurs caracteristiques
			add(group,frame_choix)
			list_libelle=fun_char_spe(objet@listechoix)
			choix=gradio(items=list_libelle,selected=objet@selected,horizontal=TRUE,container=frame_choix,handler=hlist)
		})

