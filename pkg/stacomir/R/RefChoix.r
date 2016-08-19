# Nom fichier :        RefChoix   (classe)

#' @title RefChoix referencial class allows to choose within several values with radiobuttons
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
#' @slot listechoice="character" a character vector giving possible choices
#' @slot label="character" the title of the box giving the possible choices
#' @slot selected="integer"  the initial selected value (as an index) first=1 used in gradio
#' @expamples objet=new("RefChoix")
#' @method charge
#' @method choice
setClass(Class="RefChoix",representation= representation(listechoice="character",label="character",selected="integer"))

#' Loading method for Rechoice referential objects
#' @returnType S4 object
#' @return An S4 object of class RefChoix
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
#' #' @expamples 
#'  objet=new("RefChoix")
#' charge(objet,vecteur=c("oui","non"),label="essai",selected=as.integer(1))
setMethod("charge",signature=signature("RefChoix"),definition=function(objet,vecteur,label,selected) {
			objet@listechoice=vecteur
			objet@label=label
			objet@selected=selected
			return(objet)
		})
#' Choice method for Rechoice referential objects
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
#' @expamples 
#'  objet=new("RefChoix")
#'  objet<-charge(objet,vecteur=c("oui","non"),label="essai",selected=as.integer(1))
#' win=gwindow(title="test refChoix")
#' group=ggroup(container=win,horizontal=FALSE)
#' choice(objet) 
#' dispose(win)
setMethod("choice",signature=signature("RefChoix"),definition=function(objet) {
			hlist=function(h,...){
				valeurchoisie=svalue(choice)
				objet@listechoice<-valeurchoisie
				assign("refchoice",objet,envir_stacomi)
				funout(paste(objet@label,"\n"))
			}
			frame_choice<<-gframe(objet@label)
			
			##=>selection de plusieurs caracteristiques
			add(group,frame_choice)
			list_libelle=fun_char_spe(objet@listechoice)
			choice=gradio(items=list_libelle,selected=objet@selected,horizontal=TRUE,container=frame_choice,handler=hlist)
		})

