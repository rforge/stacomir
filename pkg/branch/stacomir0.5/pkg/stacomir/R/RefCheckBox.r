# Nom fichier :        RefCheckBox   (classe)

#' @title RefCheckBox referencial class allows to choose for several parms with checkbox
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @slot title="character" the title of the box giving the possible choices
#' @slot labels the logical parameters choice
#' @slot checked a vectore
#' @example objet=new("RefCheckBox")
#' @method charge
#' @method choix
setClass(Class="RefCheckBox",representation= representation(title="character",labels="character",checked="logical"),
		prototype=prototype(title="liste de choix",labels="choix",checked=FALSE))

#' Loading method for ReCheckBox referential objects
#' @returnType S4 object
#' @return An S4 object of class RefCheckBox
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @example 
#'  objet=new("RefCheckBox")
#' charge(objet,title="essai",labels=c("par1","par2","par3"),checked=c(TRUE,TRUE,TRUE))
setMethod("charge",signature=signature("RefCheckBox"),definition=function(objet,title,labels,checked) {
			if (length(labels) != length(checked)) stop ("les longeur de 'labels' et 'checked' sont differentes")
			objet@title=title
			objet@labels=labels
			objet@checked=checked
			return(objet)
		})
#' Choice method for ReCheckBox referential objects
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @example 
#' objet=new("RefCheckBox")
#' objet<- charge(objet,title="essai",labels=c("par1","par2","par3"),checked=c(TRUE,TRUE,TRUE))
#' win=gwindow(title="test RefCheckBox")
#' group=ggroup(container=win,horizontal=FALSE)
#' choix(objet) 
#' dispose(win)
setMethod("choix",signature=signature("RefCheckBox"),definition=function(objet) {
			hlist=function(h,...){
				i=h$action
				if (exists("refCheckBox",envir_stacomi)) {
					# on récupère les valeurs de l'objet assigné précédement
					# car l'objet dans .GlobalEnv n'est pas à jour...
					objet<-get("refCheckBox",envir_stacomi)
				}
				objet@checked[i]<-svalue(choix[[i]])
				assign("refCheckBox",objet,envir_stacomi)
				funout(paste("choix",objet@labels[i],"\n"))
			}
			
			frame_check<<-gframe(objet@title)	
			##=>selection de plusieurs caracteristiques
			add(group,frame_check)
			choix=list()
			for(i in 1: length(objet@labels)){
				choix[[i]]=gcheckbox(text=objet@labels[i], action=i,checked = objet@checked[i],container=frame_check,handler=hlist)
			}
		})

