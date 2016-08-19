# Nom fichier :        RefCheckBox   (classe)

#' @title RefCheckBox referencial class allows to choose for several parms with checkbox
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
#' @slot title="character" the title of the box giving the possible choices
#' @slot labels the logical parameters choice
#' @slot checked a vectore
#' @expamples objet=new("RefCheckBox")
#' @method charge
#' @method choice
setClass(Class="RefCheckBox",representation= representation(title="character",labels="character",checked="logical"),
		prototype=prototype(title="liste de choice",labels="choice",checked=FALSE))

#' Loading method for ReCheckBox referential objects
#' @returnType S4 object
#' @return An S4 object of class RefCheckBox
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
#' @expamples 
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
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
#' @expamples 
#' objet=new("RefCheckBox")
#' objet<- charge(objet,title="essai",labels=c("par1","par2","par3"),checked=c(TRUE,TRUE,TRUE))
#' win=gwindow(title="test RefCheckBox")
#' group=ggroup(container=win,horizontal=FALSE)
#' choice(objet) 
#' dispose(win)
setMethod("choice",signature=signature("RefCheckBox"),definition=function(objet) {
			hlist=function(h,...){
				i=h$action
				if (exists("refCheckBox",envir_stacomi)) {
					# on r�cup�re les valeurs de l'objet assign� pr�c�dement
					# car l'objet dans .GlobalEnv n'est pas � jour...
					objet<-get("refCheckBox",envir_stacomi)
				}
				objet@checked[i]<-svalue(the_choice[[i]])
				assign("refCheckBox",objet,envir_stacomi)
				funout(paste("choice",objet@labels[i],"\n"))
			}
			
			frame_check<<-gframe(objet@title)	
			##=>selection de plusieurs caracteristiques
			add(group,frame_check)
			the_choice=list()
			for(i in 1: length(objet@labels)){
				the_choice[[i]]=gcheckbox(text=objet@labels[i], action=i,checked = objet@checked[i],container=frame_check,handler=hlist)
			}
		})

