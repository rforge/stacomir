# Nom fichier :        RefTextBox   (classe)

#' @title RefTextBox referencial class allows to a value within a glabel
#' @author cedric.briand"at"eptb-vilaine.fr
#' @slot title="character" the title of the box giving the possible choices
#' @slot labels the logical parameters choice
#' @slot checked a vectore
#' @examples objet=new("RefTextBox")
#' @method charge
#' @method choix
setClass(Class="RefTextBox",representation= representation(title="character",label="character"))

#' Loading method for ReTextBox referential objects
#' @returnType S4 object
#' @return An S4 object of class RefTextBox
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @examples 
#'  objet=new("RefTextBox")
#' charge(objet,title="un titre",label="20")
setMethod("charge",signature=signature("RefTextBox"),definition=function(objet,title,label) {
			objet@title=title
			objet@label=label
			return(objet)
		})
#' Choice method for ReTextBox referential objects
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @examples 
#' objet=new("RefTextBox")
#' objet<- charge(objet,title="le titre",label="20")
#' win=gwindow(title="test RefTextBox")
#' group=ggroup(container=win,horizontal=FALSE)
#' choix(objet) 
#' dispose(win)
setMethod("choix",signature=signature("RefTextBox"),definition=function(objet) {
			hlist=function(h,...){
				objet@label<-svalue(choix)
				assign("refTextBox",objet,envir_stacomi)
				funout(paste("choix",objet@label,"\n"))
			}
			
			frame_text<-gframe(objet@title)	
			assign("frame_text",frame_text,.GlobalEnv)
			add(group,frame_text)
			choix=glabel(text=objet@label,container=frame_text,handler=hlist,editable=TRUE)
			addhandlerchanged(choix,handler=hlist)
		})

