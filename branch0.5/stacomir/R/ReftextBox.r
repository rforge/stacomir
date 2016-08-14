# Nom fichier :        RefTextBox   (classe)

#' @title RefTextBox referencial class 
#' 
#' allows to a put a value within a glabel
#' @author cedric.briand"at"eptb-vilaine.fr
#' @slot title="character" the title of the box giving the possible choices
#' @slot labels the logical parameters choice
#' @slot checked a vector
#' @examples object=new("RefTextBox")
setClass(Class="RefTextBox",representation= representation(title="character",label="character"))

#' Loading method for ReTextBox referential objects
#' @return An S4 object of class RefTextBox
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#'  object=new("RefTextBox")
#' charge(object,title="un titre",label="20")
setMethod("charge",signature=signature("RefTextBox"),definition=function(object,title,label) {
			object@title=title
			object@label=label
			return(object)
		})


#' Choice method for ReTextBox referential objects
#' 
#' 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' object=new("RefTextBox")
#' object<- charge(object,title="le titre",label="20")
#' win=gwindow(title="test RefTextBox")
#' group=ggroup(container=win,horizontal=FALSE)
#' choix(object) 
#' dispose(win)
setMethod("choix",signature=signature("RefTextBox"),definition=function(object) {
			hlist=function(h,...){
				object@label<-svalue(choix)
				assign("refTextBox",object,envir_stacomi)
				funout(paste("choix",object@label,"\n"))
			}
			
			frame_text<-gframe(object@title)	
			assign("frame_text",frame_text,.GlobalEnv)
			add(group,frame_text)
			choix=glabel(text=object@label,container=frame_text,handler=hlist,editable=TRUE)
			addhandlerchanged(choix,handler=hlist)
		})

