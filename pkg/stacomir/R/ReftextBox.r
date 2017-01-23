#' RefTextBox referencial class 
#' 
#' allows to a put a value within a glabel
#' @author cedric.briand"at"eptb-vilaine.fr
#' @slot title="character" the title of the box giving the possible choices
#' @slot labels the logical parameters choice
#' @slot checked a vector
setClass(Class="RefTextBox",representation= representation(title="character",label="character"))

#' Loading method for ReTextBox referential objects
#' @param object An object of class \link{RefTextBox-class}
#' @param title A title for the frame
#' @param label A label for the TextBox
#' @return An S4 object of class RefTextBox
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("RefTextBox")
#' charge(object,title="un titre",label="20")
#' }
setMethod("charge",signature=signature("RefTextBox"),definition=function(object,title,label) {
			object@title=title
			object@label=label
			return(object)
		})


#' Choice method for ReTextBox referential objects
#' 
#' 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @param object An object of class \link{RefTextBox-class}
#' @examples 
#' \dontrun{
#' object=new("RefTextBox")
#' object<- charge(object,title="le titre",label="20")
#' win=gwindow(title="test RefTextBox")
#' group=ggroup(container=win,horizontal=FALSE)
#' choice(object) 
#' dispose(win)
#' }
setMethod("choice",signature=signature("RefTextBox"),definition=function(object) {
			hlist=function(h,...){
				object@label<-svalue(choice)
				assign("refTextBox",object,envir_stacomi)
				funout(paste("choice",object@label,"\n"))
			}
			
			frame_text<-gframe(object@title)	
			assign("frame_text",frame_text,.GlobalEnv)
			add(group,frame_text)
			choice=glabel(text=object@label,container=frame_text,handler=hlist,editable=TRUE)
			addhandlerchanged(choice,handler=hlist)
		})

#' Choice_c method for ReTextBox referential objects
#' 
#' 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @param object An object of class \link{RefTextBox-class}
#' @param value The value to set
setMethod("choice_c",signature=signature("RefTextBox"),definition=function(object,value) {
			object@label<-value
			return(object)
		})
