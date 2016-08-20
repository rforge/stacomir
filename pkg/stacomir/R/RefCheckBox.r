#' RefCheckBox referencial class 
#' 
#' referential class allows to choose for several parms with checkbox
#' @slot title A "character", the title of the box giving the possible choices
#' @slot labels The logical parameters choice
#' @slot checked A boolean vector
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefCheckBox", ...)}.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other referential classes 
#' \code{\linkS4class{RefAnnee}}
#' \code{\linkS4class{RefCheckBox}} 
#' \code{\linkS4class{RefChoix}}
#' \code{\linkS4class{RefCoe}} 
#' \code{\linkS4class{RefDC}}
#' \code{\linkS4class{RefDF}} 
#' \code{\linkS4class{RefListe}}
#' \code{\linkS4class{Refpar}} 
#' \code{\linkS4class{Refparqual}}
#' \code{\linkS4class{Refparquan}} 
#' \code{\linkS4class{RefPoidsMoyenPeche}}
#' \code{\linkS4class{RefStades}} 
#' \code{\linkS4class{RefStationMesure}}
#' \code{\linkS4class{RefTaxon}}
#' @family Referential objects
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setClass(Class="RefCheckBox",representation= representation(title="character",labels="character",checked="logical"),
		prototype=prototype(title="liste de choice",labels="choice",checked=FALSE))

#' Loading method for ReCheckBox referential objects
#' @return An S4 object of class RefCheckBox
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("RefCheckBox")
#' charge(object,title="essai",labels=c("par1","par2","par3"),checked=c(TRUE,TRUE,TRUE))
#' }
setMethod("charge",signature=signature("RefCheckBox"),definition=function(object,title,labels,checked) {
			if (length(labels) != length(checked)) stop ("les longeur de 'labels' et 'checked' sont differentes")
			object@title=title
			object@labels=labels
			object@checked=checked
			return(object)
		})
#' Choice method for ReCheckBox referential objects
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#' object=new("RefCheckBox")
#' object<- charge(object,title="essai",labels=c("par1","par2","par3"),checked=c(TRUE,TRUE,TRUE))
#' win=gwindow(title="test RefCheckBox")
#' group=ggroup(container=win,horizontal=FALSE)
#' choice(object) 
#' dispose(win)
#' '}
setMethod("choice",signature=signature("RefCheckBox"),definition=function(object) {
			hlist=function(h,...){
				i=h$action
				if (exists("refCheckBox",envir_stacomi)) {
					# on r�cup�re les valeurs de l'object assign� pr�c�dement
					# car l'object dans .GlobalEnv n'est pas � jour...
					object<-get("refCheckBox",envir_stacomi)
				}
				object@checked[i]<-svalue(the_choice[[i]])
				assign("refCheckBox",object,envir_stacomi)
				funout(paste("choice",object@labels[i],"\n"))
			}
			
			frame_check<<-gframe(object@title)	
			##=>selection de plusieurs caracteristiques
			add(group,frame_check)
			the_choice=list()
			for(i in 1: length(object@labels)){
				the_choice[[i]]=gcheckbox(text=object@labels[i], action=i,checked = object@checked[i],container=frame_check,handler=hlist)
			}
		})

