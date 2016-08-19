#' Validity check for RefAnnee 
#' 
#' validite_Annee tests for validity within the class
#' 
#' 
#' @param object An object of class \code{\linkS4class{RefAnnee}}
#' @return boolean The test for the object Refannee
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
validite_Annee=function(object)
{
	rep1= class(object@data)=="data.frame"
	rep2= class(object@annee_selectionnee)=="numeric"
	
	return(ifelse(rep1&rep2,TRUE,FALSE))
}
#definition de la classe

#' Year reference class
#' 
#' Class used to select one or several years 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefAnnee", data=data.frame(), annee_selectionnee=numeric())}.
#' @slot data A \code{data.frame} with the list of possible years selected as numerics
#' @slot annee_selectionnee A numeric vector
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
#' @keywords classes
#' @family Referential objects
#' @author cedric.briand"at"eptb-vilaine.fr
#' @examples
#' 
#' showClass("RefAnnee")
#' 
setClass(Class="RefAnnee",representation=
				representation(data="data.frame",annee_selectionnee="numeric"),
		validity=validite_Annee,
		prototype=prototype(data=data.frame(),annee_selectionnee=numeric()))

#' Loading method for RefAnnee referential objects
#' 
#' Selects year avalaible in the t_operation_ope table
#' @param object An object of class RefAnnee
#' @return object An object of class RefAnnee with slot data filled with the selected value
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples   object=new("RefAnnee")
#' charge(object)
#'  validObject( annee)
#' showMethods("charge")
setMethod("charge",signature=signature("RefAnnee"),definition=function(object){
			requete=new("RequeteODBC")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@sql=paste("select  DISTINCT ON (year) year from( select date_part('year', ope_date_debut) as year from ",
					get("sch",envir=envir_stacomi),
					"t_operation_ope) as tabletemp",sep="")
			requete<-stacomirtools::connect(requete)  # appel de la methode connect de l'object requeteODBC
			object@data<-requete@query
			return(object)
		})

#' choice method for RefAnnee referential 
#' 
#' Allows the selection of year and the assignment in environment envir_stacomi
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @param nomassign The name to be asssigned in envir_stacomi
#' @param funoutlabel The label that appears in funout
#' @param titleFrame Title for the frame
#' @param preselect The number of the year selected in the gdroplist (integer)
#' @examples  object=new("RefAnnee")
#' object<-charge(object)
#' win=gwindow(title="test refAnnee")
#' group=ggroup(container=win,horizontal=FALSE)
#' choix(object,nomassign="refAnnee",funoutlabel="essai",titleFrame="essai RefAnnee",preselect=1)
#' dispose(win)
setMethod("choix",
		signature=signature("RefAnnee"),definition=function(object,
				nomassign="refAnnee", 
				funoutlabel=get("msg",envir=envir_stacomi)$RefAnnee.2,
				titleFrame=get("msg",envir=envir_stacomi)$RefAnnee.1, 
				preselect=1){
			if (nrow(object@data) > 0){      
				hannee=function(h,...){      
					object@annee_selectionnee<-svalue(choix)					
					assign(nomassign,object,envir_stacomi)
					funout(funoutlabel)      
				}    
				frame_annee<<-gframe(titleFrame)    
				add(group,frame_annee)    
				annees=object@data$year    
				choix=gdroplist(annees,container=frame_annee,handler=hannee,selected=preselect)    
				gbutton("OK", container=frame_annee,handler=hannee)  
			} else { 
				funout(get("msg",envir=envir_stacomi)$RefAnnee.3,arret=TRUE)  
			}
		}) 
