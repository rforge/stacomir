# Nom fichier :        RefAnnee(classe)





#' validite_Annee tests for validity within the class
#' 
#' validite_Annee tests for validity within the class
#' 
#' 
#' @param object An object of class \code{\linkS4class{RefAnnee}}
#' @return the test for the object Refannee
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
validite_Annee=function(object)
{
	rep1= class(object@data)=="data.frame"
	rep2= class(object@annee_selectionnee)=="numeric"
	
	return(ifelse(rep1&rep2,TRUE,FALSE))
}
#definition de la classe

#' @title RefAnnee referential class to choose years
#' @name RefAnnee-class
#' @aliases RefAnnee-class RefAnnee

#' #' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefAnnee", data=data.frame(), annee_selectionnee=numeric())}.
#'#' @slot data="data.frame" the list of parameters
#' @slot annee_selectionnee="numeric"
#' @method charge
#' @method choix
#'  @seealso Other referential classes \code{\linkS4class{RefAnnee}}
#' \code{\linkS4class{RefCheckBox}} \code{\linkS4class{RefChoix}}
#' \code{\linkS4class{RefCoe}} \code{\linkS4class{RefDC}}
#' \code{\linkS4class{RefDF}} \code{\linkS4class{RefListe}}
#' \code{\linkS4class{Refpar}} \code{\linkS4class{Refparqual}}
#' \code{\linkS4class{Refparquan}} \code{\linkS4class{RefPoidsMoyenPeche}}
#' \code{\linkS4class{RefStades}} \code{\linkS4class{RefStationMesure}}
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

#' Loading method for RefAnnee referential objects, selects year avalaible in the t_operation_ope table
#' @returnType S4 object
#' @return An S4 object of class RefAnnee
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @examples   objet=new("RefAnnee")
#' charge(objet)
#'  validObject( annee)
#' showMethods("charge")
setMethod("charge",signature=signature("RefAnnee"),definition=function(objet){
			requete=new("RequeteODBC")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@sql=paste("select  DISTINCT ON (year) year from( select date_part('year', ope_date_debut) as year from ",
					get("sch",envir=envir_stacomi),
					"t_operation_ope) as tabletemp",sep="")
			requete=connect(requete)  # appel de la methode connect de l'objet requeteODBC
			objet@data<-requete@query
			return(objet)
		})

#' choice method for RefAnnee referential objects assign the object in envir_stacomi
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @param nomassign the name to be asssigned in envir_stacomi
#' @param funoutlabel the label that appears in funout
#' @param titleFrame title for the frame
#' @param preselect the number of the year selected in the gdroplist (integer)
#' @examples  objet=new("RefAnnee")
#' objet<-charge(objet)
#' win=gwindow(title="test refAnnee")
#' group=ggroup(container=win,horizontal=FALSE)
#' choix(objet,nomassign="refAnnee",funoutlabel="essai",titleFrame="essai RefAnnee",preselect=1)
#' dispose(win)
setMethod("choix",
		signature=signature("RefAnnee"),definition=function(objet,
				nomassign="refAnnee", 
				funoutlabel=get("msg",envir=envir_stacomi)$RefAnnee.2,
				titleFrame=get("msg",envir=envir_stacomi)$RefAnnee.1, 
				preselect=1){
			if (nrow(objet@data) > 0){      
				hannee=function(h,...){      
					objet@annee_selectionnee<-svalue(choix)					
					assign(nomassign,objet,envir_stacomi)
					funout(funoutlabel)      
				}    
				frame_annee<<-gframe(titleFrame)    
				add(group,frame_annee)    
				annees=objet@data$year    
				choix=gdroplist(annees,container=frame_annee,handler=hannee,selected=preselect)    
				gbutton("OK", container=frame_annee,handler=hannee)  
			} else { 
				funout(get("msg",envir=envir_stacomi)$RefAnnee.3,arret=TRUE)  
			}
		}) 
