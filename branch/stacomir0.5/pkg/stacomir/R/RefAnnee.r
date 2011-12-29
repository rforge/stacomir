# Nom fichier :        RefAnnee(classe)

#' validite_Annee tests for validity within the class
#' @param object 
#' @returnType logical
#' @return the test for the object Refannee
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
validite_Annee=function(object)
{
	rep1= class(object@data)=="data.frame"
	rep2= class(object@annee_selectionnee)=="numeric"
	
	return(ifelse(rep1&rep2,TRUE,FALSE))
}
#definition de la classe

#' @title RefAnnee referencial class
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @slot data="data.frame" the list of parameters
#' @method charge
#' @method choix
#' @example objet=new("RefAnnee")
setClass(Class="RefAnnee",representation=
				representation(data="data.frame",annee_selectionnee="numeric"),
		validity=validite_Annee,
		prototype=prototype(data=data.frame(),annee_selectionnee=numeric()))

#' Loading method for RefAnnee referential objects, selects year avalaible in the t_operation_ope table
#' @returnType S4 object
#' @return An S4 object of class RefAnnee
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @example   objet=new("RefAnnee")
#' charge(objet)
#'  validObject( annee)
#' showMethods("charge")
setMethod("charge",signature=signature("RefAnnee"),definition=function(objet){
			requete=new("RequeteODBC")
			requete@baseODBC=baseODBC
			requete@sql=paste("select  DISTINCT ON (year) year from( select date_part('year', ope_date_debut) as year from ",
					baseODBC[2],
					".t_operation_ope) as tabletemp")
			requete=connect(requete)  # appel de la methode connect de l'objet requeteODBC
			objet@data<-requete@query
			return(objet)
		})

#' choice method for RefAnnee referential objects assign the object in envir_stacomi
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @param nomassign the name to be asssigned in envir_stacomi
#' @param funoutlabel the label that appears in funout
#' @param titleFrame title for the frame
#' @param preselect the number of the year selected in the gdroplist (integer)
#' @example  objet=new("RefAnnee")
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