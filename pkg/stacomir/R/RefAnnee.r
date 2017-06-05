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
#' @include create_generic.r
#' @slot data A \code{data.frame} with the list of possible years selected as numerics
#' @slot annee_selectionnee A numeric vector
#' @keywords classes
#' @family Referential objects
#' @author cedric.briand"at"eptb-vilaine.fr
setClass(Class="RefAnnee",representation=
				representation(data="data.frame",annee_selectionnee="numeric"),
		validity=validite_Annee,
		prototype=prototype(data=data.frame(),annee_selectionnee=numeric()))

#' Loading method for RefAnnee referential objects
#' 
#' Selects year avalaible either in the bjo table (if ObjetBilan==BilanMigrationInterannelle) or in the t_operation_ope table
#' @param object An object of class RefAnnee
#' @param objectBilan The object Bilan, default "Bilan_poids_moyen" other possible value BilanMigrationInterAnnuelle
#' @return object An object of class RefAnnee with slot data filled with the selected value
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples   
#' \dontrun{
#' object=new("RefAnnee")
#' charge(object)
#'  validObject(annee)
#' showMethods("charge")
#' }
setMethod("charge",signature=signature("RefAnnee"),definition=function(object,objectBilan="Bilan_poids_moyen"){
			requete=new("RequeteODBC")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			if (objectBilan=="BilanMigrationInterAnnuelle") {
				if (exists("refDC",envir_stacomi)) {
					dc<-get("refDC",envir_stacomi)
					and1<-paste(" AND bjo_dis_identifiant =",dc@dc_selectionne)
				} else {
					and1<-""
				}
				if (exists("refTaxon",envir_stacomi)) {
					taxons<-get("refTaxon",envir_stacomi)
					and2<-stringr::str_c(" AND bjo_tax_code ='",taxons@data$tax_code,"'")
				} else {      
					and2<-""
				}
				if (exists("refStades",envir_stacomi)){
					stades<-get("refStades",envir_stacomi)
					and3<-stringr::str_c(" AND bjo_std_code ='",stades@data$std_code,"'")
				} else 
				{
					and3=""
				}
				requete@sql=paste("select  DISTINCT ON (bjo_annee) bjo_annee from ",
						get("sch",envir=envir_stacomi),
						"t_bilanmigrationjournalier_bjo where bjo_identifiant>0 ",
						# I want and statements to not have to choose the order
						# the where statement is always verified
						and1,and2,and3, sep="")	
			} else if (objectBilan=="Bilan_poids_moyen") {
				requete@sql=paste("select  DISTINCT ON (year) year from( select date_part('year', ope_date_debut) as year from ",
						get("sch",envir=envir_stacomi),
						"t_operation_ope) as tabletemp",sep="")
			} else if (objectBilan=="BilanAnnuels") {
				if (exists("refDC",envir_stacomi)) {
					dc<-get("refDC",envir_stacomi)
					and1<-paste(" AND ope_dic_identifiant in ",vector_to_listsql(dc@dc_selectionne))
				} else {
					and1<-""
				}
				if (exists("refTaxon",envir_stacomi)) {
					taxons<-get("refTaxon",envir_stacomi)
					and2<-stringr::str_c(" AND lot_tax_code in ",vector_to_listsql(taxons@data$tax_code))
				} else {      
					and2<-""
				}
				if (exists("refStades",envir_stacomi)){
					stades<-get("refStades",envir_stacomi)
					and3<-stringr::str_c(" AND lot_std_code in ",vector_to_listsql(stades@data$std_code))
				} else 
				{
					and3=""
				}
				requete@sql=paste("select  DISTINCT ON (year) year from (select date_part('year', ope_date_debut) as year from ",
						get("sch",envir=envir_stacomi),
						"t_operation_ope JOIN ",
						get("sch",envir=envir_stacomi),
						"t_lot_lot on lot_ope_identifiant=ope_identifiant",
						" WHERE lot_lot_identifiant is null",
						and1,and2,and3, ") as tabletemp", sep="")				
			} else {
				funout(gettextf("Not implemented for objectBilan = %s",objectBilan),arret=TRUE)
			}
			requete<-stacomirtools::connect(requete)  # appel de la methode connect de l'object requeteODBC
			object@data<-requete@query
			return(object)
		})

#' choice method for RefAnnee referential 
#' 
#' Allows the selection of year and the assignment in environment envir_stacomi
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @param object An object of class \link{RefAnnee-class}
#' @param nomassign The name to be asssigned in envir_stacomi
#' @param funoutlabel The label that appears in funout
#' @param titleFrame Title for the frame
#' @param preselect The number of the year selected in the gdroplist (integer)
#' @examples  
#' \dontrun{
#' object=new("RefAnnee")
#' object<-charge(object)
#' win=gwindow(title="test refAnnee")
#' group=ggroup(container=win,horizontal=FALSE)
#' choice(object,nomassign="refAnnee",funoutlabel="essai",titleFrame="essai RefAnnee",preselect=1)
#' dispose(win)
#' }
setMethod("choice",
		signature=signature("RefAnnee"),definition=function(object,
				nomassign="refAnnee", 
				funoutlabel=gettext("Year selected\n",domain="R-stacomiR"),
				titleFrame=gettext("Year choice",domain="R-stacomiR"), 
				preselect=1){
			if (nrow(object@data) > 0){      
				hannee=function(h,...){      
					object@annee_selectionnee<-svalue(choice)					
					assign(nomassign,object,envir_stacomi)
					funout(funoutlabel)      
				}    
				group<-get("group",envir=envir_stacomi)
				frame_annee<-gframe(titleFrame) 
				assign("frame_annee",frame_annee,envir=envir_stacomi)
				add(group,frame_annee)    
				annees=object@data$year    
				choice=gdroplist(annees,container=frame_annee,handler=hannee,selected=preselect)    
				gbutton("OK", container=frame_annee,handler=hannee)  
			} else { 
				funout(gettext("Problem when loading data or no data in the database (ODBC link ?)",domain="R-stacomiR"),arret=TRUE)  
			}
		}) 


#' choice_c method for RefAnnee referential from the command line
#' 
#' The choice_c method will issue a warning if the year is not present in the database
#' Allows the selection of year and the assignment in environment envir_stacomi
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @param object An object of class \link{RefAnnee-class}
#' @param annee The year to select, either as a character or as a numeric
#' @param nomassign The name to be asssigned in envir_stacomi
#' @param funoutlabel The label that appears in funout
#' @param silent Stops messages from being displayed if silent=TRUE, default FALSE
#' @examples  
#' \dontrun{
#' object=new("RefAnnee")
#' object<-charge(object)
#' win=gwindow(title="test refAnnee")
#' group=ggroup(container=win,horizontal=FALSE)
#' choice(object,nomassign="refAnnee",funoutlabel="essai",titleFrame="essai RefAnnee",preselect=1)
#' dispose(win)
#' }
setMethod("choice_c",
		signature=signature("RefAnnee"),definition=function(object,
				annee,
				nomassign="refAnnee", 
				funoutlabel=gettext("Year selected\n",domain="R-stacomiR"),
				silent=FALSE
		){
			if (length(annee)>1) stop("horodate should be a vector of length 1")
			if (class (annee)=="character") annee<-as.numeric(annee)
			# the charge method must be performed before
			gettext("no year",domain="R-stacomiR")
			if ( !annee %in% object@data[,1] ) {
				
				warning(stringr::str_c("Attention, year ",annee," is not available in the database, available years :",
								ifelse(length(object@data$bjo_annee)==0,gettext(" none, were you lazy?",domain="R-stacomiR"),
								stringr::str_c(object@data$bjo_annee,collapse=","))))
			}
				object@annee_selectionnee<-annee
			
			assign(nomassign,object,envir_stacomi)
			if (! silent) funout(funoutlabel)  	
			return(object)
		}) 
