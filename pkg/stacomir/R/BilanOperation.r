#' Report on operations
#' 
#' Operations are monitoring operations generated between two dates. In the case of video monitoring
#' or similar, they can be instantaneous
#' 
#' @include RefDC.r
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanOperation")}.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Class \code{\linkS4class{Bilan_carlot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} 
#' \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} 
#' \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @concept Bilan Object 
#' @keywords classes
#' @export 
setClass(Class="BilanOperation",
		representation= representation(data="data.frame",
				dc="RefDC",
				horodatedebut="RefHorodate",
				horodatefin="RefHorodate"),
		prototype=prototype(data=data.frame(),
				dc=new("RefDC"),
				horodatedebut=new("RefHorodate"),
				horodatefin=new("RefHorodate")
				
		)
)

#' connect method for BilanOperation
#' 
#' @param object An object of class \link{BilanOperation-class}
#' load data from the operation table, one dataset per DC
#' @param silent Boolean, TRUE removes messages.
#' @return  An object of class \link{BilanOperation-class}
#' 
#' @author cedric.briand
setMethod("connect",signature=signature("BilanOperation"),definition=function(object,silent=FALSE) {
			# object=bilanOperation
			req<-new("RequeteODBCwheredate")
			req@baseODBC<-get("baseODBC",envir=envir_stacomi)
			lesdc<-object@dc@dc_selectionne			
			req@colonnedebut="ope_date_debut"
			req@colonnefin="ope_date_fin"
			req@order_by="ORDER BY ope_dic_identifiant, ope_date_debut"
			req@datedebut<-object@horodatedebut@horodate
			req@datefin<-object@horodatefin@horodate
			req@select<-paste("SELECT * FROM  ",get("sch",envir=envir_stacomi),"t_operation_ope ")		
			req@and=paste("AND ope_dic_identifiant in",stringr::str_c("(",stringr::str_c(lesdc,collapse=","),")"))
			req<-stacomirtools::connect(req) # appel de la methode connect de l'object ODBCWHEREDATE
			object@data<-req@query	
			if (!silent) funout(gettext("Loading data for operations",domain="R-stacomiR"))
			return(object)
		})


#' charge method for BilanOperation
#' 
#' 
#' used by the graphical interface to retrieve Referential classes
#' assigned to envir_stacomi
#' @param object An object of class \link{BilanOperation-class}
#' @param silent Keeps program silent
#' @return  An object of class \link{BilanOperation-class}
#' 
#' @author cedric.briand
setMethod("charge",signature=signature("BilanOperation"),definition=function(object,silent=FALSE) {
			# object<-bilanOperation
			if (exists("refDC",envir=envir_stacomi)) {
				object@dc<-get("refDC",envir=envir_stacomi)
			} else {
				funout(gettext("You need to choose a counting device, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}     
			
			if (exists("bilanOperation_date_debut",envir=envir_stacomi)) {
				object@horodatedebut@horodate<-get("bilanOperation_date_debut",envir=envir_stacomi)
			} else {
				funout(gettext("You need to choose the starting date\n",domain="R-stacomiR"),arret=TRUE)
			}
			
			if (exists("bilanOperation_date_fin",envir=envir_stacomi)) {
				object@horodatefin@horodate<-get("bilanOperation_date_fin",envir=envir_stacomi)
			} else {
				funout(gettext("You need to choose the ending date\n",domain="R-stacomiR"),arret=TRUE)
			}			
			assign("bilanOperation",object,envir=envir_stacomi)  
			return(object)
		})
