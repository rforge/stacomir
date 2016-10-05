# Nom fichier :        RefPar  (classe)
# Contact :            cedric.briand"at"eptb-vilaine.fr
# Date de creation :   22/03/2009 21:14:14

#TODO  selection de plusieurs caracteristiques
#' Class "Refpar"
#' 
#' Class enabling to load the list of parameters and select one of them
#' 
#' @slot data A data.frame
#' @slot par_selectionne A character vector corresponding to par_code
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Refpar", data)}.  \describe{ \item{list("data")}{Object of class
#' \code{"data.frame"} ~ All the parameters stored in the
#' database}\item{:}{Object of class \code{"data.frame"} ~ All the parameters
#' stored in the database} }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @keywords classes
#' @slot data="data.frame" the list of parameters
#' @family Referential objects
setClass(Class="Refpar",representation= representation(data="data.frame",par_selectionne="character"))


setValidity("Refpar",method=function(object){
			if (length(object@par_selectionne)!=0){		
				if (nrow(object@data)>0) {
					concord<-object@par_selectionne%in%object@data$par_code					
					if (any(!concord)){
						return(paste("No data for par",object@par_selectionne[!concord]))
						
					} else {
						return(TRUE)
					}
				} else {
					return("You tried to set a value for par_selectionne without initializing the data slot")
				}
			}  else return(TRUE)
			
		}   
)
#' Loading method for Repar referential objects
#' @param object An object of class \link{Refpar-class}
#' @return An S4 object of class Refpar
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("Refpar")
#' charge(object)
#' }
setMethod("charge",signature=signature("Refpar"),definition=function(object) {
			requete=new("RequeteODBC")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@sql= paste("SELECT par_code, par_nom  from ref.tg_parametre_par")
			requete<-stacomirtools::connect(requete)
			funout(get("msg",envir=envir_stacomi)$Refpar.1)
			object@data<-requete@query
			return(object)
		})
#' Loading method for Repar referential objects searching only those parameters existing for a DC, a Taxa, and a stage
#' @param object An object of class \link{Refpar-class}
#' @param dc_selectionne A counting device selected for the bilan 
#' @param taxon_selectionne The taxa selected for the bilan
#' @param stade_selectionne The stage selected for the bilan
#' @return An S4 object of class Refpar
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("Refpar")
#' charge_avec_filtre(object,dc_selectionne=6,taxon_selectionne=2038,stade_selectionne=c("AGJ","CIV")
#' }
setMethod("charge_avec_filtre",signature=signature("Refpar"),definition=function(object,dc_selectionne,
				taxon_selectionne,
				stade_selectionne) {
			requete=new("RequeteODBCwhere")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@select=paste("SELECT DISTINCT ON (par_code) par_code, par_nom", 
					" FROM ",get("sch",envir=envir_stacomi),"tg_dispositif_dis",
					" JOIN ",get("sch",envir=envir_stacomi),"t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"t_lot_lot on lot_ope_identifiant=ope_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"tj_caracteristiquelot_car on car_lot_identifiant=lot_identifiant",
					" JOIN ref.tg_parametre_par on par_code=car_par_code",sep="")
			requete@where=paste("where dis_identifiant in ",vector_to_listsql(dc_selectionne))
			requete@and=paste("and lot_tax_code in",vector_to_listsql(taxon_selectionne),
					" and lot_std_code in ",vector_to_listsql(stade_selectionne),sep="")
			requete@order_by="ORDER BY par_code"  
			requete<-stacomirtools::connect(requete)  # appel de la methode connect de l'object requeteODBC
			object@data<-requete@query
			if (nrow(object@data)==0) funout(get("msg",envir=envir_stacomi)$Refpar.2,arret=TRUE)
			return(object)
		})
#' Choice method for Refpar referential objects
#' @param object An object of class \link{Refpar-class}
#' @param label The label that will be displayed in the message frame or as output text
#' @param nomassign The assignment name in envir_stacomi
#' @param frameassign The name of the frame used for assignement in .GlobalEnv
#' @param is.enabled Default TRUE.
#' @note the choice method assigns an object of class Refpar named refpar in the environment envir_stacomi
#' @note this method choice is also on daughter classes Refparquan, hence the parameters, however it was redefined in refparqual
#' @note to load the possible values of qualitative parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples  
#' \dontrun{
#'  object=new("Refpar")
#'  win=gwindow()
#'  group=ggroup(container=win,horizontal=FALSE)
#'  object<-charge(object)
#'  choice(object)
#' }
setMethod("choice",signature=signature("Refpar"),definition=function(object,
				label="Choix d'une caracteristique de lot",
				nomassign="refpar",
				frameassign="frame_par",
				is.enabled=TRUE) {
			if (nrow(object@data) > 0){
				hcar=function(h,...){
					carchoisi=svalue(choice)
					object@par_selectionne<-carchoisi
					#object@data<-object@data[car_libelle%in%carchoisi ,]
					assign(nomassign,object,envir_stacomi)
					funout(get("msg",envir=envir_stacomi)$Refpar.3)
				}
				#frame_par<<-gframe(label)
				assign(frameassign,gframe(label,horizontal=FALSE),envir= .GlobalEnv)
				# pour pouvoir la supprimer ensuite
				add(group,get(eval(frameassign),envir= .GlobalEnv))
				car_libelle=fun_char_spe(object@data$par_nom)
				car_libelle[nchar(car_libelle)>30]<-paste(substr(car_libelle[nchar(car_libelle)>30],1,30),".",sep="")
				choice=gdroplist(items=car_libelle,container=get(eval(frameassign),envir= .GlobalEnv),handler=hcar)
				gbutton("OK", container=get(eval(frameassign),envir= .GlobalEnv),handler=hcar)
			} else funout(get("msg",envir=envir_stacomi)$Refpar.4,arret=TRUE)
		})


#' Command line interface to select a parameter
#' 
#' the choice_c method is intented to have the same behaviour as choice (which creates a
#' widget in the graphical interface) but from the command line. 
#' If an objectBilan is passed as a parameter, the method will do a charge_avec_filtre to select only the taxa present in the counting devices
#' @param object an object of class  \link{Refpar-class}
#' @param par A character vector of par
#' @param silent Default FALSE but not used there
#' @return An object of class \link{Refpar-class}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("choice_c",signature=signature("Refpar"),definition=function(object,par,silent=FALSE) {
			if (class(par)=="numeric") {
				par<-as.character(par) 
			}
			if (any(is.na(par))) stop ("NA values par")			
			object@par_selectionne<-par				
			if (nrow(object@data)==0){
				stop ("Internal error : tried to set a value for par_selectionne without initializing the data slot")
			}
			#validObject(object,test=FALSE) 
			#here I don't want to generate an error if parm is not present
			#so I'm not using the validObject which will throw and error
			concord<-object@par_selectionne%in%object@data$par_code	
			
			if (any(!concord)){
				warning(paste("No data for par",object@par_selectionne[!concord]))
			}
				
			assign("refpar",object,envir=envir_stacomi)
			return(object)
		})