# Nom fichier :        RefPar  (classe)
# Contact :            cedric.briand"at"eptb-vilaine.fr
# Date de creation :   22/03/2009 21:14:14

#TODO  selection de plusieurs caracteristiques
#' Class "Refpar"
#' 
#' Class enabling to load the list of parameters and select one of them
#' 
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Refpar", data)}.  \describe{ \item{list("data")}{Object of class
#' \code{"data.frame"} ~ All the parameters stored in the
#' database}\item{:}{Object of class \code{"data.frame"} ~ All the parameters
#' stored in the database} }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @keywords classes
#' @slot data="data.frame" the list of parameters
#' @family Referential objects
setClass(Class="Refpar",representation= representation(data="data.frame"))

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
#' charge_avec_filtre(object,dc_selectionne=6,taxon_selectionne=2038,stade_selectionne="CIV")
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
			requete@where=paste("where dis_identifiant=",dc_selectionne)
			requete@and=paste("and lot_tax_code='",taxon_selectionne,"' and lot_std_code='",stade_selectionne,"'",sep="")
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
					object@data<-object@data[car_libelle%in%carchoisi ,]
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
