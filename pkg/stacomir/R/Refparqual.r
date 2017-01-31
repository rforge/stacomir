#' Class "Refparqual"
#' 
#' Class enabling to load the list of qualitative parameters and to select one
#' of them. It inherits from 'Refpar', uses its 'choice' method
#' 
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Refparqual", ...)}.  \describe{ \item{list("valqual")}{Object of
#' class \code{"data.frame"} Values of qualitatives parameters}\item{:}{Object
#' of class \code{"data.frame"} Values of qualitatives parameters}
#' \item{list("data")}{Object of class \code{"data.frame"} Qualitatives
#' parameters }\item{:}{Object of class \code{"data.frame"} Qualitatives
#' parameters } }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @slot valqual="data.frame" the list of qualitative parameters
#' @include Refpar.r
#' @family Referential objects
setClass(Class="Refparqual",representation= representation(valqual="data.frame"),contains="Refpar")

#' Loading method for Reparqual referential objects
#' @param object An object of class \link{Refparqual-class}
#' @return An S4 object of class Refparqual
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("Refparqual")
#'  charge(object)
#' }
setMethod("charge",signature=signature("Refparqual"),definition=function(object) {
			requete=new("RequeteODBC")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@sql= "select * from ref.tg_parametre_par
					INNER JOIN ref.tr_parametrequalitatif_qal ON tr_parametrequalitatif_qal.qal_par_code::text = tg_parametre_par.par_code::text"
			requete<-stacomirtools::connect(requete)
			#funout(gettext("The query to load parameters is done \n",domain="R-stacomiR"))
			object@data<-requete@query
			return(object)
		})

#' Loading method for Reparqual referential objects searching only those parameters existing for a DC, a Taxon, and a stade
#' @param object An object of class \link{Refparqual-class}
#' @param dc_selectionne The dc set in the Bilan object
#' @param taxon_selectionne The taxa set in the Bilan object
#' @param stade_selectionne The stage set in the Bilan object
#' @return An S4 object of class \link{Refparqual-class}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  dc_selectionne=6
#'	taxon_selectionne=2038
#'  stade_selectionne="AGJ"
#'  object=new("Refparqual")
#'  charge_avec_filtre(object,dc_selectionne,taxon_selectionne,stade_selectionne)
#' }
setMethod("charge_avec_filtre",signature=signature("Refparqual"),definition=function(object,dc_selectionne,taxon_selectionne,stade_selectionne) {
			requete=new("RequeteODBCwhere")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@select=paste("SELECT DISTINCT ON (par_code) par_code, par_nom", 
					" FROM ",get("sch",envir=envir_stacomi),"tg_dispositif_dis",
					" JOIN ",get("sch",envir=envir_stacomi),"t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"t_lot_lot on lot_ope_identifiant=ope_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"tj_caracteristiquelot_car on car_lot_identifiant=lot_identifiant",
					" JOIN ref.tg_parametre_par on par_code=car_par_code",
					" JOIN ref.tr_parametrequalitatif_qal ON tr_parametrequalitatif_qal.qal_par_code::text = tg_parametre_par.par_code::text",sep="")
			requete@where=paste("where dis_identifiant=",dc_selectionne)
			requete@and=paste("and lot_tax_code='",taxon_selectionne,"' and lot_std_code='",stade_selectionne,"'",sep="")
			requete@order_by="ORDER BY par_code"  
			requete<-stacomirtools::connect(requete)
			object@data<-requete@query
			if (nrow(object@data)==0) {object@data=data.frame("par_code"=NA,"par_nom"="aucune")
			} else object@data=rbind(object@data,c(NA,"aucune"))
			return(object)
		})

#'  Loads an additional dataset
#' this method is loaded to obtain the possible values of a qualitative parameter. Here data only contains one line
#' @param object An object of class \link{Refparqual-class}
#' @return An S4 object of class Refparqual with the valqual slot filled
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  dc_selectionne=6
#'	taxon_selectionne=2038
#'  stade_selectionne="AGJ"
#'  object=new("Refparqual")
#'  object<-charge(object)
#'  charge_complement(object)
#' }		
setMethod("charge_complement",signature=signature("Refparqual"),definition=function(object) {
			if (nrow(object@data)!=1) funout(gettextf("Internal error : there must have one line in Refparqual@data, or nbligne= %s",nrow(object@data)),arret=TRUE)
			requete=new("RequeteODBC")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@sql= paste("select * from ref.tr_valeurparametrequalitatif_val",
					" WHERE val_qal_code='",object@data$par_code,
					"' ORDER BY val_rang",sep="")
			requete<-stacomirtools::connect(requete)
			#funout(gettext("The query to load parameters is done \n",domain="R-stacomiR"))
			object@valqual<-requete@query
			return(object)
		})


#' Choice method for Refparqual referential objects, internal use
#' @note the choice method assigns an object of class Refparqual named refparqual in the environment envir_stacomi
#' this method rewrites the method from Refpar, as it integrates a request of the possible values of qualitative parameters, hence the parameters,however it was redefined in refparqual
#' to load the possible values of qualitative parameters
#' @param object An object of class \link{Refparqual-class}
#' @param label The label to set in the frame
#' @param nomassign The name used when assigning the object Refparqual to the \code{envir_stacomi} environment
#' @param frameassign The name of the gframe assigned to the user environment
#' @param is.enabled Is the frame enabled at launch.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples  
#' \dontrun{
#'  object=new("Refparqual")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object<-charge(object)
#' choice(object)
#' }
setMethod("choice",signature=signature("Refparqual"),definition=function(object,
				label=gettext("Choice of a sample characteristic",domain="R-stacomiR"),
				nomassign="refpar",
				frameassign="frame_par",
				is.enabled=TRUE) {
			if (nrow(object@data) > 0){
				hcar=function(h,...){
					carchoisi=svalue(choice)
					object@data<-object@data[car_libelle%in%carchoisi ,]
					object<-charge_complement(object)
					assign(nomassign,object,envir_stacomi)
					funout(gettext("Feature has been selected\n",domain="R-stacomiR"))
				}
				assign(frameassign,gframe(label),envir= .GlobalEnv)
				add(group,get(eval(frameassign),envir= .GlobalEnv))
				car_libelle=fun_char_spe(object@data$par_nom)
				choice=gdroplist(items=car_libelle,container=get(eval(frameassign),envir= .GlobalEnv),handler=hcar)
				gbutton("OK", container=get(eval(frameassign),envir= .GlobalEnv),handler=hcar)
			} else stop(gettext("Internal error : unable to load any feature to make the choice\n",domain="R-stacomiR"),arret=TRUE)
		})
