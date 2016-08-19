# Nom fichier :        RefParquan   (classe)
#' Class "Refparquan"
#' 
#' Class enabling to load the list of quantitative parameters and to select one
#' of them. It inherits from 'Refpar', uses its 'choix' method
#' 
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Refparquan", data=data.frame())}.  \describe{
#' \item{list("data")}{Object of class \code{"data.frame"} ~ Quantitative
#' parameters }\item{:}{Object of class \code{"data.frame"} ~ Quantitative
#' parameters } }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso Other referential classes \code{\linkS4class{RefAnnee}}
#' \code{\linkS4class{RefCheckBox}} \code{\linkS4class{RefChoix}}
#' \code{\linkS4class{RefCoe}} \code{\linkS4class{RefDC}}
#' \code{\linkS4class{RefDF}} \code{\linkS4class{RefListe}}
#' \code{\linkS4class{Refpar}} \code{\linkS4class{Refparqual}}
#' \code{\linkS4class{Refparquan}} \code{\linkS4class{RefPoidsMoyenPeche}}
#' \code{\linkS4class{RefStades}} \code{\linkS4class{RefStationMesure}}
#' \code{\linkS4class{RefTaxon}}
#' @keywords classes
#' @family Referential objects
#' @include Refpar.r
#' @examples
#' 
#' showClass("Refparquan")
#' 
setClass(Class="Refparquan",contains="Refpar")

#' Loading method for Reparquan referential objects
#' @return An S4 object of class Refparquan
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#'  object=new("Refparquan")
#'  charge(object)
setMethod("charge",signature=signature("Refparquan"),definition=function(object) {
			requete=new("RequeteODBC")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@sql= "SELECT * FROM ref.tg_parametre_par
					INNER JOIN ref.tr_parametrequantitatif_qan ON qan_par_code=par_code"
			requete<-stacomirtools::connect(requete)
			#funout("La requete est effectuee pour charger les parametres \n")
			object@data<-requete@query
			return(object)
		})


#' Loading method for Reparquan referential objects searching only those parameters existing for a DC, a Taxon, and a stade
#' @return An S4 object of class Refparqualn
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#'  dc_selectionne=6
#'	taxon_selectionne=2038
#'  stade_selectionne="AGJ"
#'  object=new("Refparquan")
#'  charge_avec_filtre(object,dc_selectionne,taxon_selectionne,stade_selectionne)		
setMethod("charge_avec_filtre",signature=signature("Refparquan"),definition=function(object,dc_selectionne,taxon_selectionne,stade_selectionne) {
			requete=new("RequeteODBCwhere")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@select=paste("SELECT DISTINCT ON (par_code) par_code, par_nom", 
					" FROM ",get("sch",envir=envir_stacomi),"tg_dispositif_dis",
					" JOIN ",get("sch",envir=envir_stacomi),"t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"t_lot_lot on lot_ope_identifiant=ope_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"tj_caracteristiquelot_car on car_lot_identifiant=lot_identifiant",
					" JOIN ref.tg_parametre_par on par_code=car_par_code",
					" JOIN ref.tr_parametrequantitatif_qan ON qan_par_code=par_code",sep="")
			requete@where=paste("where dis_identifiant=",dc_selectionne)
			requete@and=paste("and lot_tax_code='",taxon_selectionne,"' and lot_std_code='",stade_selectionne,"'",sep="")
			requete@order_by="ORDER BY par_code"  
			requete<-stacomirtools::connect(requete) 
			object@data<-requete@query
			if (nrow(object@data)==0) {object@data=data.frame("par_code"=NA,"par_nom"="aucune")
			} else object@data=rbind(object@data,c(NA,"aucune"))
			return(object)
		})