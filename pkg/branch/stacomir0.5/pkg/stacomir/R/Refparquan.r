# Nom fichier :        RefParquan   (classe)
#' @title Refparquan referential class choose a quantitative parameter
#' @note The choice method of this class is inherited from the parent class
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @example objet=new("Refparquan")
setClass(Class="Refparquan",contains="Refpar")

#' Loading method for Reparquan referential objects
#' @returnType S4 object
#' @return An S4 object of class Refparquan
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @example 
#'  objet=new("Refparquan")
#'  charge(objet)
setMethod("charge",signature=signature("Refparquan"),definition=function(objet) {
			requete=new("RequeteODBC")
			requete@baseODBC=baseODBC
			requete@sql= "SELECT * FROM ref.tg_parametre_par
					INNER JOIN ref.tr_parametrequantitatif_qan ON qan_par_code=par_code"
			requete<-connect(requete)
			#funout("La requete est effectuee pour charger les parametres \n")
			objet@data<-requete@query
			return(objet)
		})


#' Loading method for Reparquan referential objects searching only those parameters existing for a DC, a Taxon, and a stade
#' @returnType S4 object
#' @return An S4 object of class Refparqualn
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @example 
#'  dc_selectionne=6
#'	taxon_selectionne=2038
#'  stade_selectionne="AGJ"
#'  objet=new("Refparquan")
#'  charge_avec_filtre(objet,dc_selectionne,taxon_selectionne,stade_selectionne)		
setMethod("charge_avec_filtre",signature=signature("Refparquan"),definition=function(objet,dc_selectionne,taxon_selectionne,stade_selectionne) {
			requete=new("RequeteODBCwhere")
			requete@baseODBC=baseODBC
			requete@select=paste("SELECT DISTINCT ON (par_code) par_code, par_nom", 
					" FROM ",sch,"tg_dispositif_dis",
					" JOIN ",sch,"t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
					" JOIN ",sch,"t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
					" JOIN ",sch,"t_lot_lot on lot_ope_identifiant=ope_identifiant",
					" JOIN ",sch,"tj_caracteristiquelot_car on car_lot_identifiant=lot_identifiant",
					" JOIN ref.tg_parametre_par on par_code=car_par_code",
					" JOIN ref.tr_parametrequantitatif_qan ON qan_par_code=par_code",sep="")
			requete@where=paste(" where dis_identifiant in (",paste(dc_selectionne,collapse=","),")",sep="")
			requete@and=paste(paste(" and lot_tax_code in ('",paste(taxon_selectionne,collapse="','"),"')",sep=""),
					paste(" and lot_std_code in ('",paste(stade_selectionne,collapse="','"),"')",sep=""),sep="")
			requete@order_by="ORDER BY par_code"  
			requete=connect(requete) 
			objet@data<-requete@query
			if (nrow(objet@data)==0) {objet@data=data.frame("par_code"=NA,"par_nom"="aucune")
			} else objet@data=rbind(objet@data,c(NA,"aucune"))
			return(objet)
		})