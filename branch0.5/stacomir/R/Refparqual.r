# Nom fichier :        RefParqual   (classe)
# Projet :             controle migrateur
# Date de creation :   22/03/2009 21:14:14

#' Class "Refparqual"
#' 
#' Class enabling to load the list of qualitative parameters and to select one
#' of them. It inherits from 'Refpar', uses its 'choix' method
#' 
#' 
#' @name Refparqual-class
#' @aliases Refparqual Refparqual-class

#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Refparqual", ...)}.  \describe{ \item{list("valqual")}{Object of
#' class \code{"data.frame"} Values of qualitatives parameters}\item{:}{Object
#' of class \code{"data.frame"} Values of qualitatives parameters}
#' \item{list("data")}{Object of class \code{"data.frame"} Qualitatives
#' parameters }\item{:}{Object of class \code{"data.frame"} Qualitatives
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
#' @slot valqual="data.frame" the list of qualitative parameters
#' @include Refpar.r
#' @family Referential objects
#' @examples
#' 
#' showClass("Refparqual")
setClass(Class="Refparqual",representation= representation(valqual="data.frame"),contains="Refpar")

#' Loading method for Reparqual referential objects
#' @returnType S4 object
#' @return An S4 object of class Refparqual
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @examples 
#'  objet=new("Refparqual")
#'  charge(objet)
setMethod("charge",signature=signature("Refparqual"),definition=function(objet) {
			requete=new("RequeteODBC")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@sql= "select * from ref.tg_parametre_par
					INNER JOIN ref.tr_parametrequalitatif_qal ON tr_parametrequalitatif_qal.qal_par_code::text = tg_parametre_par.par_code::text"
			requete<-connect(requete)
			#funout("La requete est effectuee pour charger les parametres \n")
			objet@data<-requete@query
			return(objet)
		})

#' Loading method for Reparqual referential objects searching only those parameters existing for a DC, a Taxon, and a stade
#' @returnType S4 object
#' @return An S4 object of class Refparqual
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @examples 
#'  dc_selectionne=6
#'	taxon_selectionne=2038
#'  stade_selectionne="AGJ"
#'  objet=new("Refparqual")
#'  charge_avec_filtre(objet,dc_selectionne,taxon_selectionne,stade_selectionne)
setMethod("charge_avec_filtre",signature=signature("Refparqual"),definition=function(objet,dc_selectionne,taxon_selectionne,stade_selectionne) {
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
			requete=connect(requete)
			objet@data<-requete@query
			if (nrow(objet@data)==0) {objet@data=data.frame("par_code"=NA,"par_nom"="aucune")
			} else objet@data=rbind(objet@data,c(NA,"aucune"))
			return(objet)
		})

#'  method chargecomplement
#' cette methode est appellee apres la selection de l'objet (data ne contient plus qu'une seule ligne)
#' et permet de lancer une requete pour obtenir un complement, ici les valeurs possibles d'un parametre qualitatif
#' @returnType S4 object
#' @return An S4 object of class Refparqual with the valqual slot filled
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @examples 
#'  dc_selectionne=6
#'	taxon_selectionne=2038
#'  stade_selectionne="AGJ"
#'  objet=new("Refparqual")
#'  objet<-charge(objet)
#'  chargecomplement(objet)		
setMethod("chargecomplement",signature=signature("Refparqual"),definition=function(objet) {
			if (nrow(objet@data)!=1) funout(paste(get("msg",envir=envir_stacomi)$Refparqual.1,nrow(objet@data)),arret=TRUE)
			requete=new("RequeteODBC")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@sql= paste("select * from ref.tr_valeurparametrequalitatif_val",
					" WHERE val_qal_code='",objet@data$par_code,
					"' ORDER BY val_rang",sep="")
			requete<-connect(requete)
			#funout("La requete est effectuee pour charger les parametres \n")
			objet@valqual<-requete@query
			return(objet)
		})


# la methode choix differe de celle de Refpar car elle integre la requete des valeurs possibles des parametres qualitatifs		
#' Choix=Choice method for Refparqual referential objects
#' @note the choice method assigns an object of class Refparqual named refparqual in the environment envir_stacomi
#' this method rewrites the method from Refpar, as it integrates a request of the possible values of qualitative parameters, hence the parameters,however it was redefined in refparqual
#' to load the possible values of qualitative parameters
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @examples  
#'  objet=new("Refparqual")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' objet<-charge(objet)
#' choix(objet)
setMethod("choix",signature=signature("Refparqual"),definition=function(objet,label="Choix d'une caracteristique qualitative de lot",nomassign="refpar",frameassign="frame_par",is.enabled=TRUE) {
			if (nrow(objet@data) > 0){
				hcar=function(h,...){
					carchoisi=svalue(choix)
					objet@data<-objet@data[car_libelle%in%carchoisi ,]
					objet<-chargecomplement(objet)
					assign(nomassign,objet,envir_stacomi)
					funout(get("msg",envir=envir_stacomi)$Refpar.3)
				}
				assign(frameassign,gframe(label),envir= .GlobalEnv)
				add(group,get(eval(frameassign),envir= .GlobalEnv))
				car_libelle=fun_char_spe(objet@data$par_nom)
				choix=gdroplist(items=car_libelle,container=get(eval(frameassign),envir= .GlobalEnv),handler=hcar)
				gbutton("OK", container=get(eval(frameassign),envir= .GlobalEnv),handler=hcar)
			} else stop(get("msg",envir=envir_stacomi)$Refpar.4,arret=TRUE)
		})
