# Nom fichier :        RefTaxon   (classe)
# Projet :             controle migrateur
# Organisme :          IAV
# Auteur :             Cedric Briand
# Contact :            cedric.briand"at"eptb-vilaine.fr
# Date de creation :   31/03/2008 17:21:30

#' @title Refstades referential class to load and choose the list of taxa
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @slot data="data.frame"
#' @expamples \dontrun {objet=new("RefTaxon")}
setClass(Class="RefTaxon",representation= representation(data="data.frame" ))
#' Loading method for RefTaxon referential objects
#' @returnType S4 object
#' @return An S4 object of class RefTaxon
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @expamples \dontrun {
#'  objet=new("RefTaxon")
#'  charge(objet)}
setMethod("charge",signature=signature("RefTaxon"),definition=function(objet) {
			req=new("RequeteODBC") 
			req@baseODBC<-get("baseODBC",envir=envir_stacomi)
			req@sql="SELECT tax_code, tax_nom_latin, tax_nom_commun, tax_ntx_code, tax_tax_code FROM ref.tr_taxon_tax  ORDER BY tax_rang ASC ;"
			req=connect(req)  # appel de la methode connect de l'objet requeteODBC
			objet@data<-req@query
			return(objet)
		})

#' Loading method for RefTaxon referential objects searching only those stages existing for a DC and a Taxon
#' @returnType S4 object
#' @return An S4 object of class RefTaxon
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @exportMethod charge_avec_filtre
#' @expamples \dontrun {
#'  dc_selectionne=6
#'  objet=new("RefTaxon")
#'  charge_avec_filtre(objet,dc_selectionne=dc_selectionne)}
setMethod("charge_avec_filtre",signature=signature("RefTaxon"),definition=function(objet,dc_selectionne) {
			requete=new("RequeteODBCwhere")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@select=paste("SELECT DISTINCT ON (tax_rang) tax_code, tax_nom_latin, tax_nom_commun, tax_ntx_code, tax_tax_code", 
					" FROM ",get("sch",envir=envir_stacomi),"tg_dispositif_dis",
					" JOIN ",get("sch",envir=envir_stacomi),"t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"t_lot_lot on lot_ope_identifiant=ope_identifiant",
					" JOIN ref.tr_taxon_tax on lot_tax_code=tax_code",sep="")
			requete@where=paste("where dis_identifiant=",dc_selectionne)
			requete@order_by="ORDER BY tax_rang ASC"  
			requete=connect(requete)  
			objet@data<-requete@query
			return(objet)
		})
#' Choice method for Reftaxon referential objects
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @expamples  \dontrun {
#'  objet=new("RefTaxon")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' objet<-charge(objet)
#' bilanMigration=new(BilanMigration)
#' choix(objet,objetBilan=bilanMigration)}
setMethod("choix",signature=signature("RefTaxon"),definition=function(objet,objetBilan=NULL,is.enabled=TRUE) {
			if (nrow(objet@data) > 0){
				htax=function(h,...){
					taxons=svalue(choix)
					objet@data<-objet@data[tax_libelle%in%taxons ,]
					assign("refTaxons",objet,envir_stacomi)
					funout(get("msg",envir=envir_stacomi)$RefTaxon.1)
					if (!is.null(objetBilan)) {
						objetBilan@stades<<-charge_avec_filtre(objet=objetBilan@stades,dc_selectionne=get("refDC",envir_stacomi)@dc_selectionne,taxon_selectionne=get("refTaxons",envir_stacomi)@data$tax_code)
						if (exists("frame_std")) delete(group,frame_std)
						if (exists("frame_par")) delete(group,frame_par)
						if (exists("frame_parquan")) delete(group,frame_parquan)
						if (exists("frame_parqual")) delete(group,frame_parqual)
						choix(objetBilan@stades,objetBilan,is.enabled=TRUE)						
					}
				}
				frame_tax<<-gframe(get("msg",envir=envir_stacomi)$RefTaxon.2)
				add(group,frame_tax)
				tax_libelle=fun_char_spe(objet@data$tax_nom_latin)
				choix=gdroplist(tax_libelle,container=frame_tax,handler=htax)
				enabled(frame_tax)<-is.enabled
				gbutton("OK", container=frame_tax,handler=htax)
			} else funout(get("msg",envir=envir_stacomi)$RefTaxon.3,arret=TRUE)
		})
# pour test #choix(objet)