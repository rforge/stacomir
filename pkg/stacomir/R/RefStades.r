# Nom fichier :        RefStades   (classe)

#' @title Refstades referential class to load and choose the list of stage
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @slot data="data.frame"
#' @expamples objet=new("RefStades")
setClass(Class="RefStades",representation=representation(data="data.frame") )

#' Loading method for RefStades referential objects
#' @returnType S4 object
#' @return An S4 object of class RefStades
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @expamples 
#'  objet=new("RefStades")
#'  charge(objet)
setMethod("charge",signature=signature("RefStades"),definition=function(objet) {
			req=new("RequeteODBC") 
			req@sql="SELECT std_code, std_libelle FROM ref.tr_stadedeveloppement_std ORDER BY std_code ;"
			req=connect(req)  # appel de la methode connect de l'objet requeteODBC
			objet@data<-req@query
			return(objet)
		})
#' Loading method for RefStades referential objects searching only those stages existing for a DC and a Taxon
#' @returnType S4 object
#' @return An S4 object of class RefStades
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @expamples 
#'  dc_selectionne=6
#'	taxon_selectionne=2038
#'  objet=new("RefStades")
#'  charge_avec_filtre(objet,dc_selectionne,taxon_selectionne)
setMethod("charge_avec_filtre",signature=signature("RefStades"),definition=function(objet,dc_selectionne,taxon_selectionne) {
			requete=new("RequeteODBCwhere")
			requete@baseODBC=baseODBC
			requete@select=paste("SELECT DISTINCT ON (std_rang) std_code, std_libelle", 
					" FROM ",get("sch",envir=envir_stacomi),"tg_dispositif_dis",
					" JOIN ",get("sch",envir=envir_stacomi),"t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"t_lot_lot on lot_ope_identifiant=ope_identifiant",
					" JOIN ref.tr_stadedeveloppement_std on lot_std_code=std_code",sep="")
			requete@where=paste("where dis_identifiant='",dc_selectionne,"'",sep="")
			requete@and=paste("and lot_tax_code='",taxon_selectionne,"'",sep="")
			requete@order_by="ORDER BY std_rang"  
			requete=connect(requete)  # appel de la methode connect de l'objet requeteODBC
			objet@data<-requete@query
			if (nrow(objet@data)==0) funout(get("msg",envir=envir_stacomi)$RefStades.1,arret=TRUE)
			return(objet)
		})
#' Choice method for RefStades referential objects
#' @note the method tests if the load is called from within a "bilan" object, and loads par, parqual, or parquan objects accordingly
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @expamples 
#'  objet=new("RefStades")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' objet<-charge(objet)
#' bilanMigrationPar=new(BilanMigrationPar)
#' # objetBilan=bilan_taille # pour autre test
#' choix(objet,objetBilan=bilanMigrationPar)
setMethod("choix",signature=signature("RefStades"),definition=function(objet,objetBilan=NULL,is.enabled=TRUE) {
			if (nrow(objet@data) > 0){
				hstd=function(h,...){
					stades=svalue(choix)
					objet@data<-objet@data[std_libelle%in%stades ,]
					assign("refStades",objet,envir_stacomi)
					funout(get("msg",envir=envir_stacomi)$RefStades.2)
					if (!is.null(objetBilan)) {
						# par defaut la methode ne charge pas de maniere interactive  (par exemple en ne premnant que les stades des taxon du dc par la methode charge_avec_filtre
						# elle est alors affichee des le debut par la methode choix à laquelle on ne passe pas d'objetBilan en parametre 
						#il y a bien un objet par dans l'objet Bilan             
						if (class(try(objetBilan@par,silent=TRUE))!="try-error") {
							objetBilan@par<<-charge_avec_filtre(objet=objetBilan@par,dc_selectionne=get("refDC",envir_stacomi)@dc_selectionne,taxon_selectionne=get("refTaxons",envir_stacomi)@data$tax_code,stade_selectionne=get("refStades",envir_stacomi)@data$std_code)
							if (exists("frame_par")) delete(group,frame_par)
							choix(objetBilan@par,is.enabled=TRUE)
						} 
						#il y a bien un objet parqual dans l'objet Bilan						
						if (class(try(objetBilan@parqual,silent=TRUE))!="try-error") {
							objetBilan@parqual<<-charge_avec_filtre(objet=objetBilan@parqual,dc_selectionne=get("refDC",envir_stacomi)@dc_selectionne,taxon_selectionne=get("refTaxons",envir_stacomi)@data$tax_code,stade_selectionne=get("refStades",envir_stacomi)@data$std_code)
							
							if (exists("frame_parqual")) delete(group,frame_parqual)
							choix(objetBilan@parqual,label=get("msg",envir=envir_stacomi)$RefStades.3,nomassign="refparqual",frameassign="frame_parqual",is.enabled=TRUE)
						}
#il y a bien un objet parquan dans l'objet Bilan
						if (class(try(objetBilan@parquan,silent=TRUE))!="try-error") {
							objetBilan@parquan<<-charge_avec_filtre(objet=objetBilan@parquan,dc_selectionne=get("refDC",envir_stacomi)@dc_selectionne,taxon_selectionne=get("refTaxons",envir_stacomi)@data$tax_code,stade_selectionne=get("refStades",envir_stacomi)@data$std_code)
							if (class(objetBilan)=="Bilan_taille" )
							{
								if (nrow(objetBilan@parquan@data)>0) {
									objetBilan@parquan@data=objetBilan@parquan@data[objetBilan@parquan@data$par_code=="1786"|  #taille
													objetBilan@parquan@data$par_code=="1785"|     # taille fourche
													is.na(objetBilan@parquan@data$par_code)|objetBilan@parquan@data$par_code=="C001",]     # aucune
								}
							}
							if (exists("frame_parquan")) delete(group,frame_parquan)
							choix(objetBilan@parquan,label=get("msg",envir=envir_stacomi)$RefStades.4,nomassign="refparquan",frameassign="frame_parquan",is.enabled=TRUE)
						}
						
					}
				}
				frame_std<<-gframe(get("msg",envir=envir_stacomi)$RefStades.6)
				add(group,frame_std)
				std_libelle=fun_char_spe(objet@data$std_libelle)
				choix=gcombobox(std_libelle,container=frame_std,handler=hstd)
				enabled(frame_std)<-is.enabled
				gbutton("OK", container=frame_std,handler=hstd)
			} else funout(get("msg",envir=envir_stacomi)$RefStades.5,arret=TRUE)
		})