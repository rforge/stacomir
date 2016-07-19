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
			requete@where=paste("where dis_identifiant in",vector_to_listsql(dc_selectionne))
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
#' Multiple Choice method for Reftaxon referential objects
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @example  
#'  objet=new("RefTaxon")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' objet<-charge(objet)
#' bilanMigration=new(BilanMigration)
#' choixmult(objet,objetBilan=bilanMigration)
setMethod("choixmult",signature=signature("RefTaxon"),definition=function(objet,objetBilan=NULL,is.enabled=TRUE) {
			if (nrow(objet@data) > 0){
				htax=function(h,...){
					taxons=tbdesttaxon[,][tbdesttaxon[,]!=""]
					objet@data<-objet@data[tax_libelle%in%taxons ,]
					assign("refTaxons",objet,envir_stacomi)
					funout(get("msg",envir=envir_stacomi)$RefTaxon.4)
					if (!is.null(objetBilan)) {
						objetBilan@stades<<-charge_avec_filtre(objet=objetBilan@stades,
								dc_selectionne=get("refDC",envir_stacomi)@dc_selectionne,
								taxon_selectionne=get("refTaxons",envir_stacomi)@data$tax_code)
						# suppresses all tab larger than 3 (taxon)
						if (length(notebook)>3){
							for (i in 4:length(notebook)){
								svalue(notebook) <- i							
								dispose(notebook) ## dispose current tab
							}
						}
						choixmult(objetBilan@stades,objetBilan,is.enabled=TRUE)						
					}
					# changing tab of notebook to next tab
					if (svalue(notebook)<length(notebook)){
						svalue(notebook)<-svalue(notebook)+1	
					}
				}				
				# below the widget structure [=> within (=> type
				# group(ggroup)[notebook(notebook)[grouptaxon(ggroup&tab)[[frametaxonsource(gframe)[tbsourcetaxon(gtable)],frametaxondest(gframe)[tbdtaxondest(gtable)]],OKbutton]]
				if (!exists("notebook")) notebook <- gnotebook(container=group) 				
				tax_libelle=fun_char_spe(objet@data$tax_nom_latin)
				grouptaxon<<-ggroup() 
				add(notebook,grouptaxon,label="taxon")
				frametaxonsource<-gframe(get("msg",envir=envir_stacomi)$RefTaxon.2,cont=grouptaxon)
				tbsourcetaxon  = gtable(tax_libelle,cont=frametaxonsource,expand = TRUE, fill = TRUE)
				size(tbsourcetaxon)<-c(160,300) # les dimensions sont testees a la main 
				# pour s'ajuster aux dimensions du notebook (largeur 400)
				#TODO addmsg
				frametaxondest<-gframe("deposez ici",cont=grouptaxon)
				# need for a fixed size data.frame otherwise errors when adding new lines
				xx<-data.frame(choix=rep("",8))
				xx$choix<-as.character(xx$choix)
				tbdesttaxon=gtable(xx,cont=frametaxondest,expand = TRUE, fill = TRUE)
				size(tbdesttaxon)<-c(160,300)
				adddropsource(tbsourcetaxon)
				adddroptarget(tbdesttaxon)				
				adddropmotion(tbdesttaxon,handler=function(h,...) {
							valeurs<-tbdesttaxon[,]
							valeurs<-valeurs[valeurs!=""]
							if (!svalue(tbsourcetaxon)%in%valeurs){
								tbdesttaxon[length(valeurs)+1,1]<-svalue(tbsourcetaxon)
							}
						})
				addHandlerDoubleclick(tbsourcetaxon,handler=function(h,...) {
							valeurs<-tbdesttaxon[,]
							valeurs<-valeurs[valeurs!=""]
							if (!svalue(tbsourcetaxon)%in%valeurs){
								tbdesttaxon[length(valeurs)+1,1]<-svalue(h$obj)
							}
						})
				adddropsource(tbdesttaxon)
				adddroptarget(tbsourcetaxon)
				removetaxon<-function(){
					valeurs<-tbdesttaxon[,]
					valeurs<-valeurs[valeurs!=""]
					valeurs<-valeurs[-match(svalue(tbdesttaxon),valeurs)]
					tbdesttaxon[,]<-c(valeurs,rep("",8-length(valeurs)))
				}
				adddropmotion(tbsourcetaxon,handler=function(h,...) {
							removetaxon()
						})
				addHandlerDoubleclick(tbdesttaxon,handler=function(h,...) {
							removetaxon()
						})
				gbutton("ok", cont = grouptaxon, handler = htax)
			} else {
				funout(get("msg",envir=envir_stacomi)$RefDC.7,arret=TRUE)
			}
		})