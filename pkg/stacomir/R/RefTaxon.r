#' Class "RefTaxon"
#' 
#' Loading and selection of fish species. This class is a referential class, and it is 
#' integrated into refBilan objects.
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefTaxon", ...)}.   
#' @slot data A \code{"data.frame"} of species available in the database
#' @author cedric.briand"at"eptb-vilaine.fr
#' @family Referential objects
setClass(Class="RefTaxon",representation= representation(data="data.frame" ))


#' Loading method for RefTaxon referential objects
#' 
#' @return An S4 object of class RefTaxon
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @param object An object of class \link{RefTaxon-class}
#' @examples \dontrun{
#'  object=new("RefTaxon")
#'  charge(object)}
setMethod("charge",signature=signature("RefTaxon"),definition=function(object) {
			req=new("RequeteODBC") 
			req@baseODBC<-get("baseODBC",envir=envir_stacomi)
			req@sql="SELECT tax_code, tax_nom_latin, tax_nom_commun, tax_ntx_code, tax_tax_code FROM ref.tr_taxon_tax  ORDER BY tax_rang ASC ;"
			req<-stacomirtools::connect(req)  # appel de la methode connect de l'object requeteODBC
			object@data<-req@query
			return(object)
		})

#' Loading method for RefTaxon referential objects searching only taxa existing for a DC
#' @param object An object of class \link{RefTaxon-class}
#' @param dc_selectionne A counting device selected, only taxa attached to this dc are selected
#' @return An S4 object of class RefTaxon
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
#' @examples \dontrun{
#'  dc_selectionne=6
#'  object=new("RefTaxon")
#'  charge_avec_filtre(object,dc_selectionne=dc_selectionne)}
setMethod("charge_avec_filtre",signature=signature("RefTaxon"),definition=function(object,dc_selectionne) {
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
			requete<-stacomirtools::connect(requete)  
			object@data<-requete@query
			return(object)
		})


#' Choice method for Reftaxon referential objects with only one taxa selected
#' @param object An object of class \link{RefTaxon-class}
#' @param objectBilan An object Bilan which includes the \link{RefTaxon-class}, default NULL
#' @param is.enabled Sets if the frame is enabled at launch, defaut TRUE
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples  \dontrun{
#'  object=new("RefTaxon")
#' win=gwindow()
#' group<-ggroup(container=win,horizontal=FALSE)
#' object<-charge(object)
#' bilanMigration<-new(BilanMigration)
#' choice(object,objectBilan=bilanMigration)
#' }
setMethod("choice",signature=signature("RefTaxon"),
		definition=function(
				object,
				objectBilan=NULL,
				is.enabled=TRUE) {
			if (nrow(object@data) > 0){
				htax=function(h,...){
					taxons=svalue(choice)
					object@data<-object@data[tax_libelle%in%taxons ,]
					assign("refTaxon",object,envir_stacomi)
					funout(gettext("Taxon selected\n",domain="R-stacomiR"))
					if (!is.null(objectBilan)) {
						objectBilan@stades<<-charge_avec_filtre(object=objectBilan@stades,dc_selectionne=get("refDC",envir_stacomi)@dc_selectionne,taxon_selectionne=get("refTaxon",envir_stacomi)@data$tax_code)
						if (exists("frame_std")) delete(group,frame_std)
						if (exists("frame_par")) delete(group,frame_par)
						if (exists("frame_parquan")) delete(group,frame_parquan)
						if (exists("frame_parqual")) delete(group,frame_parqual)
						choice(objectBilan@stades,objectBilan,is.enabled=TRUE)						
					}
				}
				frame_tax<<-gframe(gettext("Taxon selection",domain="R-stacomiR"))
				add(group,frame_tax)
				tax_libelle=fun_char_spe(object@data$tax_nom_latin)
				choice=gdroplist(tax_libelle,container=frame_tax,handler=htax)
				enabled(frame_tax)<-is.enabled
				gbutton("OK", container=frame_tax,handler=htax)
			} else funout(gettext("Stop there is no line in the taxons table (problem with the ODBC link ?)\n",domain="R-stacomiR"),arret=TRUE)
		})

#' Multiple Choice method for Reftaxon referential objects, the graphical interface is built to allow
#' for multiple choices. See load for method in the command line.
#' @param object An object of class \link{RefTaxon-class}
#' @param objectBilan An object Bilan which includes the \link{RefTaxon-class}, default NULL
#' @param is.enabled Sets if the frame is enabled at launch, defaut TRUE
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{ 
#'  object=new("RefTaxon")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object<-charge(object)
#' bilanMigration=new(BilanMigration)
#' choicemult(object,objectBilan=bilanMigration)
#' }
setMethod("choicemult",signature=signature("RefTaxon"),definition=function(object,objectBilan=NULL,is.enabled=TRUE) {
			if (nrow(object@data) > 0){
				htax=function(h,...){
					taxons=tbdesttaxon[,][tbdesttaxon[,]!=""]
					object@data<-object@data[tax_libelle%in%taxons ,]
					assign("refTaxon",object,envir_stacomi)
					funout(gettext("The taxa(s) have been selected\n",domain="R-stacomiR"))
					if (!is.null(objectBilan)) {
						objectBilan@taxons<-object
						objectBilan@stades<-charge_avec_filtre(object=objectBilan@stades,
								dc_selectionne=get("refDC",envir_stacomi)@dc_selectionne,
								taxon_selectionne=get("refTaxon",envir_stacomi)@data$tax_code
								)
						assign(get("objectBilan",envir=envir_stacomi),objectBilan,envir=envir_stacomi)
						# suppresses all tab larger than 3 (taxon)
						if (length(notebook)>3){
							for (i in 4:length(notebook)){
								svalue(notebook) <- i							
								dispose(notebook) ## dispose current tab
							}
						}
						choicemult(objectBilan@stades,objectBilan,is.enabled=TRUE)						
					}
					# changing tab of notebook to next tab
					if (svalue(notebook)<length(notebook)){
						svalue(notebook)<-svalue(notebook)+1	
					}
				}				
				# below the widget structure [=> within (=> type
				# group(ggroup)[notebook(notebook)[grouptaxon(ggroup&tab)[[frametaxonsource(gframe)[tbsourcetaxon(gtable)],frametaxondest(gframe)[tbdtaxondest(gtable)]],OKbutton]]
				if (!exists("notebook")) notebook <- gnotebook(container=group) 				
				tax_libelle=fun_char_spe(object@data$tax_nom_latin)
				grouptaxon<-ggroup() 
				assign("grouptaxon",grouptaxon,.GlobalEnv)
				add(notebook,grouptaxon,label="taxon")
				frametaxonsource<-gframe(gettext("Taxon selection",domain="R-stacomiR"),container=grouptaxon)
				tbsourcetaxon  = gtable(tax_libelle,container=frametaxonsource,expand = TRUE, fill = TRUE)
				size(tbsourcetaxon)<-c(160,300) # les dimensions sont testees a la main 
				# pour s'ajuster aux dimensions du notebook (largeur 400)
				#TODO addmsg
				frametaxondest<-gframe(gettext("drop here",domain="R-stacomiR"),container=grouptaxon)
				# need for a fixed size data.frame otherwise errors when adding new lines
				xx<-data.frame(choice=rep("",8))
				xx$choice<-as.character(xx$choice)
				tbdesttaxon=gtable(xx,container=frametaxondest,expand = TRUE, fill = TRUE)
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
				gbutton("OK", container = grouptaxon, handler = htax)
			} else {
				funout(gettext("Stop there is no line in the taxons table (problem with the ODBC link ?)\n",domain="R-stacomiR"),arret=TRUE)
			}
		})


#' choice_c method for RefTaxon
#' 
#' the choice_cc method is intented to have the same behaviour as choice (which creates a
#' widget in the graphical interface) but from the command line. The values passed to the choice_c method
#' for taxon can be either numeric (2038 = Anguilla anguilla) or character.  
#' @param object An object from the class RefTaxon
#' @param taxons The vector of taxon, can be either code (numeric) or latin name
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples
#' \dontrun{
#' object=new("RefTaxon")
#' object<-charge(object)
#' objectBilan=new("BilanMigrationMult")
#' choice_c(object=object,objectBilan=objectBilan,"Anguilla anguilla")
#' }
setMethod("choice_c",signature=signature("RefTaxon"),definition=function(object,taxons) {
			if (is.null(taxons)) {
				funout(gettext("No value for argument taxon\n",domain="R-stacomiR"),arret=TRUE)
			} else	if (class(taxons)=="character"){	
				libellemanquants<-taxons[!taxons%in%object@data$tax_nom_latin]
				if (length(libellemanquants)>0) warning(gettextf("Taxa not present :\n %s",stringr::str_c(libellemanquants,collapse=", "),domain="R-stacomiR"))
				object@data<-object@data[object@data$tax_nom_latin%in%taxons,]
			} else if (class(taxons)=="numeric"){
				codemanquants<-taxons[!taxons%in%object@data$tax_code]
				if (length(codemanquants)>0) warning(gettextf("Taxa not present :\n %s",stringr::str_c(codemanquants,collapse=", "),domain="R-stacomiR"))
				object@data<-object@data[object@data$tax_code%in%taxons,]
			}
			if (nrow(object@data)==0 )	{
				funout(gettext("Stop there is no line in the taxons table (problem with the ODBC link ?)\n",domain="R-stacomiR"),arret=TRUE)
			}
			assign("refTaxon",object,envir=envir_stacomi)
			return(object)
		})
