# Nom fichier :        RefStades   (classe)

#' Class "RefStades"
#' 
#' Representation of a fish phase
#' 
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefStades", data="data.frame")}.  \describe{
#' \item{list("data")}{Object of class \code{"data.frame"} ~ The phases
#' available in the database}\item{:}{Object of class \code{"data.frame"} ~ The
#' phases available in the database} }
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
setClass(Class="RefStades",representation=representation(data="data.frame") )

#' Loading method for RefStades referential objects
#' @return An S4 object of class RefStades
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("RefStades")
#'  charge(object)
#' }
setMethod("charge",signature=signature("RefStades"),definition=function(object) {
			req=new("RequeteODBC") 
			req@baseODBC<-get("baseODBC",envir=envir_stacomi)
			req@sql="SELECT std_code, std_libelle FROM ref.tr_stadedeveloppement_std ORDER BY std_code ;"
			req<-stacomirtools::connect(req)  # appel de la methode connect de l'object requeteODBC
			object@data<-req@query
			return(object)
		})
#' Loading method for RefStades referential objects searching only those stages existing for a DC and a Taxon
#' @return An S4 object of class RefStades
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  dc_selectionne=6
#'	taxon_selectionne=2038
#'  object=new("RefStades")
#'  charge_avec_filtre(object,dc_selectionne,taxon_selectionne)
#' }
setMethod("charge_avec_filtre",signature=signature("RefStades"),definition=function(object,dc_selectionne,taxon_selectionne) {
			requete=new("RequeteODBCwhere")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@select=paste("SELECT DISTINCT ON (std_code) std_code, std_libelle", 
					" FROM ",get("sch",envir=envir_stacomi),"tg_dispositif_dis",
					" JOIN ",get("sch",envir=envir_stacomi),"t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"t_lot_lot on lot_ope_identifiant=ope_identifiant",
					" JOIN ref.tr_stadedeveloppement_std on lot_std_code=std_code",sep="")
			requete@where=paste("where dis_identifiant in ",vector_to_listsql(dc_selectionne),sep="")
			requete@and=paste("and lot_tax_code in ",vector_to_listsql(taxon_selectionne),sep="")
			requete@order_by="ORDER BY std_code"  
			requete<-stacomirtools::connect(requete)  # appel de la methode connect de l'object requeteODBC
			object@data<-requete@query
			if (nrow(object@data)==0) funout(get("msg",envir=envir_stacomi)$RefStades.1,arret=TRUE)
			return(object)
		})
#' Choice method for RefStades referential objects
#' @note the method tests if the load is called from within a "bilan" object, and loads par, parqual, or parquan objects accordingly
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("RefStades")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object<-charge(object)
#' bilanMigrationPar=new(BilanMigrationPar)
#' objectBilan=bilan_taille # for another test
#' choice(object,objectBilan=bilanMigrationPar)
#' }
setMethod("choice",signature=signature("RefStades"),definition=function(object,objectBilan=NULL,is.enabled=TRUE) {
			if (nrow(object@data) > 0){
				hstd=function(h,...){
					stades=svalue(choice)
					object@data<-object@data[std_libelle%in%stades ,]
					assign("refStades",object,envir_stacomi)
					funout(get("msg",envir=envir_stacomi)$RefStades.2)
					if (!is.null(objectBilan)) {
						# par defaut la methode ne charge pas de maniere interactive  (par exemple en ne premnant que les stades des taxon du dc par la methode charge_avec_filtre
						# elle est alors affichee des le debut par la methode choice e laquelle on ne passe pas d'objectBilan en parametre 
						#il y a bien un object par dans l'object Bilan             
						if (class(try(objectBilan@par,silent=TRUE))!="try-error") {
							objectBilan@par<<-charge_avec_filtre(object=objectBilan@par,dc_selectionne=get("refDC",envir_stacomi)@dc_selectionne,taxon_selectionne=get("refTaxons",envir_stacomi)@data$tax_code,stade_selectionne=get("refStades",envir_stacomi)@data$std_code)
							if (exists("frame_par")) delete(group,frame_par)
							choice(objectBilan@par,is.enabled=TRUE)
						} 
						#il y a bien un object parqual dans l'object Bilan						
						if (class(try(objectBilan@parqual,silent=TRUE))!="try-error") {
							objectBilan@parqual<<-charge_avec_filtre(object=objectBilan@parqual,dc_selectionne=get("refDC",envir_stacomi)@dc_selectionne,taxon_selectionne=get("refTaxons",envir_stacomi)@data$tax_code,stade_selectionne=get("refStades",envir_stacomi)@data$std_code)
							
							if (exists("frame_parqual")) delete(group,frame_parqual)
							choice(objectBilan@parqual,label=get("msg",envir=envir_stacomi)$RefStades.3,nomassign="refparqual",frameassign="frame_parqual",is.enabled=TRUE)
						}
#il y a bien un object parquan dans l'object Bilan
						if (class(try(objectBilan@parquan,silent=TRUE))!="try-error") {
							objectBilan@parquan<<-charge_avec_filtre(object=objectBilan@parquan,dc_selectionne=get("refDC",envir_stacomi)@dc_selectionne,taxon_selectionne=get("refTaxons",envir_stacomi)@data$tax_code,stade_selectionne=get("refStades",envir_stacomi)@data$std_code)
							if (class(objectBilan)=="Bilan_taille" )
							{
								if (nrow(objectBilan@parquan@data)>0) {
									objectBilan@parquan@data=objectBilan@parquan@data[objectBilan@parquan@data$par_code=="1786"|  #taille
													objectBilan@parquan@data$par_code=="1785"|     # taille fourche
													is.na(objectBilan@parquan@data$par_code)|objectBilan@parquan@data$par_code=="C001",]     # aucune
								}
							}
							if (exists("frame_parquan")) delete(group,frame_parquan)
							choice(objectBilan@parquan,label=get("msg",envir=envir_stacomi)$RefStades.4,nomassign="refparquan",frameassign="frame_parquan",is.enabled=TRUE)
						}
						
					}
				}
				frame_std<<-gframe(get("msg",envir=envir_stacomi)$RefStades.6)
				add(group,frame_std)
				std_libelle=fun_char_spe(object@data$std_libelle)
				choice=gcombobox(std_libelle,container=frame_std,handler=hstd)
				enabled(frame_std)<-is.enabled
				gbutton("OK", container=frame_std,handler=hstd)
			} else funout(get("msg",envir=envir_stacomi)$RefStades.5,arret=TRUE)
		})

#' Multiple Choice method for RefStades referential objects
#' @note the method tests if the load is called from within a "bilan" object, and loads par, parqual, or parquan objects accordingly
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("RefStades")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object<-charge(object)
#' bilanMigrationPar=new(BilanMigrationPar)
#' objectBilan=bilan_taille # for other test
#' choicemult(object,objectBilan=bilanMigrationPar)	
#' }
setMethod("choicemult",signature=signature("RefStades"),definition=function(object,objectBilan=NULL,is.enabled=TRUE) {
			
			if (nrow(object@data) > 0){
				hstd=function(h,...){
					stades=tbdeststd[,][tbdeststd[,]!=""]
					object@data<-object@data[std_libelle%in%stades ,]
					assign("refStades",object,envir_stacomi)
					funout(get("msg",envir=envir_stacomi)$RefStades.2)
					if (!is.null(objectBilan)) {
						objectBilan@stades<-object
						assign(get("objectBilan",envir=envir_stacomi),objectBilan,envir=envir_stacomi)
						
						# suppresses all tab larger than 3 (stage))
						if (length(notebook)>4){
							for (i in 5:length(notebook)){
								svalue(notebook) <- i							
								dispose(notebook) ## dispose current tab
							}
						}
						# par defaut la methode ne charge pas de maniere interactive  (par exemple en ne prenant que les stades des taxon du dc par la methode charge_avec_filtre
						# elle est alors affichee des le debut par la methode choice e laquelle on ne passe pas d'objectBilan en parametre 
						#il y a bien un object par dans l'object Bilan 
						if (class(try(objectBilan@par,silent=TRUE))!="try-error") {
							objectBilan@par<-charge_avec_filtre(object=objectBilan@par,
									dc_selectionne=get("refDC",envir_stacomi)@dc_selectionne,
									taxon_selectionne=get("refTaxons",envir_stacomi)@data$tax_code,
									stade_selectionne=get("refStades",envir_stacomi)@data$std_code)
							choicemult(objectBilan@par,is.enabled=TRUE)
						} 
						#il y a bien un object parqual dans l'object Bilan						
						if (class(try(objectBilan@parqual,silent=TRUE))!="try-error") {
							objectBilan@parqual<-charge_avec_filtre(object=objectBilan@parqual,
									dc_selectionne=get("refDC",envir_stacomi)@dc_selectionne,
									taxon_selectionne=get("refTaxons",envir_stacomi)@data$tax_code,
									stade_selectionne=get("refStades",envir_stacomi)@data$std_code)
							choicemult(objectBilan@parqual,label=get("msg",envir=envir_stacomi)$RefStades.3,nomassign="refparqual",frameassign="frame_parqual",is.enabled=TRUE)
						}
#il y a bien un object parquan dans l'object Bilan
						if (class(try(objectBilan@parquan,silent=TRUE))!="try-error") {
							objectBilan@parquan<-charge_avec_filtre(object=objectBilan@parquan,
									dc_selectionne=get("refDC",envir_stacomi)@dc_selectionne,
									taxon_selectionne=get("refTaxons",envir_stacomi)@data$tax_code,
									stade_selectionne=get("refStades",envir_stacomi)@data$std_code)
							if (class(objectBilan)=="Bilan_taille" )
							{
								if (nrow(objectBilan@parquan@data)>0) {
									objectBilan@parquan@data=objectBilan@parquan@data[objectBilan@parquan@data$par_code=="1786"|  #taille
													objectBilan@parquan@data$par_code=="1785"|     # taille fourche
													is.na(objectBilan@parquan@data$par_code)|objectBilan@parquan@data$par_code=="C001",]     # aucune
								}
							}
							choicemult(objectBilan@parquan,label=get("msg",envir=envir_stacomi)$RefStades.4,nomassign="refparquan",frameassign="frame_parquan",is.enabled=TRUE)
						}
						if (svalue(notebook)<length(notebook)){
							svalue(notebook)<-svalue(notebook)+1	
						}
					}
				}
				# below the widget structure [=> within (=> type
				# group(ggroup)[notebook(notebook)[groupstd(ggroup&tab)[[framestdsource(gframe)[tbsourcestd(gtable)],framestddest(gframe)[tbdeststd(gtable)]],OKbutton]]
				if (!exists("notebook")) notebook <- gnotebook(container=group) 				
				#TODO addmsg
				std_libelle=fun_char_spe(object@data$std_libelle)
				groupstd<-ggroup() 
				assign("goupstd",groupstd,envir=.GlobalEnv)
				add(notebook,groupstd,label="stade")
				framestdsource<-gframe(get("msg",envir=envir_stacomi)$RefStades.6,container=groupstd)
				tbsourcestd  = gtable(std_libelle,container=framestdsource,expand = TRUE, fill = TRUE)
				size(tbsourcestd)<-c(160,300) 
				#TODO addmsg
				framestddest<-gframe("deposez ici",container=groupstd)
				# need for a fixed size data.frame otherwise errors when adding new lines
				xx<-data.frame(choice=rep("",8))
				xx$choice<-as.character(xx$choice)
				tbdeststd=gtable(xx,container=framestddest,expand = TRUE, fill = TRUE)
				size(tbdeststd)<-c(160,300)
				adddropsource(tbsourcestd)
				adddroptarget(tbdeststd)				
				adddropmotion(tbdeststd,handler=function(h,...) {
							valeurs<-tbdeststd[,]
							valeurs<-valeurs[valeurs!=""]
							if (!svalue(tbsourcestd)%in%valeurs){
								tbdeststd[length(valeurs)+1,1]<-svalue(tbsourcestd)
							}
						})
				addHandlerDoubleclick(tbsourcestd,handler=function(h,...) {
							valeurs<-tbdeststd[,]
							valeurs<-valeurs[valeurs!=""]
							if (!svalue(tbsourcestd)%in%valeurs){
								tbdeststd[length(valeurs)+1,1]<-svalue(h$obj)
							}
						})
				adddropsource(tbdeststd)
				adddroptarget(tbsourcestd)
				removestd<-function(){
					valeurs<-tbdeststd[,]
					valeurs<-valeurs[valeurs!=""]
					valeurs<-valeurs[-match(svalue(tbdeststd),valeurs)]
					tbdeststd[,]<-c(valeurs,rep("",8-length(valeurs)))
				}
				adddropmotion(tbsourcestd,handler=function(h,...) {
							removestd()
						})
				addHandlerDoubleclick(tbdeststd,handler=function(h,...) {
							removestd()
						})
				gbutton("ok", container = groupstd, handler = hstd)
			} else {
				funout(get("msg",envir=envir_stacomi)$RefDC.7,arret=TRUE)
			}
		})

#' choice_c method for RefStades
#' 
#' the choice_c method is intented to have the same behaviour as choice (which creates a
#' widget in the graphical interface) but from the command line. The values passed to the choice_c method
#' for stades is the code.  Any numeric value will be discarded
#' @param stades the vector of stages chosen
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples
#' \dontrun{
#'object=new("RefTaxon")
#'object<-charge(object)
#'objectBilan=new("BilanMigrationMult")
#' choice_c(object=object,objectBilan=objectBilan,"Anguilla anguilla")
#' }

setMethod("choice_c",signature=signature("RefStades"),definition=function(object,stades) {
			if (is.null(stades)) {
				funout(get("msg",envir=envir_stacomi)$RefStades.7,arret=TRUE)
			}
			libellemanquants<-stades[!stades%in%object@data$std_code]
			if (length(libellemanquants)>0) funout(paste(get("msg",envir=envir_stacomi)$RefStades.8,stringr::str_c(libellemanquants,collapse=", ")))
			object@data<-object@data[object@data$std_code%in%stades,]					
			if (nrow(object@data)==0 )	{
				funout(get("msg",envir=envir_stacomi)$RefTaxon.3,arret=TRUE)
			}
			assign("refStade",object,envir=envir_stacomi)
			return(object)
		})
