# Nom fichier :        RefDC   (classe)

#' Class "RefDC"
#' 
#' Description of a control device.
#' 
#' @include create_generic.r
#' @slot dc_selectionne Object of class \code{"integer"}, The selected device
#' @slot ouvrage Object of class \code{"integer"}, the attached dam
#' @slot station Object of class \code{"character"}, the attached migration monitoring station, this is necessary to join the
#' table of escapments calculated at the station level.
#' @slot data Object of class \code{"data.frame"} data pertaining to the control device
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefDC", dc_selectionne=integer(), ouvrage=integer(),
#' data=data.frame())}.  
#' @author cedric.briand"at"eptb-vilaine.fr
#' @keywords classes
#' @family Referential objects
setClass(Class="RefDC",representation=
				representation(dc_selectionne="integer",ouvrage="integer",station="character",data="data.frame") ,
		prototype=prototype(dc_selectionne=integer(),ouvrage=integer(),station=character(),data=data.frame())
)



setValidity("RefDC",method=function(object){
			if (length(object@dc_selectionne)!=0){		
				if (nrow(object@data)>0) {
					concord<-object@dc_selectionne%in%object@data$dc					
					if (any(!concord)){
						return(paste("No data for DC",object@dc_selectionne[!concord]))
						
					} else {
						return(TRUE)
					}
				} else {
					return("You tried to set a value for dc_selectionne without initializing the data slot")
				}
			}  else return(TRUE)
			
		}   
)


#' Method to load the counting devices of the control station
#' @param object An object of class \link{RefDC-class}
#' @return Object of class RefDC
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("charge",signature=signature("RefDC"),definition=function(object) {
			requete=new("RequeteODBC")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@sql=  paste("select dis_identifiant as DC,",
					" dis_date_creation,",
					" dis_date_suppression,",
					" dif_dis_identifiant as DF,",
					" dis_commentaires,",
					" dif_ouv_identifiant,",
					" ouv_libelle,",
					" dif_code as DF_code,",
					" dic_code as DC_code,",
					" dif_localisation,",
					" dif_orientation,",
					" tdf_libelle as type_DF,",
					" tdc_libelle as type_DC,",
					"sta_code",
					" FROM ",get("sch",envir=envir_stacomi),"tg_dispositif_dis",
					" JOIN ",get("sch",envir=envir_stacomi),"t_dispositifcomptage_dic ON dic_dis_identifiant =dis_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"t_dispositiffranchissement_dif ON dif_dis_identifiant=dic_dif_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"tj_dfesttype_dft ON dif_dis_identifiant=dft_df_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"t_ouvrage_ouv on dif_ouv_identifiant=ouv_identifiant", 
					" JOIN ",get("sch",envir=envir_stacomi),"t_station_sta on ouv_sta_code=sta_code", 
					" JOIN ref.tr_typedf_tdf ON tdf_code=dft_tdf_code",
					" JOIN ref.tr_typedc_tdc ON dic_tdc_code=tdc_code",
					" WHERE  dft_rang=1",
					" ORDER BY dis_identifiant;",sep="")
			requete<-stacomirtools::connect(requete) 
			#funout("La requete est effectuee pour charger les Dispositifs de comptage \n")
			object@data<-requete@query
			return(object)
		})



#' Graphical method to choose a fishway through the interface
#' 
#' @note   The choice method has for arguments a report (bilan) object
#'  (e.g) is called from a report Bilan(e.g Bilan_carlot).
#'   By default,  the value of the objectbilan is null.
#'   When it is not   the method calls daughter widgets (e.g. the dc widget will call species) 
#' and fills it with the method \link{charge_avec_filtre,RefTaxon-method} 
#' @param object An object of class \link{RefDC-class}
#' @param objectBilan the objectBilan from which this frame was called
#' @param is.enabled a boolean indicating whether the current frame will be enabled. Selecting a "parent"
#' frame may cause some frame to be disabled. When created the frame is enabled.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples \dontrun{ 
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object=new("RefDC")
#' object<-charge(object)
#' objectBilan=new("BilanMigration")
#' choice(object=object,objectBilan=objectBilan)
#'}
setMethod("choice",signature=signature("RefDC"),definition=function(object,objectBilan=NULL,is.enabled=TRUE) {
			if (nrow(object@data) > 0){
				hDC=function(h,...){
					object@dc_selectionne<-svalue(choice)
					object@ouvrage= object@data$dif_ouv_identifiant[object@data$dc%in%object@dc_selectionne]
					object@station=object@data$sta_code[object@data$dc%in%object@dc_selectionne]
					assign("refDC",object,envir_stacomi)
					funout(get("msg",envir=envir_stacomi)$RefDC.1)
					# si il existe un object fils; supprimer
					# referentiel fils, celui charge par la methode charge_avec_filtre
					# ici comme on fait appel e un autre object il faut appeller le conteneur qui contient l'object
					if (!is.null(objectBilan)) {
						# ci dessous pas d'appel de charge_avec_filtre pour les bilanEspeces (tous les taxons)
						# pas non plus d'appel pour les bilanArgentee dont les slots taxon, stade ,et par sont fixes
						if("RefTaxon"%in%as.character(getSlots(class(objectBilan)))&class(objectBilan)!="BilanArgentee"&class(objectBilan)!="BilanAgedemer"){
							objectBilan@taxons<<-charge_avec_filtre(object=objectBilan@taxons,dc_selectionne=get("refDC",object,envir_stacomi)@dc_selectionne)
							if (exists("frame_tax")) delete(group,frame_tax)
							if (exists("frame_std")) delete(group,frame_std)
							if (exists("frame_par")) delete(group,frame_par)
							if (exists("frame_parquan")) delete(group,frame_parquan)
							if (exists("frame_parqual")) delete(group,frame_parqual)
							choice(objectBilan@taxons,objectBilan,is.enabled=TRUE)
							funout(get("msg",envir=envir_stacomi)$RefDC.2)	
						}
					}
					#dispose(winst)
				} 
				# Handler d'affichage du tableau
				hDCi=function(h,...){
					w=gwindow(get("msg",envir=envir_stacomi)$RefDC.3,width=400)
					wg=ggroup(horizontal=FALSE,container=w)
					tab=gtable(object@data[,c(1,4,7,8,11,12)],chosencol=1,multiple=FALSE,expand=TRUE, container=wg)
					bg<-ggroup(container=wg)
					addSpring(bg)
					gbutton(get("msg",envir=envir_stacomi)$RefDC.4, container=bg, handler = function(h,...) dispose(w))
				}
				frame_DC<<-gframe(get("msg",envir=envir_stacomi)$RefDC.5)
				add(group,frame_DC)
				DC_identifiant=object@data$dc
				choice=gdroplist(DC_identifiant,container=frame_DC,handler=hDC)
				gbutton(get("msg",envir=envir_stacomi)$RefDC.6, container=frame_DC,handler=hDCi) 
				enabled(frame_DC)<-is.enabled
				gbutton("OK", container=frame_DC,handler=hDC)
			} else {
				funout(get("msg",envir=envir_stacomi)$RefDC.7,arret=TRUE)
			}
			return(object)
		})
# pour test #choice(object)



#' choicemult, selection method for refDC allowing to select several DC
#' 
#' @note   The choice method has for arguments a report (bilan) object
#'  (e.g) is called from a report Bilan(e.g Bilan_carlot).
#'   By default,  the value of the objectbilan is null.
#'   When it is not   the method calls daughter widgets (e.g. the dc widget will call species) 
#' and fills it with the method \link{charge_avec_filtre,RefTaxon-method}
#' @param object An object of class RefDC
#' @param objectBilan A bilan object
#' @param is.enabled A boolean indicating if the widget can be seleted at launch
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{ 
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object=new("RefDC")
#' object<-charge(object)
#' objectBilan=new("BilanMigrationMult")
#' choicemult(object=object,objectBilan=objectBilan)
#'}
setMethod("choicemult",signature=signature("RefDC"),definition=function(object,objectBilan=NULL,is.enabled=TRUE) {
			
			if (nrow(object@data) > 0){
				hDC=function(h,...){
					#browser()
					object@dc_selectionne<-as.integer(tbdestdc[,][tbdestdc[,]!=""])
					object@ouvrage= object@data$dif_ouv_identifiant[object@data$dc%in%object@dc_selectionne]
					object@station= as.character(object@data$sta_code[object@data$dc%in%object@dc_selectionne])
					assign("refDC",object,envir_stacomi)
					funout(get("msg",envir=envir_stacomi)$RefDC.1)
					# si il existe un object fils; supprimer
					# referentiel fils, celui charge par la methode charge_avec_filtre
					# ici comme on fait appel e un autre object il faut appeller le conteneur qui contient l'object
					if (!is.null(objectBilan)) {
						# ci dessous pas d'appel de charge_avec_filtre pour les bilanEspeces (tous les taxons)
						if("RefTaxon"%in%as.character(getSlots(class(objectBilan)))){
							
							
							objectBilan@dc<-object
							objectBilan@taxons<-charge_avec_filtre(object=objectBilan@taxons,dc_selectionne=get("refDC",envir_stacomi)@dc_selectionne)
							# the name was created by the interface
							# as I can't get the name from within the function (deparse(substitute(objectBilan does not return
							# "bilanMigrationMult"
							assign(get("objectBilan",envir=envir_stacomi),objectBilan,envir=envir_stacomi)
							# suppresses all tab larger than 1 (dc)
							if (length(notebook)>2){
								for (i in 3:length(notebook)){
									svalue(notebook) <- i							
									dispose(notebook) ## dispose current tab
								}}
							choicemult(objectBilan@taxons,objectBilan,is.enabled=TRUE)
							funout(get("msg",envir=envir_stacomi)$RefDC.2)	
						}
					}
					# changing tab of notebook to next tab
					if (svalue(notebook)<length(notebook)){
						svalue(notebook)<-svalue(notebook)+1	
					}
					#dispose(winst)
				} 
				# Handler d'affichage du tableau
				# below the widget structure [=> within (=> type
				# group(ggroup)[nb(notebook)[groupdc(ggroup&tab)[[frameDCsource(gframe)[tbsourcedc(gtable)],frameDCdest(gframe)[tbdcdest(gtable)]],OKbutton]]
				
				DC=object@data[,c("dc","dis_commentaires","type_dc")]
				#TODO addmsg
				groupdc<-ggroup(container=notebook, label="dc")   ## "add" called by constructor this is a tab of the notebook
				assign("groupdc",groupdc,envir=.GlobalEnv)
				frameDCsource<-gframe(get("msg",envir=envir_stacomi)$RefDC.5,container=groupdc)
				size(frameDCsource)<-c(250,300)
				tbsourcedc  = gtable(DC,container=frameDCsource,expand = TRUE, fill = TRUE)
				
				frameDCdest<-gframe("deposez ici",container=groupdc)
				size(frameDCdest)<-c(60,300)
				#addSpring(groupdc)
				# need for a fixed size data.frame otherwise errors when adding new lines
				xx<-data.frame(choice=rep("",12))
				xx$choice<-as.character(xx$choice)
				tbdestdc=gtable(xx,container=frameDCdest,expand=TRUE, fill=TRUE)
				adddropsource(tbsourcedc)
				adddroptarget(tbdestdc)				
				adddropmotion(tbdestdc,handler=function(h,...) {
							valeurs<-tbdestdc[,]
							valeurs<-valeurs[valeurs!=""]
							if (!svalue(tbsourcedc)%in%valeurs){
								tbdestdc[length(valeurs)+1,1]<-svalue(tbsourcedc)
							}
						})
				addHandlerDoubleclick(tbsourcedc,handler=function(h,...) {
							valeurs<-tbdestdc[,]
							valeurs<-valeurs[valeurs!=""]
							if (!svalue(tbsourcedc)%in%valeurs){
								tbdestdc[length(valeurs)+1,1]<-svalue(h$obj)
							}
						})
				adddropsource(tbdestdc)
				adddroptarget(tbsourcedc)
				removedc<-function(){
					valeurs<-tbdestdc[,]
					valeurs<-valeurs[valeurs!=""]
					valeurs<-valeurs[-match(svalue(tbdestdc),valeurs)]
					tbdestdc[,]<-c(valeurs,rep("",8-length(valeurs)))
				}
				adddropmotion(tbsourcedc,handler=function(h,...) {
							removedc()
						})
				addHandlerDoubleclick(tbdestdc,handler=function(h,...) {
							removedc()
						})
				gbutton("ok", container = groupdc, handler = hDC)
			} else {
				funout(get("msg",envir=envir_stacomi)$RefDC.7,arret=TRUE)
			}
			return(object)
		})


#' Command line interface to select a counting device
#' 
#' the choice_c method is intented to have the same behaviour as choice (which creates a
#' widget in the graphical interface) but from the command line.  The parameters for dc are transformed to integer as the RefDC only 
#' takes integer in the dc slots. The method also loads the stations and ouvrages (dams) associated with the counting device (dc).
#' The values passed to the choice_c method are then checked with the setValidty method.
#' Finally, if an objectBilan is passed as a parameter, the method will do a charge_avec_filtre to select only the taxa present in the counting devices
#' @param object an object of class RefDC
#' @param dc a character vector of dc chosen
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples
#' \dontrun{
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object=new("RefDC")
#' object<-charge(object)
#' objectBilan=new("BilanMigrationMult")
#' choice_c(object=object,objectBilan=objectBilan,dc=1)
#' }
setMethod("choice_c",signature=signature("RefDC"),definition=function(object,dc) {
			if (class(dc)=="numeric") {
				dc<-as.integer(dc) 
			}else if	(class(dc)=="character"){
				dc=as.integer(as.numeric(dc))
			}
			if (any(is.na(dc))) stop ("NA values dc")
			
			
			object@dc_selectionne<-dc
			validObject(object) 		
# the method validObject verifies that the dc is in the data slot of RefDC			
			
			object@station<- as.character(object@data$sta_code[ object@data$dc%in% object@dc_selectionne])
			object@ouvrage<-object@data$dif_ouv_identifiant[object@data$dc%in%object@dc_selectionne]
			assign("refDC",object,envir=envir_stacomi)
			return(object)
		})