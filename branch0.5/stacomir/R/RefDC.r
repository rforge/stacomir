# Nom fichier :        RefDC   (classe)

#' Class "RefDC"
#' 
#' Description of a control device.
#' 
#' 
#' @name RefDC-class
#' @aliases RefDC-class RefDC

#' @slot dc_selectionne="integer"
#' @slot ouvrage="integer"
#' @slot station="character"
#' @slot data="data.frame"
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefDC", dc_selectionne=integer(), ouvrage=integer(),
#' data=data.frame())}.  \describe{ \item{list("dc_selectionne")}{Object of
#' class \code{"integer"}The selected device}\item{:}{Object of class
#' \code{"integer"}The selected device} \item{list("ouvrage")}{Object of class
#' \code{"integer"} The attached dam}\item{:}{Object of class \code{"integer"}
#' The attached dam} \item{list("station")}{Object of class \code{"character"}
#' The attached migration monitoring station, this is necessary to join the
#' table of escapments calculated at the station level}\item{:}{Object of class
#' \code{"character"} The attached migration monitoring station, this is
#' necessary to join the table of escapments calculated at the station level}
#' \item{list("data")}{Object of class \code{"data.frame"} data pertaining to
#' the control device}\item{:}{Object of class \code{"data.frame"} data
#' pertaining to the control device} }
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
#' @examples
#' 
#' showClass("RefDC")
#' 
setClass(Class="RefDC",representation=
				representation(dc_selectionne="integer",ouvrage="integer",station="character",data="data.frame") ,
		prototype=prototype(dc_selectionne=integer(),ouvrage=integer(),station=character(),data=data.frame()),
		validity=)


#' validity check for class RefDC
#' @describeIn RefDC
setValidity("RefDC",function(object)
		{
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
#' Referential class RefDC loads the counting devices of the control station
#' @returnType Object of class RefDC
#' @return Object of class RefDC
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
setMethod("charge",signature=signature("RefDC"),definition=function(objet) {
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
			requete<-connect(requete) 
			#funout("La requete est effectuee pour charger les Dispositifs de comptage \n")
			objet@data<-requete@query
			return(objet)
		})
#' choice method for RefDC
#' @note   The choix method has for arguments a report (bilan) object
#'  (e.g) is called from a report Bilan(e.g Bilan_lot).
#'   By default,  the value of the objetbilan is null.
#'   When it is not   the method calls daughter widgets (e.g. the dc widget will call species) 
#' and fills it with the method \link charge_avec_filtre #' @param objetBilan un objet bilan
#' @param is.enabled a boolean indincating # see if deprecated...
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @examples \dontrun{ win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' objet=new("RefDC")
#'	objet<-charge(objet)
#'	objetBilan=new("BilanMigration")
#' choix(objet=objet,objetBilan=objetBilan)}
setMethod("choix",signature=signature("RefDC"),definition=function(objet,objetBilan=NULL,is.enabled=TRUE) {
			if (nrow(objet@data) > 0){
				hDC=function(h,...){
					objet@dc_selectionne<-svalue(choix)
					objet@ouvrage= objet@data$dif_ouv_identifiant[objet@data$dc%in%objet@dc_selectionne]
					objet@station=objet@data$sta_code[objet@data$dc%in%objet@dc_selectionne]
					assign("refDC",objet,envir_stacomi)
					funout(get("msg",envir=envir_stacomi)$RefDC.1)
					# si il existe un objet fils; supprimer
					# referentiel fils, celui charge par la methode charge_avec_filtre
					# ici comme on fait appel ï¿½ un autre objet il faut appeller le conteneur qui contient l'objet
					if (!is.null(objetBilan)) {
						# ci dessous pas d'appel de charge_avec_filtre pour les bilanEspeces (tous les taxons)
						if("RefTaxon"%in%as.character(getSlots(class(objetBilan)))){
							objetBilan@taxons<<-charge_avec_filtre(objet=objetBilan@taxons,dc_selectionne=get("refDC",objet,envir_stacomi)@dc_selectionne)
							if (exists("frame_tax")) delete(group,frame_tax)
							if (exists("frame_std")) delete(group,frame_std)
							if (exists("frame_par")) delete(group,frame_par)
							if (exists("frame_parquan")) delete(group,frame_parquan)
							if (exists("frame_parqual")) delete(group,frame_parqual)
							choix(objetBilan@taxons,objetBilan,is.enabled=TRUE)
							funout(get("msg",envir=envir_stacomi)$RefDC.2)	
						}
					}
					#dispose(winst)
				} 
				# Handler d'affichage du tableau
				hDCi=function(h,...){
					w=gwindow(get("msg",envir=envir_stacomi)$RefDC.3,width=400)
					wg=ggroup(horizontal=FALSE,cont=w)
					tab=gtable(objet@data[,c(1,4,7,8,11,12)],chosencol=1,multiple=FALSE,expand=TRUE, container=wg)
					bg<-ggroup(cont=wg)
					addSpring(bg)
					gbutton(get("msg",envir=envir_stacomi)$RefDC.4, cont=bg, handler = function(h,...) dispose(w))
				}
				frame_DC<<-gframe(get("msg",envir=envir_stacomi)$RefDC.5)
				add(group,frame_DC)
				DC_identifiant=objet@data$dc
				choix=gdroplist(DC_identifiant,container=frame_DC,handler=hDC)
				gbutton(get("msg",envir=envir_stacomi)$RefDC.6, container=frame_DC,handler=hDCi) 
				enabled(frame_DC)<-is.enabled
				gbutton("OK", container=frame_DC,handler=hDC)
			} else {
				funout(get("msg",envir=envir_stacomi)$RefDC.7,arret=TRUE)
			}
			return(objet)
		})
# pour test #choix(objet)



#' choixmult, selection method for refDC allowing to select several DC
#' @note   The choix method has for arguments a report (bilan) object
#'  (e.g) is called from a report Bilan(e.g Bilan_lot).
#'   By default,  the value of the objetbilan is null.
#'   When it is not   the method calls daughter widgets (e.g. the dc widget will call species) 
#' and fills it with the method \link charge_avec_filtre #' @param objetBilan un objet bilan
#' @param is.enabled a boolean indicating if the widget is to be selectable
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @examples \dontrun{ win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' objet=new("RefDC")
#'	objet<-charge(objet)
#'	objetBilan=new("BilanMigrationMult")
#' choixmult(objet=objet,objetBilan=objetBilan)}
setMethod("choixmult",signature=signature("RefDC"),definition=function(objet,objetBilan=NULL,is.enabled=TRUE) {
			if (nrow(objet@data) > 0){
				hDC=function(h,...){
					objet@dc_selectionne<-as.integer(tbdestdc[,][tbdestdc[,]!=""])
					objet@ouvrage= objet@data$dif_ouv_identifiant[objet@data$dc%in%objet@dc_selectionne]
					objet@station= objet@data$sta_code[objet@data$dc%in%objet@dc_selectionne]
					assign("refDC",objet,envir_stacomi)
					funout(get("msg",envir=envir_stacomi)$RefDC.1)
					# si il existe un objet fils; supprimer
					# referentiel fils, celui charge par la methode charge_avec_filtre
					# ici comme on fait appel à un autre objet il faut appeller le conteneur qui contient l'objet
					if (!is.null(objetBilan)) {
						# ci dessous pas d'appel de charge_avec_filtre pour les bilanEspeces (tous les taxons)
						if("RefTaxon"%in%as.character(getSlots(class(objetBilan)))){
							
						
							objetBilan@dc<-objet
							objetBilan@taxons<-charge_avec_filtre(objet=objetBilan@taxons,dc_selectionne=get("refDC",envir_stacomi)@dc_selectionne)
							# the name was created by the interface
							# as I can't get the name from within the function (deparse(substitute(objetBilan does not return
							# "bilanMigrationMult"
							assign(get("objetBilan",envir=envir_stacomi),objetBilan,envir=envir_stacomi)
							# suppresses all tab larger than 1 (dc)
							if (length(notebook)>2){
								for (i in 3:length(notebook)){
									svalue(notebook) <- i							
									dispose(notebook) ## dispose current tab
								}}
							choixmult(objetBilan@taxons,objetBilan,is.enabled=TRUE)
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
				
				DC=objet@data[,c("dc","dis_commentaires","type_dc")]
				#TODO addmsg
				groupdc<<-ggroup(cont=notebook, label="dc")   ## "add" called by constructor this is a tab of the notebook
				frameDCsource<-gframe(get("msg",envir=envir_stacomi)$RefDC.5,cont=groupdc)
				size(frameDCsource)<-c(250,300)
				tbsourcedc  = gtable(DC,cont=frameDCsource,expand = TRUE, fill = TRUE)
				
				frameDCdest<-gframe("deposez ici",cont=groupdc)
				size(frameDCdest)<-c(60,300)
				#addSpring(groupdc)
				# need for a fixed size data.frame otherwise errors when adding new lines
				xx<-data.frame(choix=rep("",8))
				xx$choix<-as.character(xx$choix)
				tbdestdc=gtable(xx,cont=frameDCdest,expand=TRUE, fill=TRUE)
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
				gbutton("ok", cont = groupdc, handler = hDC)
			} else {
				funout(get("msg",envir=envir_stacomi)$RefDC.7,arret=TRUE)
			}
			return(objet)
		})


#' load method for RefDC
#' 
#' the load method is intented to have the same behaviour as choix (which creates a
#' widget in the graphical interface) but from the command line.  The parameters for dc are transformed to integer as the RefDC only 
#' takes integer in the dc slots. The method also loads the stations and ouvrages (dams) associated with the counting device (dc).
#' The values passed to the load function are then checked with the setValidty method.
#' Finally, if an objetBilan is passed as a parameter, the method will do a charge_avec_filtre to select only the taxa present in the counting devices
#' @param dc a character vector of dc chosen
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @family load functions
#' @example
#' \dontrun{
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#'objet=new("RefDC")
#'objet<-charge(objet)
#'objetBilan=new("BilanMigrationMult")
#' load(objet=objet,objetBilan=objetBilan,dc=1)}
setMethod("load",signature=signature("RefDC"),definition=function(objet,dc) {
			if (class(dc)=="numeric") {
				dc<-as.integer(dc) 
			}else if	(class(dc)=="character"){
				dc=as.integer(as.numeric(dc))
			}
			if (any(is.na(dc))) stop ("NA values dc")
			

			objet@dc_selectionne<-dc
			validObject(objet) 		
# the method validObject verifies that the dc is in the data slot of RefDC			

			objet@ouvrage= objet@data$dif_ouv_identifiant[ objet@data$dc%in% objet@dc_selectionne]
			objet@station= objet@data$sta_code[ objet@data$dc%in% objet@dc_selectionne]
			objet@ouvrage= objet@data$dif_ouv_identifiant[objet@data$dc%in%objet@dc_selectionne]
			assign("refDC",objet,envir=envir_stacomi)
			return(objet)
		})