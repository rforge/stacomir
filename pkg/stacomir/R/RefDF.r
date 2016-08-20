# Nom fichier :        RefDF   (classe)

#' Class "RefDF"
#' 
#' Representation of a fishway, Contains description data of all fishways from
#' the database along with the selected fishway(df) (integer)
#' Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefDF", df_selectionne=integer(), ouvrage=integer(),
#' data=data.frame())}.  
#' 
#' @slot df_selectionne Object of class \code{"integer"} The identifier of the fishway
#' @slot ouvrage Object of class \code{"integer"} The attached dam
#' @slot data Object of class \code{"data.frame"} Data concerning the fishway
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso Other referential classes 
#' \code{\linkS4class{RefAnnee}}
#' \code{\linkS4class{RefCheckBox}} 
#' \code{\linkS4class{RefChoix}}
#' \code{\linkS4class{RefCoe}} 
#' \code{\linkS4class{RefDC}}
#' \code{\linkS4class{RefDF}} 
#' \code{\linkS4class{RefListe}}
#' \code{\linkS4class{Refpar}} 
#' \code{\linkS4class{Refparqual}}
#' \code{\linkS4class{Refparquan}} 
#' \code{\linkS4class{RefPoidsMoyenPeche}}
#' \code{\linkS4class{RefStades}} 
#' \code{\linkS4class{RefStationMesure}}
#' \code{\linkS4class{RefTaxon}}
#' @family Referential objects
setClass(Class="RefDF",representation=
				representation(df_selectionne="integer",ouvrage="integer",data="data.frame") )
                               
#' Loading method for DF referential objects
#' 
#' @return An object of class RefDF
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("RefDF")
#' charge(object)
#' }
setMethod("charge",signature=signature("RefDF"),definition=function(object) {
			requete=new("RequeteODBC")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@sql = paste("select dis_identifiant as DF,",
					" dis_date_creation,",
					" dis_date_suppression,",
					" dis_commentaires,",
					" dif_ouv_identifiant,",
					" ouv_libelle,",
					" dif_code as DF_code,",
					" dif_localisation,",
					" dif_orientation,",
					" tdf_libelle as type_DF",
					" from ",get("sch",envir=envir_stacomi),"tg_dispositif_dis",
					" JOIN ",get("sch",envir=envir_stacomi),"t_dispositiffranchissement_dif ON dif_dis_identifiant=dis_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"tj_dfesttype_dft ON dif_dis_identifiant=dft_df_identifiant",
					" JOIN ",get("sch",envir=envir_stacomi),"t_ouvrage_ouv on dif_ouv_identifiant=ouv_identifiant",   
					" JOIN ref.tr_typedf_tdf ON tdf_code=dft_tdf_code",
					" ORDER BY dis_identifiant;",sep="")
			requete<-stacomirtools::connect(requete) 
			object@data<-requete@query
			return(object)
		})

#' Choice method for DF referential objects
#' 
#' @note the choice method assigns an object of class refDF in the environment envir_stacomi
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples  
#'  \dontrun{ 
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object=new("RefDF")
#' object<-charge(object)
#' choice(object)
#' }
setMethod("choice",signature=signature("RefDF"),definition=function(object) {
			if (nrow(object@data) > 0){
				hDF=function(h,...){
					object@df_selectionne=svalue(choice)
					object@ouvrage= object@data$dif_ouv_identifiant[object@data$df%in%object@df_selectionne]
					#cat("passe par la")
					assign("refDF",object,envir_stacomi)
					funout(get("msg",envir=envir_stacomi)$RefDF.1)
					#dispose(winst)
				} 
				# Handler d'affichage du tableau
				hDFi=function(h,...){
					w=gwindow(get("msg",envir=envir_stacomi)$RefDF.2,width=400)
					wg=ggroup(horizontal=FALSE,cont=w)
					tab=gtable(object@data[,c(1,6,7)],chosencol=1,multiple=FALSE,expand=TRUE, container=wg)
					bg<-ggroup(cont=wg)
					addSpring(bg)
					gbutton(get("msg",envir=envir_stacomi)$RefDC.4, cont=bg, handler = function(h,...) dispose(w))
				}
				frameDF=gframe(get("msg",envir=envir_stacomi)$RefDF.3,container=group)
				DF_identifiant=object@data$df
				choice=gdroplist(DF_identifiant,container=frameDF,handler=hDF)
				gbutton(get("msg",envir=envir_stacomi)$RefDC.6, container=frameDF,handler=hDFi)
				gbutton("OK", container=frameDF,handler=hDF)
			} else {
				funout(get("msg",envir=envir_stacomi)$RefDF.4,arret=TRUE)
			}
		})