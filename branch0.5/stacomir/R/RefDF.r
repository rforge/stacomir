# Nom fichier :        RefDF   (classe)

#' Class "RefDF"
#' 
#' Representation of a fishway, Contains description data of all fishways from
#' the database along with the selected fishway(df) (integer)
#' 
#' 
#' @name RefDF-class
#' @aliases RefDF RefDF-class

#' @slot df_selectionne="integer"
#' @slot ouvrage="integer"
#' @slot data="data.frame"
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefDF", df_selectionne=integer(), ouvrage=integer(),
#' data=data.frame())}.  \describe{ \item{list("df_selectionne")}{Object of
#' class \code{"integer"} The identifier of the fishway}\item{:}{Object of
#' class \code{"integer"} The identifier of the fishway}
#' \item{list("ouvrage")}{Object of class \code{"integer"} The attached
#' dam}\item{:}{Object of class \code{"integer"} The attached dam}
#' \item{list("data")}{Object of class \code{"data.frame"} Data concerning the
#' fishway}\item{:}{Object of class \code{"data.frame"} Data concerning the
#' fishway} }
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
#' showClass("RefDF")
#' 
setClass(Class="RefDF",representation=
				representation(df_selectionne="integer",ouvrage="integer",data="data.frame") )
                               
#' Loading method for DF referential objects
#' @returnType S4 object
#' @return An object of class RefDF
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @examples 
#'  \dontrun {objet=new("RefDF")
#' charge(objet)}
setMethod("charge",signature=signature("RefDF"),definition=function(objet) {
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
			requete<-connect(requete) 
			objet@data<-requete@query
			return(objet)
		})

#' Choice method for DF referential objects
#' @note the choice method assigns an object of class refDF in the environment envir_stacomi
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @examples   \dontrun { 
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#'  objet=new("RefDF")
#' objet<-charge(objet)
#' choix(objet)}
setMethod("choix",signature=signature("RefDF"),definition=function(objet) {
			if (nrow(objet@data) > 0){
				hDF=function(h,...){
					objet@df_selectionne=svalue(choix)
					objet@ouvrage= objet@data$dif_ouv_identifiant[objet@data$df%in%objet@df_selectionne]
					#cat("passe par la")
					assign("refDF",objet,envir_stacomi)
					funout(get("msg",envir=envir_stacomi)$RefDF.1)
					#dispose(winst)
				} 
				# Handler d'affichage du tableau
				hDFi=function(h,...){
					w=gwindow(get("msg",envir=envir_stacomi)$RefDF.2,width=400)
					wg=ggroup(horizontal=FALSE,cont=w)
					tab=gtable(objet@data[,c(1,6,7)],chosencol=1,multiple=FALSE,expand=TRUE, container=wg)
					bg<-ggroup(cont=wg)
					addSpring(bg)
					gbutton(get("msg",envir=envir_stacomi)$RefDC.4, cont=bg, handler = function(h,...) dispose(w))
				}
				frameDF=gframe(get("msg",envir=envir_stacomi)$RefDF.3,container=group)
				DF_identifiant=objet@data$df
				choix=gdroplist(DF_identifiant,container=frameDF,handler=hDF)
				gbutton(get("msg",envir=envir_stacomi)$RefDC.6, container=frameDF,handler=hDFi)
				gbutton("OK", container=frameDF,handler=hDF)
			} else {
				funout(get("msg",envir=envir_stacomi)$RefDF.4,arret=TRUE)
			}
		})