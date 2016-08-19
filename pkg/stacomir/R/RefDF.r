# Nom fichier :        RefDF   (classe)

#' @title RefDF referential class to load the DF dataframe
#' @note Contains description data of all DF from the database along with the selected df (integer)
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
#' @slot df_selectionne="integer"
#' @slot ouvrage="integer"
#' @slot data="data.frame"
#' @expamples \ dontrun objet=new("RefDF")
setClass(Class="RefDF",representation=
				representation(df_selectionne="integer",ouvrage="integer",data="data.frame") )
                               
#' Loading method for DF referential objects
#' @returnType S4 object
#' @return An object of class RefDF
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
#' @expamples 
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
			requete<-stacomirtools::connect(requete) 
			objet@data<-requete@query
			return(objet)
		})

#' Choice method for DF referential objects
#' @note the choice method assigns an object of class refDF in the environment envir_stacomi
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
#' @expamples   \dontrun { 
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#'  objet=new("RefDF")
#' objet<-charge(objet)
#' choice(objet)}
setMethod("choice",signature=signature("RefDF"),definition=function(objet) {
			if (nrow(objet@data) > 0){
				hDF=function(h,...){
					objet@df_selectionne=svalue(choice)
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
				choice=gdroplist(DF_identifiant,container=frameDF,handler=hDF)
				gbutton(get("msg",envir=envir_stacomi)$RefDC.6, container=frameDF,handler=hDFi)
				gbutton("OK", container=frameDF,handler=hDF)
			} else {
				funout(get("msg",envir=envir_stacomi)$RefDF.4,arret=TRUE)
			}
		})