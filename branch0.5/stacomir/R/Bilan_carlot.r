#' Class "Bilan_carlot"
#' 
#' Bilan_carlot Bilan class calls the content of the postgres view vue_lot_ope_car, it displays the
#' results of a categorical variable, or quantitative variable attached for lot, for instance,
#' it can be used to analyse size or sex
#' 
#' @note This class is displayed by interface_bilan_lot
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Bilan_carlot", ...)}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Class \code{\linkS4class{Bilan_carlot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} 
#' \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} 
#' \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @references
#' \url{http://w3.eptb-vilaine.fr:8080/tracstacomi/wiki/Recette\%20BilanLot}
#' @examples
#' 
#' showClass("Bilan_carlot")
#' object=new("Bilan_carlot")
#' 
#' @export 
setClass(Class="Bilan_carlot",
		representation= representation(
				data="data.frame",
				dc="RefDC",
				taxons="RefTaxon",
				stades="RefStades",
				par="Refpar",
				horodate="RefHorodate",
				requete="RequeteODBCwheredate"),
		prototype=prototype(data=data.frame(),
				dc=new("RefDC"),
				taxons=new("RefTaxon"),
				stades=new("RefStades"),
				par=new("Refpar"),
				horodate=new("RefHorodate"),
				requete=new("RequeteODBCwheredate")
		))
#

#' connect method for Bilan_carlot
#' 
#' @return An object of class bilan_lot Bilan_carlot
#' @param h a handler
#' @param ... additional parameters passed to the method
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("connect",signature=signature("Bilan_carlot"),definition=function(object,h,...) {
#  construit une requete ODBCwheredate
			object@requete@baseODBC=get("baseODBC",envir=envir_stacomi)
			object@requete@select= paste("SELECT * FROM ",get("sch",envir=envir_stacomi),"vue_lot_ope_car",sep="")
			object@requete@colonnedebut="ope_date_debut"
			object@requete@colonnefin="ope_date_fin"
			object@requete@order_by="ORDER BY ope_date_debut"
			object@requete@and=paste(" AND ope_dic_identifiant=",object@dc@dc_selectionne,
					" AND lot_tax_code= '", object@taxons@data$tax_code,
					"' AND lot_std_code= '", object@stades@data$std_code,
					"' AND car_par_code='", object@par@data$par_code, "'",sep="")
#object@requete@where=#defini dans la methode ODBCwheredate
			object@requete<-connect(object@requete) # appel de la methode connect de l'object ODBCWHEREDATE
			object@data<-object@requete@query
			funout(get("msg",envir_stacomi)$Bilan_carlot.1)
			return(object)
		})


#' charge method for Bilan_carlot class
#' 
#' this method verifies that boxes have been clicked in the user interface and gets the objects pasted in 
#' envir_stacomi
#' @return Bilan_carlot with slots filled with user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
#' @return An object of the class
setMethod("charge",signature=signature("Bilan_carlot"),definition=function(object,h) {
			if (exists("refDC",envir_stacomi)) {
				object@dc<-get("refDC",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.1,arret=TRUE)
			} 
			if (exists("refTaxons",envir_stacomi)) {
				object@taxons<-get("refTaxons",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.2,arret=TRUE)
			}
			if (exists("refStades",envir_stacomi)) {
				object@stades<-get("refStades",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.3,arret=TRUE)
			}
			if (exists("refpar",envir_stacomi)) {
				object@par<-get("refpar",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.4,arret=TRUE)
			}		
			# rem pas tres satisfaisant car ce nom est choisi dans l'interface
			if (exists("bilan_lot_date_debut",envir_stacomi)) {
				object@requete@datedebut<-get("bilan_lot_date_debut",envir_stacomi)@horodate
			} else {
				funout(get("msg",envir_stacomi)$ref.5,arret=TRUE)
			}
			# rem id
			if (exists("bilan_lot_date_fin",envir_stacomi)) {
				object@requete@datefin<-get("bilan_lot_date_fin",envir_stacomi)@horodate
			} else {
				funout(get("msg",envir_stacomi)$ref.6,arret=TRUE)
			}         
			object<-connect(object)
			
			return(object)
		})

#' Calcule method for Bilan_carlot
#' 
#' 
#' @param h A handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("calcule",signature=signature("Bilan_carlot"),definition=function(object,h) {
			bilan_lot<-object
			if(nrow(bilan_lot@data)==0) {
				funout(get("msg",envir_stacomi)$Bilan_carlot.2, arret=TRUE)
			}   
			vue_ope_lot=bilan_lot@requete@query # on recupere le data.frame
			nom_variable=bilan_lot@par@data$par_nom
			stopifnot(length(nom_variable)==1)
			vue_ope_lot$ope_dic_identifiant=as.factor(vue_ope_lot$ope_dic_identifiant)
			vue_ope_lot$dev_code=as.factor(vue_ope_lot$dev_code)
			vue_ope_lot$car_val_identifiant=as.factor(vue_ope_lot$car_val_identifiant)
			vue_ope_lot$car_par_code=as.factor(vue_ope_lot$car_par_code)
			vue_ope_lot$ope_identifiant=as.factor(vue_ope_lot$ope_identifiant)
			vue_ope_lot$lot_pere=as.factor(vue_ope_lot$lot_pere)
			vue_ope_lot$val_libelle=as.factor(vue_ope_lot$val_libelle)
			vue_ope_lot$lot_tax_code=as.factor(vue_ope_lot$lot_tax_code)
			vue_ope_lot<-funtraitementdate(data=vue_ope_lot,
					nom_coldt="ope_date_debut",
					annee=TRUE,
					mois=TRUE,
					quinzaine=TRUE,
					semaine=TRUE,
					jour_an=TRUE,
					jour_mois=FALSE,
					heure=FALSE)
			vue_ope_lot=chnames(vue_ope_lot,
					c("ope_identifiant","lot_identifiant","ope_dic_identifiant","lot_pere",             
							"ope_date_debut","ope_date_fin","lot_effectif","lot_quantite","lot_tax_code","lot_std_code","tax_nom_latin","std_libelle","dev_code","dev_libelle","par_nom","car_par_code","car_methode_obtention","car_val_identifiant",    "car_valeur_quantitatif","val_libelle", "annee","mois","quinzaine","semaine","jour_365"),
					c("ope","lot","dic","lot_pere",             
							"date","date_fin","effectif","quantite","lot_tax_code","lot_std_code","tax","std","dev_code","dev","par","car_par_code","meth","val","val_quant","val_libelle", "annee","mois","quinzaine","semaine","jour"))
			vue_ope_lot=vue_ope_lot[,c("ope","lot","dic","lot_pere","date","effectif","quantite","tax","std","dev","par","meth","val","val_quant","val_libelle", "annee","mois","quinzaine","semaine","jour")]
			bilan_lot@data<-vue_ope_lot
			assign("bilan_lot",bilan_lot,envir_stacomi)#assign("bilan_lot",vue_ope_lot,envir_stacomi)
			assign("vue_ope_lot",vue_ope_lot,envir=.GlobalEnv)
			funout(get("msg",envir_stacomi)$Bilan_carlot.3)
			return(bilan_lot)
		})


#' fundensityBilan_carlot uses ggplot2 to draw density plots
#' 
#' assigns an object g in envir_stacomi for eventual modification of the plot
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
fundensityBilan_carlot = function(h,...) {
	bilan_lot<-charge(bilan_lot)
	bilan_lot<-calcule(bilan_lot)
	g<-ggplot(bilan_lot@data,aes(x=val_quant))
	g<-g+stat_density(aes(ymax = ..density..,  ymin = -..density..),
					fill = "grey50", colour = "grey10",
					geom = "ribbon", position = "identity") +
			facet_grid(. ~ annee) +
			coord_flip()
	print(g) 
	assign("g",g,envir_stacomi)
	funout(get("msg",envir_stacomi)$Bilan_carlot.4)
}

#' Boxplots for ggplot2
#' 
#' assigns an object g in envir_stacomi for eventual modification of the plot
#' @param h A handler passed by the graphical interface
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
funboxplotBilan_carlot = function(h,...) {
	bilan_lot<-charge(bilan_lot)
	bilan_lot<-calcule(bilan_lot)
	g<-ggplot(bilan_lot@data)
	g<-g+geom_boxplot(aes(x=quinzaine,y=val_quant))
	print(g) 
	assign("g",g,envir_stacomi)
	funout(get("msg",envir_stacomi)$Bilan_carlot.4)
}


#' Point graph from ggplot
#' 
#' assigns an object g in envir_stacomi for eventual modification of the plot
#' @param h handler passed by the graphical interface
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
funpointBilan_carlot = function(h,...) {
	bilan_lot<-charge(bilan_lot)
	bilan_lot<-calcule(bilan_lot)
	g<-ggplot(bilan_lot@data)
	g<-g+geom_point(aes(x=date,y=val_quant))
	print(g) 
	assign("g",g,envir_stacomi)
	funout(get("msg",envir_stacomi)$Bilan_carlot.4)

}  

#' table function
#' 
#' funtableBilan_carlot shows a table of results in gdf
#' @param h hanlder passed by the graphical interface
#' @param ... 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
funtableBilan_carlot = function(h,...) {
	bilan_lot=charge(bilan_lot)
	vue_ope_lot=bilan_lot@requete@query # on recupere le data.frame
	assign("bilan_lot",bilan_lot,envir_stacomi)#assign("bilan_lot",vue_ope_lot,envir_stacomi)
	funout(get("msg",envir_stacomi)$Bilan_carlot.3)
	vue_ope_lot[is.na(vue_ope_lot)]<-""
	vue_ope_lot$ope_date_debut=as.character(vue_ope_lot$ope_date_debut)
	vue_ope_lot$ope_date_fin=as.character(vue_ope_lot$ope_date_fin)   
	gdf(vue_ope_lot, container=TRUE)
}
