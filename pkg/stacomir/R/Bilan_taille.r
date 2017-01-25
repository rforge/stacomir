# Nom fichier :        Bilantaille.R    (classe)

#' Class "Bilan_taille" Bilan class
#' 
#' Bilan_taille class This class allows to load the following quantitative
#' sample characteristics : full size, mostly used for yellow eels.
#' 
#' Eels trapped are usually screened and separated into groups, this is not
#' good practise but still in use The groups used in stacomi are based on the
#' selectivity curve according to the mesh, roughly the separation takes place
#' at the L50 for Arzal So individuals might refer to one size class (eg the
#' size group of large eels) This affiliation is sought at the subsample level
#' by a query to display on a single line two different characteristics size
#' and size group Once the data are loaded, the program checks if the size
#' class group is indicated at the sample or susample level
#' @slot data A data frame
#' @slot dc An object of class \link{RefDC-class}
#' @slot taxons An object of class \link{RefTaxon-class}
#' @slot stades An object of class \link{RefStades-class} inherited from \link{BilanMigration-class}
#' @slot parquan An object of class \link{Refparquan-class}, quantitative parameter 
#' @slot parqual An object of class \link{Refparqual-class}, quanlitative parameter
#' @slot horodate An object of class link{RefHorodate-class} 
#' @slot requete An Object of class \link[stacomirtools]{RequeteODBC-class}
#' @slot datedebut A POSIXlt value, starting horodate for the Bilan
#' @slot datefin A POSIXlt value, ending horodate for the Bilan
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Bilan_taille", ...)}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @family Bilan Objects
#' @keywords classes 
#' @examples
#' 
#' showClass("Bilan_taille")
#' @export 
setClass(Class="Bilan_taille",
		representation= representation(data="data.frame",
				dc="RefDC",
				taxons="RefTaxon",
				stades="RefStades",
				parquan="Refparquan",
				parqual="Refparqual",
				horodate="RefHorodate",
				requete="RequeteODBC",
				datedebut="POSIXlt",
				datefin="POSIXlt"),
		prototype=prototype(data=data.frame(),
				dc=new("RefDC"),
				taxons=new("RefTaxon"),
				stades=new("RefStades"),
				parquan=new("Refparquan"),
				parqual=new("Refparqual"),
				horodate=new("RefHorodate"),
				requete=new("RequeteODBC")))

#' connect method for class Bilan_taille
#' @param object An object of class \code{\link{Bilan_taille-class}} 
#' @return An object of class \code{\link{Bilan_taille-class}}  with requete field filled
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("connect",signature=signature("Bilan_taille"),definition=function(object) {
#  construit une requeteODBC (la requete est trop compliquee pour pouvoir utiliser ODBCwheredate)
			object@requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			if (object@parquan@data$par_nom=="aucune" & object@parqual@data$par_nom=="aucune") {
				funout(gettext(get("msg",envir=envir_stacomi)$Bilan_taille.1),arret=TRUE)
			} else if (object@parquan@data$par_nom=="aucune") {
				#caracteristique qualitative uniquement
				sql=paste("SELECT * FROM ",get("sch",envir=envir_stacomi),"vue_ope_lot_ech_parqual" ,
						" WHERE ope_dic_identifiant ='",object@dc@dc_selectionne,"'",
						" AND lot_tax_code = '",object@taxons@data$tax_code,"'" ,
						" AND lot_std_code = '",object@stades@data$std_code,"'" ,
						" AND car_par_code = '",object@parqual@data$par_code,"'" ,
						" AND (ope_date_debut, ope_date_fin) OVERLAPS (DATE '",object@datedebut,"',DATE '",object@datefin,"') " ,
						" ORDER BY ope_date_debut",sep="")
				
			} else if (object@parqual@data$par_nom=="aucune") {
				# Caracteristique quantitative uniquement
				sql=paste("SELECT * FROM ",get("sch",envir=envir_stacomi),"vue_ope_lot_ech_parquan",
						" WHERE ope_dic_identifiant ='",object@dc@dc_selectionne,"'",
						" AND lot_tax_code = '",object@taxons@data$tax_code,"'" ,
						" AND lot_std_code = '",object@stades@data$std_code,"'" ,
						" AND car_par_code = '",object@parquan@data$par_code,"'" ,
						" AND (ope_date_debut, ope_date_fin) OVERLAPS (DATE '",object@datedebut,"',DATE '",object@datefin,"') " ,
						" ORDER BY ope_date_debut",sep="")
				
			} else {
				#les deux caracteristiques sont choisies, il faut faire un Bilancroise
				# attention je choisis un left  join ea veut dire certaines caracteristiques quant n'ont pas de contrepartie qualitatives
				# Pour essai voir Anguilles_nombreouPoids2.sql
				# --Bilan croise
				sql=paste(
						#"--colonnes communes aux deux tableaux", 
						#" -- tableau donnant les lots et sous lots contenant un poids pour anguille",
						"SELECT * FROM (",
						"SELECT * FROM ",get("sch",envir=envir_stacomi),"vue_ope_lot_ech_parquan", 
						" WHERE ope_dic_identifiant ='",object@dc@dc_selectionne,"'",
						" AND lot_tax_code = '",object@taxons@data$tax_code,"'" ,
						" AND lot_std_code = '",object@stades@data$std_code,"'" ,
						" AND car_par_code = '",object@parquan@data$par_code,"'" ,
						" AND (ope_date_debut, ope_date_fin) OVERLAPS (TIMESTAMP '",object@datedebut,"',TIMESTAMP '",object@datefin,"') " ,
						" ) AS qan",
						" LEFT JOIN", 
						#" --tableau donnant les lots et sous lots contenant le type de caracteristique" ,
						" (SELECT lot_identifiant as lot_identifiant1,car_val_identifiant ",
						"  FROM ",get("sch",envir=envir_stacomi),"vue_ope_lot_ech_parqual", 
						" WHERE ope_dic_identifiant ='",object@dc@dc_selectionne,"'",
						" AND lot_tax_code = '",object@taxons@data$tax_code,"'" ,     
						" AND lot_std_code = '",object@stades@data$std_code,"'" ,
						" AND car_par_code = '",object@parqual@data$par_code,"'" ,
						" AND (ope_date_debut, ope_date_fin) OVERLAPS (TIMESTAMP '",object@datedebut,"',TIMESTAMP '",object@datefin,"') " ,
						" )as qal ",
						" ON qan.lot_identifiant=qal.lot_identifiant1",
						" ORDER BY ope_date_debut",sep="")
			}
			object@requete@sql=sql	
#object@requete@where=#defini dans la methode ODBCwheredate
			object@requete<-stacomirtools::connect(object@requete) # appel de la methode stacomirtools::connect de l'object requeteODBC
			funout(gettext(get("msg",envir=envir_stacomi)$l1))
			return(object)
		})

#object=new("Bilan_taille")
#' charge method for Bilan_taille
#' @param object An object of class \code{\link{Bilan_taille-class}} 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("charge",signature=signature("Bilan_taille"),definition=function(object) {
			if (exists("refDC",envir_stacomi)) {
				object@dc<-get("refDC",envir_stacomi)
			} else {
				funout(gettext(get("msg",envir=envir_stacomi)$ref.1),arret=TRUE)
			} 
			if (exists("refTaxon",envir_stacomi)) {
				object@taxons<-get("refTaxon",envir_stacomi)
			} else {
				funout(gettext(get("msg",envir=envir_stacomi)$ref.2),arret=TRUE)
				
			}
			if (exists("refStades",envir_stacomi)) {
				object@stades<-get("refStades",envir_stacomi)
			} else {
				funout(gettext(get("msg",envir=envir_stacomi)$ref.3),arret=TRUE)
			}
			if (exists("refparquan",envir_stacomi)){
				object@parquan<-get("refparquan",envir_stacomi)
			} else 
			{
				funout(gettext(get("msg",envir=envir_stacomi)$ref.7),arret=TRUE)
			}
			if (exists("refparqual",envir_stacomi)){
				object@parqual<-get("refparqual",envir_stacomi)
			} else 
			{
				funout(gettext(get("msg",envir=envir_stacomi)$ref.8),arret=TRUE)
				
			}         
			# rem pas tres satisfaisant car ce nom est choisi dans l'interface
			if (exists("bilan_taille_date_debut",envir_stacomi)) {
				object@datedebut<-get("bilan_taille_date_debut",envir_stacomi)@horodate
			} else {
				funout(gettext(get("msg",envir=envir_stacomi)$ref.5),arret=TRUE)
			}
			# rem id
			if (exists("bilan_taille_date_fin",envir_stacomi)) {
				object@datefin<-get("bilan_taille_date_fin",envir_stacomi)@horodate
			} else {
				funout(gettext(get("msg",envir=envir_stacomi)$ref.6),arret=TRUE)
			} 
			funout(gettext(get("msg",envir=envir_stacomi)$Bilan_taille.2)        )
			object<-connect(object)
			
			return(object)
		})
hcalculeBilanTaille<-function(h,...){
	calcule(h$action)
}
#' Calcule method for BilanTaille
#' @param object An object of class \code{\link{Bilan_taille-class}} 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("calcule",signature=signature("Bilan_taille"),definition=function(object) {
			bilan_taille<-object
			bilan_taille=charge(bilan_taille)
			vue=bilan_taille@requete@query # on recupere le data.frame
			vue$ope_dic_identifiant=as.factor(vue$ope_dic_identifiant)
			vue$dev_code=as.factor(vue$dev_code)
			vue$lot_pere_dev_code=as.factor(vue$lot_pere_dev_code)
			if (!is.null(vue$car_val_identifiant)){
				vue$car_val_identifiant=as.factor(vue$car_val_identifiant)
			}
			vue$lot_pere_val_identifiant=as.factor(vue$lot_pere_val_identifiant)
			vue$lot_pereid=as.factor(vue$lot_pere)
			vue$lot_pere[!is.na(vue$lot_pere)]<-"sous lot"
			vue$lot_pere[is.na(vue$lot_pere)]<-"lot"
			vue$lot_pere=as.factor(vue$lot_pere)
			vue$car_par_code=as.factor(vue$car_par_code)
			vue$ope_identifiant=as.factor(vue$ope_identifiant) 
			vue$lot_tax_code=as.factor(vue$lot_tax_code)
			vue<-funtraitementdate(data=vue,
					nom_coldt="ope_date_debut",
					annee=TRUE,
					mois=TRUE,
					quinzaine=TRUE,
					semaine=TRUE,
					jour_an=TRUE,
					jour_mois=FALSE,
					heure=FALSE)                         
			bilan_taille@data <- vue
			assign("bilan_taille",bilan_taille,envir_stacomi)#assign("bilan_lot",vue,envir_stacomi)
			funout(gettext(get("msg",envir=envir_stacomi)$Bilan_taille.3))
			funout(gettext(get("msg",envir=envir_stacomi)$Bilan_taille.4))
			funout(gettext(get("msg",envir=envir_stacomi)$Bilan_taille.5))
			funout(gettext(get("msg",envir=envir_stacomi)$Bilan_taille.6)	)
			enabled(toolbarlist[["Grint"]])<-TRUE
		})

#' fungraphInteract_tail uses the ggplot2usr interface to build the graphes
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
fungraphInteract_tail = function(h,...) {
	if(!exists(x="bilan_taille",envir=envir_stacomi)) {
		funout(gettext(get("msg",envir=envir_stacomi)$Bilan_taille.7))
	} 	else 
	{
		bilan_taille=get("bilan_taille",envir=envir_stacomi)
	}
	g<-ggplot(bilan_taille@data,aes(x=ope_date_debut,y=car_valeur_quantitatif))
	if (bilan_taille@parqual@data$par_nom=="aucune"){
		g<-g+geom_point(aes(color="car_val_identifiant"))
		g<-g+xlab("date")+ylab("taille (mm)")
		print(g)
	}else {
		g<-g+geom_point(aes(color=car_val_identifiant))
		g<-g+xlab("date")+ylab("taille (mm)")
		print(g)
	}
}	
	
#
#' function used to display a table of the data
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
	funtableBilan_tail = function(h,...) {
		bilan_taille=charge(bilan_taille)
		vue=bilan_taille@requete@query # on recupere le data.frame
		assign("bilan_taille",bilan_taille,envir_stacomi)#assign("bilan_lot",vue,envir_stacomi)
		funout(gettext(get("msg",envir=envir_stacomi)$Bilan_taille.3))
		vue[is.na(vue)]<-""
		vue$ope_date_debut=as.character(vue$ope_date_debut)
		vue$ope_date_fin=as.character(vue$ope_date_fin)   
		gdf(vue, container=TRUE)
	}
