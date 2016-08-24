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
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Bilan_taille", ...)}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Classes \code{\linkS4class{Bilan_carlot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @concept Bilan Object 
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
#' @return bilan_taille with requete field filled
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("connect",signature=signature("Bilan_taille"),definition=function(object,h) {
#  construit une requeteODBC (la requete est trop compliquee pour pouvoir utiliser ODBCwheredate)
			object@requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			if (object@parquan@data$par_nom=="aucune" & object@parqual@data$par_nom=="aucune") {
				funout(get("msg",envir=envir_stacomi)$Bilan_taille.1,arret=TRUE)
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
			funout(get("msg",envir=envir_stacomi)$l1)
			return(object)
		})

#object=new("Bilan_taille")
#' charge method for Bilan_taille
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("charge",signature=signature("Bilan_taille"),definition=function(object) {
			if (exists("refDC",envir_stacomi)) {
				object@dc<-get("refDC",envir_stacomi)
			} else {
				funout(get("msg",envir=envir_stacomi)$ref.1,arret=TRUE)
			} 
			if (exists("refTaxons",envir_stacomi)) {
				object@taxons<-get("refTaxons",envir_stacomi)
			} else {
				funout(get("msg",envir=envir_stacomi)$ref.2,arret=TRUE)
				
			}
			if (exists("refStades",envir_stacomi)) {
				object@stades<-get("refStades",envir_stacomi)
			} else {
				funout(get("msg",envir=envir_stacomi)$ref.3,arret=TRUE)
			}
			if (exists("refparquan",envir_stacomi)){
				object@parquan<-get("refparquan",envir_stacomi)
			} else 
			{
				funout(get("msg",envir=envir_stacomi)$ref.7,arret=TRUE)
			}
			if (exists("refparqual",envir_stacomi)){
				object@parqual<-get("refparqual",envir_stacomi)
			} else 
			{
				funout(get("msg",envir=envir_stacomi)$ref.8,arret=TRUE)
				
			}         
			# rem pas tres satisfaisant car ce nom est choisi dans l'interface
			if (exists("bilan_taille_date_debut",envir_stacomi)) {
				object@datedebut<-get("bilan_taille_date_debut",envir_stacomi)@horodate
			} else {
				funout(get("msg",envir=envir_stacomi)$ref.5,arret=TRUE)
			}
			# rem id
			if (exists("bilan_taille_date_fin",envir_stacomi)) {
				object@datefin<-get("bilan_taille_date_fin",envir_stacomi)@horodate
			} else {
				funout(get("msg",envir=envir_stacomi)$ref.6,arret=TRUE)
			} 
			funout(get("msg",envir=envir_stacomi)$Bilan_taille.2)        
			object<-connect(object)
			
			return(object)
		})
hcalculeBilanTaille<-function(h,...){
	calcule(h$action)
}
#' Calcule method for BilanTaille
#' @param h 
#' @param ... 
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
			funout(get("msg",envir=envir_stacomi)$Bilan_taille.3)
			funout(get("msg",envir=envir_stacomi)$Bilan_taille.4)
			funout(get("msg",envir=envir_stacomi)$Bilan_taille.5)
			funout(get("msg",envir=envir_stacomi)$Bilan_taille.6)	
			enabled(toolbarlist[["Grint"]])<-TRUE
		})

#' fungraphInteract_tail uses the ggplot2usr interface to build the graphes
#' @param h a handler
#' @param ... 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
fungraphInteract_tail = function(h,...) {
	if(!exists(x="bilan_taille",envir=envir_stacomi)) {
		funout(get("msg",envir=envir_stacomi)$Bilan_taille.7)
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
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
	funtableBilan_tail = function(h,...) {
		bilan_taille=charge(bilan_taille)
		vue=bilan_taille@requete@query # on recupere le data.frame
		assign("bilan_taille",bilan_taille,envir_stacomi)#assign("bilan_lot",vue,envir_stacomi)
		funout(get("msg",envir=envir_stacomi)$Bilan_taille.3)
		vue[is.na(vue)]<-""
		vue$ope_date_debut=as.character(vue$ope_date_debut)
		vue$ope_date_fin=as.character(vue$ope_date_fin)   
		gdf(vue, container=TRUE)
	}
