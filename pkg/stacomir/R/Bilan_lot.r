# Nom fichier :        Bilanlot.R    (classe)
# Date de creation :   07/02/2009 21:30:54
# Compatibilite :      
# Etat :               Fonctionne
#                     L'affichage de cette classe est gere par interface_bilan_lot
#**********************************************************************                                                                  
#* Modifications :
#* ---------------
#* Les boites en chargent plus l'ensemble de la liste mais seulement celles ayant une correspondance dans la base
#* Adaptation à ggplotusr v1.0
#* integration des msg pour internationalisation
#********************************************
#' @name Bilan_lot
#' @title Bilan_lot Bilan class calls the content of the table of characteristics of lot
#' @note This class is displayed by interface_bilan_lot
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @slot data="data.frame"
#' @slot dc="RefDC"
#' @slot taxons="RefTaxon"
#' @slot stades="RefStades"
#' @slot par="Refpar"
#' @slot horodate="RefHorodate"
#' @slot requete="RequeteODBCwheredate")
#' @method connect
#' @method charge
#' @expamples objet=new("Bilan_lot")
setClass(Class="Bilan_lot",
		representation= representation(data="data.frame",
				dc="RefDC",taxons="RefTaxon",
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
# Methode pour donner les attributs de la classe RequeteODBCwheredate correspondant à l'objet fonctionnement DC
#' connect method for Bilan_lot
#' @return An object of class bilan_lot Bilan_lot
#' @param h a handler
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
setMethod("connect",signature=signature("Bilan_lot"),definition=function(objet,h) {
#  construit une requete ODBCwheredate
			objet@requete@baseODBC=baseODBC
			objet@requete@select= paste("SELECT * FROM ",get("sch",envir=envir_stacomi),"vue_lot_ope_car",sep="")
			objet@requete@colonnedebut="ope_date_debut"
			objet@requete@colonnefin="ope_date_fin"
			objet@requete@order_by="ORDER BY ope_date_debut"
			objet@requete@and=paste(" AND ope_dic_identifiant=",objet@dc@dc_selectionne,
					" AND lot_tax_code= '", objet@taxons@data$tax_code,
					"' AND lot_std_code= '", objet@stades@data$std_code,
					"' AND car_par_code='", objet@par@data$par_code, "'",sep="")
#objet@requete@where=#defini dans la methode ODBCwheredate
			objet@requete<-connect(objet@requete) # appel de la methode connect de l'objet ODBCWHEREDATE
			objet@data<-objet@requete@query
			funout(get("msg",envir_stacomi)$Bilan_lot.1)
			return(objet)
		})
# Cette methode permet de verifier que les boites ont ete cliquees et va chercher les
# objets qui sont colles dans l'environnement envir_stacomi de l'interface 
#objet<-bilan_lot
#' charge method for Bilan_lot class
#' @returnType an instance for class Bilan_lot
#' @return Bilan_lot with slots filled with user choice
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
#  objet<-bilan_lot
setMethod("charge",signature=signature("Bilan_lot"),definition=function(objet,h) {
			if (exists("refDC",envir_stacomi)) {
				objet@dc<-get("refDC",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.1,arret=TRUE)
			} 
			if (exists("refTaxons",envir_stacomi)) {
				objet@taxons<-get("refTaxons",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.2,arret=TRUE)
			}
			if (exists("refStades",envir_stacomi)) {
				objet@stades<-get("refStades",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.3,arret=TRUE)
			}
			if (exists("refpar",envir_stacomi)) {
				objet@par<-get("refpar",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.4,arret=TRUE)
			}		
			# rem pas tres satisfaisant car ce nom est choisi dans l'interface
			if (exists("bilan_lot_date_debut",envir_stacomi)) {
				objet@requete@datedebut<-get("bilan_lot_date_debut",envir_stacomi)@horodate
			} else {
				funout(get("msg",envir_stacomi)$ref.5,arret=TRUE)
			}
			# rem id
			if (exists("bilan_lot_date_fin",envir_stacomi)) {
				objet@requete@datefin<-get("bilan_lot_date_fin",envir_stacomi)@horodate
			} else {
				funout(get("msg",envir_stacomi)$ref.6,arret=TRUE)
			}         
			objet<-connect(objet)
			
			return(objet)
		})
# Methode permettant l'affichage d'un graphique en lattice (barchart) du fonctionnement mensuel du dispositif
# Compte tenu de la structure des donnees ce n'est pas si simple... 

funbarchartBilan_lot = function(h,...) {
# TODO  developper cette fonction
}   

funboxBilan_lot = function(h,...) {
# TODO developper cette fonction
}


#' fungraphInteract_lot calls the ggplotusr interface to draw plots
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
fungraphInteract_lot = function(h,...) {
	bilan_lot=charge(bilan_lot)
	bilan_lot@par@data$par_nom
	if(nrow(bilan_lot@data)==0) {
		funout(get("msg",envir_stacomi)$Bilan_lot.2, arret=TRUE)
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
	funout(get("msg",envir_stacomi)$Bilan_lot.3)
	ggplot2usr("vue_ope_lot")
	funout(get("msg",envir_stacomi)$Bilan_lot.4)
	ggi$list_aes[[2]][["droplist"]]$setValue("val_quant")
	ggi$list_aes[[1]][["droplist"]]$setValue("date")
}  


#' funtableBilan_lot shows a table of results in gdf
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
funtableBilan_lot = function(h,...) {
	bilan_lot=charge(bilan_lot)
	vue_ope_lot=bilan_lot@requete@query # on recupere le data.frame
	assign("bilan_lot",bilan_lot,envir_stacomi)#assign("bilan_lot",vue_ope_lot,envir_stacomi)
	funout(get("msg",envir_stacomi)$Bilan_lot.3)
	vue_ope_lot[is.na(vue_ope_lot)]<-""
	vue_ope_lot$ope_date_debut=as.character(vue_ope_lot$ope_date_debut)
	vue_ope_lot$ope_date_fin=as.character(vue_ope_lot$ope_date_fin)   
	gdf(vue_ope_lot, container=TRUE)
}
