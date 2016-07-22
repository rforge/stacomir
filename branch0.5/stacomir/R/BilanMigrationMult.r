# Nom fichier :        BilanMigration    (classe)
# Projet :             controle migrateur
# adaptation du bilan migration pour plusieurs dc, plusieurs taxons et plusieurs stades
# Date de creation :   27/12/2011 

#' class BilanMigrationMult
#' @slot dc="RefDC"
#' @slot taxons="RefTaxon"
#' @slot stades="RefStades"
#' @slot pasDeTemps="PasDeTempsJournalier"
#' @slot data="data.frame"
#' @slot duree="POSIXct"
#' @method calcule
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
setClass(Class="BilanMigrationMult",
		representation=
				representation(dc="RefDC",taxons="RefTaxon",stades="RefStades",pasDeTemps="PasDeTempsJournalier",data="data.frame",duree="POSIXct"),
		prototype=prototype(dc=new("RefDC"),
				taxons=new("RefTaxon"),
				stades=new("RefStades"),
				pasDeTemps=new("PasDeTempsJournalier"),
				data=data.frame(),
				duree=as.POSIXct(Sys.time()) 
		))
# BilanMigrationMult= new("BilanMigrationMult")

setValidity("BilanMigrationMult",function(object)
		{
			rep1=length(object@dc)>=1
			rep2=length(object@taxons)>=1
			rep3=length(object@stades)>=1
			rep3=length(object@pasDeTemps)==1
			rep4=(object@pasDeTemps@nbPas==365) # contrainte : pendant 365j
			rep5=as.numeric(strftime(object@pasDeTemps@dateDebut,'%d'))==1 # contrainte : depart = 1er janvier
			rep6=as.numeric(strftime(object@pasDeTemps@dateDebut,'%m'))==1			
			return(ifelse(rep1 & rep2 & rep3 & rep4 & rep5 & rep6, TRUE ,c(1:6)[!c(rep1, rep2, rep3, rep4, rep5, rep6)]))
		}   
)



#' connect method for BilanMigrationMult
#' @return BilanMigrationMult with slots filled by user choice
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
setMethod("charge",signature=signature("BilanMigrationMult"),definition=function(objet,...){ 
			bilanMigrationMult<-objet
			if (exists("refDC",envir_stacomi)) {
				bilanMigrationMult@dc<-get("refDC",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.1,arret=TRUE)	
			}
			if (exists("refTaxons",envir_stacomi)) {
				bilanMigrationMult@taxons<-get("refTaxons",envir_stacomi)
			} else {      
				funout(get("msg",envir_stacomi)$ref.2,arret=TRUE)
			}
			if (exists("refStades",envir_stacomi)){
				bilanMigrationMult@stades<-get("refStades",envir_stacomi)
			} else 
			{
				funout(get("msg",envir_stacomi)$ref.3,arret=TRUE)
			}
			if (exists("pasDeTemps",envir_stacomi)){
				bilanMigrationMult@pasDeTemps<-get("pasDeTemps",envir_stacomi)
				# pour permettre le fonctionnement de Fonctionnement DC
				assign("fonctionnementDC_date_debut",get("pasDeTemps",envir_stacomi)@"dateDebut",envir_stacomi)
				assign("fonctionnementDC_date_fin",as.POSIXlt(DateFin(get("pasDeTemps",envir_stacomi))),envir_stacomi)
			} else {
				# todo addmsg
				funout(get("msg",envir=envir_stacomi)$BilanMigration.1,arret=FALSE)
				warning(get("msg",envir=envir_stacomi)$BilanMigration.1)
			}
			stopifnot(validObject(bilanMigrationMult, test=TRUE))
			funout(get("msg",envir=envir_stacomi)$BilanMigration.2)
			return(bilanMigrationMult)
		})




#' calcule method for BilanMigration,does the calculation once data are filled in with the connect method
#' @return BilanMigration with slots filled by user choice
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
setMethod("calcule",signature=signature("BilanMigrationMult"),definition=function(objet,...){ 
			#browser()
#			'data.frame':	365 obs. of  10 variables:
#					$ No_pas            : int  0 1 2 3 4 5 6 7 8 9 ...
#			$ Debut_pas         : POSIXct, format: "2012-01-01" "2012-01-02" ...
#			$ Fin_pas           : POSIXct, format: "2012-01-02" "2012-01-03" ...
#			$ Mesure            : num  0 0 0 2.19 3.5 ...
#			$ Calcule           : num  0 0 0 0 0 0 0 0 0 0 ...
#			$ Expert            : num  0 0 0 0 0 0 0 0 0 0 ...
#			$ Ponctuel          : num  0 0 0 0 0 0 0 0 0 0 ...
#			$ type_de_quantite  : Factor w/ 1 level "effectif": 1 1 1 1 1 1 1 1 1 1 ...
#			$ taux_d_echappement: num  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 ...
#			$ Coef_conversion   : num  NA NA NA NA NA NA NA NA NA NA ...
			
			data<-funSousListeBilanMigration(bilanMigration=bilanMigrationMult)				
			tableau=data[,-c(2,3)]
			tableau$"Effectif_total"=rowSums(data[,c("Mesure","Calcule","Expert","Ponctuel")])				
			if(sum!=sum(tableau$"Effectif_total")) warning(paste("attention probleme, le total",sum,"est different de la somme des effectifs",sum(tableau$"Effectif_total"),"ceci peut se produire lorsque des operations sont a cheval sur plusieurs annees") )
			tableau=tableau[,c(1:5,9,6:8)] 	
			dimnames(tableau)=list(1:nrow(tableau),c(
							"No.pas",
							"Mesure",
							"Calcule",
							"Expert",
							"Ponctuel",
							"Effectif_total",
							"Type_de_quantite",
							"Taux_d_echappement",
							"Coef_conversion"
					))
			tableau$Coef_conversion=as.numeric(tableau$Coef_conversion)
			tableau$Coef_conversion[is.na(tableau$Coef_conversion)]=0
			bilanMigration@duree=seq.POSIXt(from=as.POSIXlt(min(data$Debut_pas)),to=max(data$Debut_pas),
					by=as.numeric(bilanMigration@pasDeTemps@dureePas)) # il peut y avoir des lignes repetees poids effectif
			# traitement des coefficients de conversion poids effectif
			
			if (bilanMigration@taxons@data$tax_nom_latin=="Anguilla anguilla"& bilanMigration@stades@data$std_libelle=="civelle") 
			{
				tableau <-funtraitement_poids(tableau,duree=bilanMigration@duree)
			}
			bilanMigration@data<-tableau
			assign("bilanMigration",bilanMigration,envir_stacomi)
			funout(get("msg",envir_stacomi)$BilanMigration.3)
			assign("tableau",tableau,envir_stacomi)
			funout(get("msg",envir_stacomi)$BilanMigration.4)
		})

setMethod("connect",signature=signature("BilanMigrationMult"),definition=function(objet,...){ 
			bilanMigrationMult<-objet
			# retrieve the argument of the function and passes it to bilanMigrationMult
			# easier to debug
			req=new("RequeteODBCwheredate")
			req@baseODBC<-get("baseODBC", envir=envir_stacomi)			
			req@colonnedebut<-"ope_date_debut"
			req@colonnefin<-"ope_date_fin"
			req@datedebut=as.POSIXlt(bilanMigration@pasDeTemps@dateDebut)
			req@datefin=as.POSIXlt(DateFin(bilanMigration@pasDeTemps))
			dc = vector_to_listsql(bilanMigrationMult@dc@dc_selectionne)
			tax=vector_to_listsql(bilanMigrationMult@taxons@data$tax_code)
			std=vector_to_listsql(bilanMigrationMult@stades@data$std_code)
			sch=get("sch",envir=envir_stacomi)
			req@select = str_c("SELECT lot_tax_code,
							lot_std_code,
							lot_effectif, 
							lot_quantite,
							lot_qte_code,
							lot_dev_code, 
							lot_methode_obtention",
					" FROM ",sch,"t_operation_ope",
					" JOIN ",sch,"t_lot_lot on lot_ope_identifiant=ope_identifiant")
			# removing character marks
			req@select<-str_replace_all(req@select,"[\r\n\t]" , "")
			# the where clause is returned by ODBCWheredate
			req@and=str_c(" AND ope_dic_identifiant in",dc,
					" AND lot_tax_code in ",tax,
					" AND lot_std_code in ",std,
					" AND lot_lot_identifiant IS NULL")
			req<-connect(req)
			rs=req@query	
			return(rs)	

})				

#' handler du calcul hBilanMigrationgraph
#' appelle les fonctions fungraph pour faire le bilan des migrations
#' et permet la sauvegarde des bilans journaliers dans la base
#' @note pb si autre chose que journalier les pas de temps ont été contraints à des pas de temps journaliers pour ce graphique
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
hbilanMigrationMultgraph = function(h,...) {
	if (exists("bilanMigrationMult",envir_stacomi)) {
		bilanMigration<-get("bilanMigrationMult",envir_stacomi)
	} else {      
		funout(get("msg",envir_stacomi)$BilanMigration.5,arret=TRUE)
	}
	taxon= as.character(bilanMigration@taxons@data$tax_nom_latin)
	stade= as.character(bilanMigration@stades@data$std_libelle)
	DC=as.numeric(bilanMigration@dc@dc_selectionne)	
	funout(get("msg",envir_stacomi)$BilanMigration.9)
	
	# si le bilan est journalier 
	if (bilanMigration@pasDeTemps@dureePas==86400 & bilanMigration@pasDeTemps@dureePas==86400) {
		
		# pour sauvegarder sous excel
		if (taxon=="Anguilla anguilla"& stade=="civelle") {
			fungraph_civelle(bilanMigration=bilanMigration,bilanMigration@data,bilanMigration@duree,taxon=taxon,stade=stade)
		}
		else {
			fungraph(bilanMigration=bilanMigration,tableau=bilanMigration@data,duree=bilanMigration@duree,taxon,stade)
		}
		
	} else {
		funout(get("msg",envir_stacomi)$BilanMigration.8)
		# normalement ce cas ne devrait plus se poser
	}	
	# ecriture du bilan journalier, ecrit aussi le bilan mensuel
	fn_EcritBilanJournalier(bilanMigration)
}
#' handler du graphique BilanMigrationMult
#' realise le calcul du bilan migration, l'ecrit dans l'environnement envir_stacomi
#' traite eventuellement les quantites de lots (si c'est des civelles)
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
hbilanMigrationMultcalc=function(h,...){
	bilanMigrationMult<-charge(bilanMigrationMult)
	bilanMigrationMult<-calcule(bilanMigrationMult)
}

#' handler du calcul hBilanMigrationgraph2
#' appelle les fonctions fungraph pour faire un graphe annuel des 
#' cumuls de migration au cours du temps
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
hbilanMigrationMultgraph2 = function(h,...) {
	if (exists("bilanMigrationMult",envir_stacomi)) {
		bilanMigration<-get("bilanMigrationMult",envir_stacomi)
	} else {      
		funout(get("msg",envir_stacomi)$BilanMigration.5,arret=TRUE)
	}
	taxon= as.character(bilanMigration@taxons@data$tax_nom_latin)
	stade= as.character(bilanMigration@stades@data$std_libelle)
	DC=as.numeric(bilanMigration@dc@dc_selectionne)	
	if (bilanMigration@pasDeTemps@dureePas==86400 & bilanMigration@pasDeTemps@dureePas==86400) {
		bilanMigration@data$duree=bilanMigration@duree
		# pour sauvegarder sous excel
		bilanMigration@data<-funtraitementdate(bilanMigration@data,
				nom_coldt="duree",
				annee=FALSE,
				mois=TRUE,
				quinzaine=TRUE,
				semaine=TRUE,
				jour_an=TRUE,
				jour_mois=FALSE,
				heure=FALSE)
		bilanMigration@data$Cumsum=cumsum(bilanMigration@data$Effectif_total)
		# pour sauvegarder sous excel
		annee=unique(strftime(as.POSIXlt(bilanMigration@duree),"%Y"))
		dis_commentaire=  as.character(bilanMigration@dc@data$dis_commentaires[bilanMigration@dc@data$dc%in%bilanMigration@dc@dc_selectionne]) 
		update_geom_defaults("step", aes(size = 3))
		p<-ggplot(bilanMigration@data)+
				geom_step(aes(x=duree,y=Cumsum,colour=mois))+
				ylab(get("msg",envir_stacomi)$BilanMigration.6)+
				opts(plot.title=theme_text(size=10,colour="blue"),
						title=paste(get("msg",envir_stacomi)$BilanMigration.7,dis_commentaire,", ",taxon,", ",stade,", ",annee,sep=""))   
		print(p)	
	} else {
		funout(get("msg",envir_stacomi)$BilanMigration.8)
	}
}

#' handler du calcul BilanMigrationstat : traitements 
#' appelle les fonctions funstat et funtable pour faire le bilan des migrations
#' dans des fichiers csv
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
hTableBilanMigrationMult=function(h,...) {
	funout("Tableau de sortie \n")
	if (exists("bilanMigration",envir_stacomi)) 
	{
		bilanMigration<-get("bilanMigration",envir_stacomi)
	} 
	else 
	{      
		funout(get("msg",envir_stacomi)$BilanMigration.5,arret=TRUE)
	}
	taxon= as.character(bilanMigration@taxons@data$tax_nom_latin)
	stade= as.character(bilanMigration@stades@data$std_libelle)
	DC=as.numeric(bilanMigration@dc@dc_selectionne)	
	funout(get("msg",envir_stacomi)$BilanMigration.9)  	
	resum=funstat(tableau=bilanMigration@data,duree=bilanMigration@duree,taxon,stade,DC)
	funtable(tableau=bilanMigration@data,duree=bilanMigration@duree,taxon,stade,DC,resum)
}