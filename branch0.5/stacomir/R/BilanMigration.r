# Nom fichier :        BilanMigration    (classe)
# Projet :             controle migrateur calmig/prog/classe
# Date de creation :   31/03/2008 17:21:18

#' Class "BilanMigration"
#' 
#' Balance of fish migrations
#' 
#' 
#' @name BilanMigration-class
#' @aliases BilanMigration BilanMigration-class

#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanMigration",
#' dc=new("RefDC"),taxons=("RefTaxon"),stades=("RefStades"),pasDeTemps=("PasDeTempsJournalier"),data=data.frame(),
#' duree=new(POSIXct) )}.  
#' \describe{ 
#' \item{list("dc")}{Object of class
#' \code{"RefDC"}: the control device }
#' \item{:}{Object of class \code{"RefDC"}:
#' the control device } \item{list("taxons")}{Object of class
#' \code{"RefTaxon"}: the taxa of the fish}
#' \item{:}{Object of class
#' \code{"RefTaxon"}: the taxa of the fish} 
#' \item{list("stades")}{Object of
#' class \code{"RefStades"} : the stage of the fish}\item{:}{Object of class
#' \code{"RefStades"} : the stage of the fish} 
#' \item{list("pasDeTemps")}{Object
#' of class \code{"PasDeTempsJournalier"} : the time step constrained to daily
#' value and 365 days}
#' \item{:}{Object of class \code{"PasDeTempsJournalier"} :
#' the time step constrained to daily value and 365 days}
#' \item{list("data")}{Object of class \code{"data.frame"} :
#' data}
#' \item{:}{Object of class \code{"data.frame"} : data}
#' \item{list("duree")}{Object of class \code{"POSIXct"} : duration of the
#' analysis}
#' \item{:}{Object of class \code{"POSIXct"} : duration of the
#' analysis} }
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Class \code{\linkS4class{Bilan_lot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @references \url{http://w3.eptb-vilaine.fr:8080/tracstacomi}
#' @examples
#' 
#' showClass("BilanMigration")
#' bilanMigration= new("BilanMigration")
#' 
#' @export 
setClass(Class="BilanMigration",
		representation=
				representation(dc="RefDC",taxons="RefTaxon",stades="RefStades",pasDeTemps="PasDeTempsJournalier",data="data.frame",duree="POSIXct"),
		prototype=prototype(dc=new("RefDC"),
				taxons=new("RefTaxon"),
				stades=new("RefStades"),
				pasDeTemps=new("PasDeTempsJournalier"),
				data=data.frame(),
				duree=as.POSIXct(Sys.time()) 
		))
# bilanMigration= new("BilanMigration")

setValidity("BilanMigration",function(object)
		{
			rep1=length(object@dc)==1
			rep2=length(object@taxons)==1
			rep3=length(object@stades)==1
			rep3=length(object@pasDeTemps)==1
			rep4=(object@pasDeTemps@nbPas==365) # contrainte : pendant 365j
			rep5=as.numeric(strftime(object@pasDeTemps@dateDebut,'%d'))==1 # contrainte : depart = 1er janvier
			rep6=as.numeric(strftime(object@pasDeTemps@dateDebut,'%m'))==1
			
			return(ifelse(rep1 & rep2 & rep3 & rep4 & rep5 & rep6, TRUE ,c(1:6)[!c(rep1, rep2, rep3, rep4, rep5, rep6)]))
		}   
)

#' handler du graphique BilanMigration
#' realise le calcul du bilan migration, l'ecrit dans l'environnement envir_stacomi
#' traite eventuellement les quantites de lots (si c'est des civelles)
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
hbilanMigrationcalc=function(h,...){
	calcule( h$action)
}


#' calcule method for BilanMigration
#' @return BilanMigration with slots filled by user choice
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
setMethod("calcule",signature=signature("BilanMigration"),definition=function(objet,...){ 
			bilanMigration<-objet
			#pour l'instant ne lancer que si les fenetre sont fermees
			# funout("lancement updateplot \n")
			if (exists("refDC",envir_stacomi)) {
				bilanMigration@dc<-get("refDC",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.1,arret=TRUE)	
			}
			if (exists("refTaxons",envir_stacomi)) {
				bilanMigration@taxons<-get("refTaxons",envir_stacomi)
			} else {      
				funout(get("msg",envir_stacomi)$ref.2,arret=TRUE)
			}
			if (exists("refStades",envir_stacomi)){
				bilanMigration@stades<-get("refStades",envir_stacomi)
			} else 
			{
				funout(get("msg",envir_stacomi)$ref.3,arret=TRUE)
			}
			if (exists("pasDeTemps",envir_stacomi)){
				bilanMigration@pasDeTemps<-get("pasDeTemps",envir_stacomi)
				# pour permettre le fonctionnement de Fonctionnement DC
				assign("fonctionnementDC_date_debut",get("pasDeTemps",envir_stacomi)@"dateDebut",envir_stacomi)
				assign("fonctionnementDC_date_fin",as.POSIXlt(DateFin(get("pasDeTemps",envir_stacomi))),envir_stacomi)
			} else {
				funout(get("msg",envir=envir_stacomi)$BilanMigration.1,arret=FALSE)
				warning(get("msg",envir=envir_stacomi)$BilanMigration.1)
			}
			stopifnot(validObject(bilanMigration, test=TRUE))
			funout(get("msg",envir=envir_stacomi)$BilanMigration.2)
			sum<-funBilanMigrationAnnuel(bilanMigration=bilanMigration)
			if (!is.na(sum)){
				data<-funSousListeBilanMigration(bilanMigration=bilanMigration)
				tableau=data[,-c(2,3)]
				tableau$"Effectif_total"=rowSums(data[,c("MESURE","CALCULE","EXPERT","PONCTUEL")])
				if(sum!=sum(tableau$"Effectif_total")) warning(paste("attention probleme, le total",sum,"est different de la somme des effectifs",sum(tableau$"Effectif_total"),"ceci peut se produire lorsque des operations sont a cheval sur plusieurs annees") )
				tableau=tableau[,c(1:5,9,6:8)] 	
				dimnames(tableau)=list(1:nrow(tableau),c(
								"No.pas",
								"MESURE",
								"CALCULE",
								"EXPERT",
								"PONCTUEL",
								"Effectif_total",
								"type_de_quantite",
								"Taux_d_echappement",
								"coe_valeur_coefficient"
						))
				tableau$coe_valeur_coefficient=as.numeric(tableau$coe_valeur_coefficient)
				tableau$coe_valeur_coefficient[is.na(tableau$coe_valeur_coefficient)]=0
				bilanMigration@duree=seq.POSIXt(from=as.POSIXlt(min(data$debut_pas)),to=max(data$debut_pas),
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
			} else {
				# no fish...
				funout(get("msg",envir_stacomi)$BilanMigration.10)
			}
		})



#' handler du calcul hBilanMigrationgraph
#' appelle les fonctions fungraph pour faire le bilan des migrations
#' et permet la sauvegarde des bilans journaliers dans la base
#' @note pb si autre chose que journalier les pas de temps ont �t� contraints � des pas de temps journaliers pour ce graphique
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
hbilanMigrationgraph = function(h,...) {
	if (exists("bilanMigration",envir_stacomi)) {
		bilanMigration<-get("bilanMigration",envir_stacomi)
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

#' handler du calcul hBilanMigrationgraph2
#' appelle les fonctions fungraph pour faire un graphe annuel des 
#' cumuls de migration au cours du temps
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
hbilanMigrationgraph2 = function(h,...) {
	if (exists("bilanMigration",envir_stacomi)) {
		bilanMigration<-get("bilanMigration",envir_stacomi)
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
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
hTableBilanMigration=function(h,...) {
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