# Nom fichier :        BilanMigration    (classe)
# Projet :             controle migrateur calmig/prog/classe
# Date de creation :   31/03/2008 17:21:18

#' Class "BilanMigration"
#' 
#' Balance of fish migrations
#' 
#' @include RefTaxon.r
#' @include RefStades.r
#' @include PasDeTempsJournalier.r
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanMigration",
#' dc=new("RefDC"),taxons=("RefTaxon"),stades=("RefStades"),pasDeTemps=("PasDeTempsJournalier"),data=data.frame(),
#' time.sequence=new(POSIXct) )}.  
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
#' \item{list("time.sequence")}{Object of class \code{"POSIXct"} : duration of the
#' analysis}
#' \item{:}{Object of class \code{"POSIXct"} : duration of the
#' analysis} }
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Class \code{\linkS4class{Bilan_carlot}}
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
#' @export 
setClass(Class="BilanMigration",
		representation=
				representation(dc="RefDC",taxons="RefTaxon",stades="RefStades",pasDeTemps="PasDeTempsJournalier",data="data.frame",time.sequence="POSIXct"),
		prototype=prototype(dc=new("RefDC"),
				taxons=new("RefTaxon"),
				stades=new("RefStades"),
				pasDeTemps=new("PasDeTempsJournalier"),
				data=data.frame(),
				time.sequence=as.POSIXct(Sys.time()) 
		))
# bilanMigration= new("BilanMigration")

setValidity("BilanMigration",function(object)
		{
			rep1=length(object@dc)==1
			rep2=length(object@taxons)==1
			rep3=length(object@stades)==1
			rep3=length(object@pasDeTemps)==1
			rep4=(object@pasDeTemps@nbStep==365) # contrainte : pendant 365j
			rep5=as.numeric(strftime(object@pasDeTemps@dateDebut,'%d'))==1 # contrainte : depart = 1er janvier
			rep6=as.numeric(strftime(object@pasDeTemps@dateDebut,'%m'))==1
			
			return(ifelse(rep1 & rep2 & rep3 & rep4 & rep5 & rep6, TRUE ,c(1:6)[!c(rep1, rep2, rep3, rep4, rep5, rep6)]))
		}   
)

#' handler for calculations BilanMigration
#' 
#'  internal use
#' @param h handler
#' @param ... additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
hbilanMigrationcalc=function(h,...){
	calcule( h$action)
}


#' calcule method for BilanMigration
#' @param object An object of class \code{\link{BilanMigration-class}}
#' @return BilanMigration with slots filled by user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("calcule",signature=signature("BilanMigration"),definition=function(object){ 
			bilanMigration<-object
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
				bilanMigration@time.sequence=seq.POSIXt(from=as.POSIXlt(min(data$debut_pas)),to=max(data$debut_pas),
						by=as.numeric(bilanMigration@pasDeTemps@stepDuration)) # il peut y avoir des lignes repetees poids effectif
				# traitement des coefficients de conversion poids effectif
				
				if (bilanMigration@taxons@data$tax_nom_latin=="Anguilla anguilla"& bilanMigration@stades@data$std_libelle=="civelle") 
				{
					tableau <-funtraitement_poids(tableau,time.sequence=bilanMigration@time.sequence)
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



#' handler hBilanMigrationgraph
#' calls the fungraph for BilanMigration and allows the saving of daily and monthly counts in the database
#' @note pb if other than daily value, the time steps have been constrained to daily values for this plot
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
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
	if (bilanMigration@pasDeTemps@stepDuration==86400 & bilanMigration@pasDeTemps@stepDuration==86400) {
		
		# pour sauvegarder sous excel
		if (taxon=="Anguilla anguilla"& stade=="civelle") {
			fungraph_civelle(bilanMigration=bilanMigration,bilanMigration@data,bilanMigration@time.sequence,taxon=taxon,stade=stade)
		}
		else {
			fungraph(bilanMigration=bilanMigration,tableau=bilanMigration@data,time.sequence=bilanMigration@time.sequence,taxon,stade)
		}
		
	} else {
		funout(get("msg",envir_stacomi)$BilanMigration.8)
		# normalement ce cas ne devrait plus se poser
	}	
	# ecriture du bilan journalier, ecrit aussi le bilan mensuel
	fn_EcritBilanJournalier(bilanMigration)
}

#' handler for calcul hBilanMigrationgraph2
#' 
#' Step plot over time
#' @param h handler
#' @param ... additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
hbilanMigrationgraph2 = function(h,...) {
	if (exists("bilanMigration",envir_stacomi)) {
		bilanMigration<-get("bilanMigration",envir_stacomi)
	} else {      
		funout(get("msg",envir_stacomi)$BilanMigration.5,arret=TRUE)
	}
	taxon= as.character(bilanMigration@taxons@data$tax_nom_latin)
	stade= as.character(bilanMigration@stades@data$std_libelle)
	DC=as.numeric(bilanMigration@dc@dc_selectionne)	
	if (bilanMigration@pasDeTemps@stepDuration==86400 & bilanMigration@pasDeTemps@stepDuration==86400) {
		bilanMigration@data$time.sequence=bilanMigration@time.sequence
		# pour sauvegarder sous excel
		bilanMigration@data<-funtraitementdate(bilanMigration@data,
				nom_coldt="time.sequence",
				annee=FALSE,
				mois=TRUE,
				quinzaine=TRUE,
				semaine=TRUE,
				jour_an=TRUE,
				jour_mois=FALSE,
				heure=FALSE)
		bilanMigration@data$Cumsum=cumsum(bilanMigration@data$Effectif_total)
		# pour sauvegarder sous excel
		annee=unique(strftime(as.POSIXlt(bilanMigration@time.sequence),"%Y"))
		dis_commentaire=  as.character(bilanMigration@dc@data$dis_commentaires[bilanMigration@dc@data$dc%in%bilanMigration@dc@dc_selectionne]) 
		update_geom_defaults("step", aes(size = 3))
		p<-ggplot(bilanMigration@data)+
				geom_step(aes(x=time.sequence,y=Cumsum,colour=mois))+
				ylab(get("msg",envir_stacomi)$BilanMigration.6)+
				ggtitle(paste(get("msg",envir_stacomi)$BilanMigration.7,dis_commentaire,", ",taxon,", ",stade,", ",annee,sep="")) + 
				theme(plot.title = element_text(size=10,colour="blue"))
		print(p)	
	} else {
		funout(get("msg",envir_stacomi)$BilanMigration.8)
	}
}

#' handler for summary function
#' calls functions funstat and funtable to build summary tables in html and
#' csv files
#' @param h Handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
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
	resum=funstat(tableau=bilanMigration@data,
			bilanMigration@time.sequence,
			taxon,
			stade,
			DC)
	funtable(tableau=bilanMigration@data,time.sequence=bilanMigration@time.sequence,taxon,stade,DC,resum)
}