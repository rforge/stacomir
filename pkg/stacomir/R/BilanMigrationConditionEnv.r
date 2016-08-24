# Nom fichier :        BilanMigrationConditionEnv    (classe)

#' Class "BilanMigrationConditionEnv"
#' 
#' Enables to compute an annual overview of fish migration and environmental
#' conditions in the same chart
#' 
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanMigrationConditionEnv",
#' bilanMigration=new("BilanMigration"),
#' bilanConditionEnv=new("BilanConditionEnv"))}.  \describe{
#' \item{list("bilanMigration")}{Object of class \code{"BilanMigration"} The
#' migration overview }\item{:}{Object of class \code{"BilanMigration"} The
#' migration overview } \item{list("bilanConditionEnv")}{Object of class
#' \code{"BilanConditionEnv"} The environmental overview}\item{:}{Object of
#' class \code{"BilanConditionEnv"} The environmental overview} }
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
setClass(Class="BilanMigrationConditionEnv",representation=
				representation(
						bilanMigration="BilanMigration",
						bilanConditionEnv="BilanConditionEnv"
				),
		prototype=prototype(
				bilanMigration=new("BilanMigration"),
				bilanConditionEnv=new("BilanConditionEnv")
		
		)
)


setValidity("BilanMigrationConditionEnv",
		function(object)
		{
			rep1=validObject(object@bilanMigration, test=TRUE)
			rep2=validObject(object@bilanConditionEnv, test=TRUE)
			rep3 = TRUE
			return(ifelse(rep1 & rep2 & rep3,TRUE,c(1:3)[!c(rep1, rep2, rep3)]))
		}   
)


#' handler du graphique BilanMigrationConditionEnv
#' realise le calcul du bilan migration avec CE, l'ecrit dans l'environnement envir_stacomi
#' traite eventuellement les quantites de lots (si c'est des civelles)
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
hbilanMigrationConditionEnvcalc=function(h,...){
	calcule(h$action)
	# calcule(bilanMigrationConditionEnv)
}
#object<-bilanMigrationConditionEnv
setMethod("calcule",signature=signature("BilanMigrationConditionEnv"),definition=function(object,...){ 
			# le chargement de bilanMigration utilise la methode calcule de BilanMigration
			# qui charge les objects et en plus fait un calcul dessus, e la fin cette methode assigne les objects
			# dans l'environnement stacomi et c'est le qu'il faut aller les chercher
			# pour eviter de lancer les calculs et d'avoir la demande de stations e la fin du bilan migration...
			if (!exists("refStationMesure",envir_stacomi)) {
				funout(get("msg",envir=envir_stacomi)$BilanCondtionEnv.2,arret=TRUE)
			}    
			calcule(object@bilanMigration)
			object@bilanMigration=get("bilanMigration",envir=envir_stacomi)
			# j'extraie les dates de debut et de fin de l'object pas de temps de l'object bilanmigration
			# il faut stocker un ojet RefHorodate dans l'environnement envir_stacomi pour reussir e le recharger dans l'object
			# bilanCOnditionEnv
			horodatedebut=new("RefHorodate")
			horodatedebut@horodate=object@bilanMigration@pasDeTemps@dateDebut  # format POSIXlt
			horodatefin=new("RefHorodate")
			horodatefin@horodate=DateFin(object@bilanMigration@pasDeTemps)    # format ePOSIXct
			# tiens c'est bizarre deux classes differents (POSIXlt et POSIXt) rentrent dans horodate
			# ben oui parce que RefHorodate est un object de classe POSIXT qui dans R est le papa des deux autres...
			horodatefin@horodate=as.POSIXlt(horodatefin@horodate) 
			# ces dates sont necessaire pour initialiser le bilanConditionEnv qui dans son interface
			# fournit d'une date de debut et d'une date de fin
			# normalement l'interface assigne les objects bilanConditionEnv_date_debut dans l'environnement env_stacomi
			# ces objects sont au format POSIXlt
			# ls(envir=envir_stacomi) 
			# Usage assign(x, value, pos = -1, envir = as.environment(pos),..)
			assign(x="bilanConditionEnv_date_debut",horodatedebut,envir=envir_stacomi)
			assign(x="bilanConditionEnv_date_fin",horodatefin,envir=envir_stacomi)
			object@bilanConditionEnv=charge(object@bilanConditionEnv) # le ea marche
			# les objects sont maintenant charges et calcules, j'assigne BilanConditionEnv qui les contient
			# dans l'environnement envir_stacomi
			funout(get("msg",envir=envir_stacomi)$BilanMigrationConditionEnv.1)
			assign("bilanMigrationConditionEnv",object,envir=envir_stacomi)
			enabled(toolbarlist[["Graph"]])<-TRUE
		})

#######################################################################
# handler du calcul BilanMigrationstat : traitements et sauvegardes
# appelle les fonctions funstat et funtable pour faire le bilan des migrations
# dans des fichiers csv
#######################################################################
hbilanMigrationstat = function(h,...) {
}

# graphiques (a affiner pb si autre chose que journalier)
#######################################################################
# handler du calcul hBilanMigrationgraph
# appelle les fonctions fungraph pour faire le bilan des migrations
#######################################################################
hbilanMigrationConditionEnvgraph = function(h,...) {   
	
	if (exists("bilanMigrationConditionEnv",envir_stacomi)) {
		bilanMigrationConditionEnv<-get("bilanMigrationConditionEnv",envir_stacomi)
	} else {
		funout(get("msg",envir=envir_stacomi)$BilanMigrationConditionEnv.2,arret=TRUE)
	} # end ifelse
	
	# dans le bilanMigration, la time.sequence est une sequence (pour l'instant bilanMigration seulement au format journalier)
	# c'est des dates en format POSIXct qui se decalent (changement d'heure)
	# je les formate au jour, il semble qu'il y ait parfois des decalages de 1 jour
	time.sequence<-as.Date(as.POSIXlt(bilanMigrationConditionEnv@bilanMigration@time.sequence,tz="GMT"))
	tableau<-bilanMigrationConditionEnv@bilanMigration@data
	tableau<-cbind("time.sequence"=time.sequence,tableau)
	tableau$time.sequencechar<-as.character(tableau$time.sequence)
	tableauCE<-bilanMigrationConditionEnv@bilanConditionEnv@data  # tableau conditions environnementales
	if (nrow(tableauCE)==0) {
		funout(get("msg",envir=envir_stacomi)$BilanMigrationConditionEnv.3,arret=TRUE)
	}
	
	stations<-bilanMigrationConditionEnv@bilanConditionEnv@stationMesure@data
	
	for (i in 1:length(unique(tableauCE$env_stm_identifiant))){
		tableauCE[unique(tableauCE$env_stm_identifiant)[i]==tableauCE$env_stm_identifiant,"stm_libelle"]<-
				stations[stations$stm_identifiant==unique(tableauCE$env_stm_identifiant)[i],"stm_libelle"]
	}
	
	# generation de donnees pour le graphe
	#tableauCE=data.frame("env_date_debut"=time.sequence, "env_stm_identifiant"="essai1","env_valeur_quantitatif"=rnorm(n=length(time.sequence),20,5))
	#tableauCE1=data.frame("env_date_debut"=time.sequence, "env_stm_identifiant"="essai2", "env_valeur_quantitatif"=sin((1:length(time.sequence))/50))
	#tableauCE=rbind(tableauCE,tableauCE1)
	tableauCE$env_date_debutchar=as.character(as.Date(tableauCE$env_date_debut))  
	
	if (nrow(stations)==0) { 
		funout(funout(get("msg",envir=envir_stacomi)$BilanMigrationConditionEnv.4))
		#assign(x="bilanCondition",bilanMigrationConditionEnv@bilanMigration,envir=envir_stacomi)
		hbilanMigrationgraph(h)   # lancement de la fonction normale
	}  else { 
		for (sta in as.character(stations$stm_libelle)){
			tableauCEst<-tableauCE[tableauCE$stm_libelle==sta,] #tableau CE d'une station
			if (length(unique(tableauCEst$env_date_debutchar))!=length(tableauCEst$env_date_debutchar)) {
				funout(paste(get("msg",envir=envir_stacomi)$BilanMigrationConditionEnv.7,
								sta,
								get("msg",envir=envir_stacomi)$BilanMigrationConditionEnv.8,
								paste(unique(tableauCEst$env_date_debutchar[duplicated(tableauCEst$env_date_debutchar)]),sep=""),
								get("msg",envir=envir_stacomi)$BilanMigrationConditionEnv.9),arret=FALSE)
				tableauCEst<-tableauCEst[induk(tableauCEst$env_date_debutchar),]
			}
			
			# ci dessous pas la meilleure facon de tester si la variable est quantitative ou qualitative mais je ne recupere pas le caractere de la
			# variable dans la table de jointure tj_conditionenvironnementale_env et il faudrait faire un requete supplementaire...	
			if (is.na(tableauCEst$env_val_identifiant[1])){
				#variable quantitative
				tableauCEst<-tableauCEst[,c("env_date_debutchar","env_valeur_quantitatif")]
				tableauCEst<-stacomirtools::chnames(tableauCEst,"env_valeur_quantitatif",sta)
				stations[stations$stm_libelle==sta,"stm_typevar"]<-"quantitatif"
				# je renomme la colonne e rentrer par le nom de la station
			}   else {
				# variable qualitative
				tableauCEst<-tableauCEst[,c("env_date_debutchar","env_val_identifiant")]
				tableauCEst$"env_val_identifiant"=as.factor(tableauCEst$"env_val_identifiant")
				tableauCEst<-stacomirtools::chnames(tableauCEst,"env_val_identifiant",sta)
				
				stations[stations$stm_libelle==sta,"stm_typevar"]<-"qualitatif"			
			} # end else
			# le merge ci dessous est l'equivalent d'une jointure gauche (LEFT JOIN)
			tableau<-merge(tableau,tableauCEst,by.x = "time.sequencechar", by.y = "env_date_debutchar",  all.x = TRUE)
			# les donnees sont normalement collees dans le tableau dans une nouvelle colonne et aux dates correspondantes
			if (length(time.sequence)!=nrow(tableau)) funout(paste(get("msg",envir=envir_stacomi)$BilanMigrationConditionEnv.5,
								nrow(tableau),
								get("msg",envir=envir_stacomi)$BilanMigrationConditionEnv.6,
								length(time.sequence),
								")\n"),arret=TRUE)
			#si la jointure e rajoute des lignes ea craint je ne sais pas comment se fera le traitement
		} # end for
		taxon= as.character(bilanMigrationConditionEnv@bilanMigration@taxons@data$tax_nom_latin)
		stade= as.character(bilanMigrationConditionEnv@bilanMigration@stades@data$std_libelle)
		fungraph_env(tableau,time.sequence,taxon,stade,stations)
	} # end else
}# end function

#######################################################################
# handler du calcul hBilanMigrationgraph2
# appelle les fonctions fungraph pour faire un graphe annuel des 
# cumuls de migration au cours du temps
#######################################################################

#hbilanMigrationConditionEnvgraph2 = function(h,...) {
#
#}
#
#hbilanMigrationConditionEnvstat = function(h,...) {
#
#}
