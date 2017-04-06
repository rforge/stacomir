#' Migration report along with quantitative and
#' qualitative characteristics
#' 
#' Migration along with qualitative or quantitative characteristics or both
#' (e.g.) weight of eels according to the size class per period of time, weight
#' of fish according to gender, number of fish per age class. This class does not split migration evenly over 
#' time period. So, unlike calculations made in class BilanMigration and BilanMigrationMult
#' the whole time span of the migration operation is not considered, only  the date of beginning of 
#' the operation is used to perform calculation. 
#' 
#' @include Refparquan.r
#' @include Refparqual.r
#' @include RefChoix.r
#' @note The program by default uses two parameter choice, checking box "none" will
#' allow the program to ignore the parameter
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanMigrationCar", ...)}.  they are loaded by the interface
#' using interface_BilanMigrationCar function.
#' @slot parquan An object of class \link{Refparquan-class}, quantitative parameter 
#' @slot parqual An object of class \link{Refparqual-class}, quanlitative parameter
#' @slot echantillon An object of class \link{RefChoix-class}, vector of choice
#' @slot valeurs_possibles A \code{data.frame} choice among possible choice of a qualitative parameter (discrete)
#' @slot dc an object of class \link{RefDC-class} inherited from \link{BilanMigration-class}
#' @slot taxons An object of class \link{RefTaxon-class} inherited from \link{BilanMigration-class}
#' @slot stades An object of class \link{RefStades-class} inherited from \link{BilanMigration-class}
#' @slot pasDeTemps An object of class \link{PasDeTempsJournalier-class} inherited from \link{BilanMigration-class}
#' @slot data A \code{data.frame} inherited from \link{BilanMigration-class}, stores the results
#' @slot time.sequence An object of class "POSIXct" inherited from \link{BilanMigration-class}
#' #' @family Bilan Objects
#' @aliases BilanMigrationMult bilanMigrationMult
#' @note program : default two parameter choice, checking box "aucun" will allow the program to ignore the parameter
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}

#' @concept Bilan Object 
#' @keywords classes
setClass(Class="BilanMigrationCar",
		representation=representation(
				echantillon="RefChoix",
				calcdata="list"),
		prototype=list(
				echantillon=new("RefChoix"),
				calcdata<-list()),
		contains="Bilan_carlot")


setValidity("BilanMigrationCar",function(object)
		{
			rep4=length(object@pasDeTemps)==1
			if (!rep4) retValue="length(object@pasDeTemps) different de 1, plusieurs stades alors que la classe n'en comporte qu'un" 
			rep5=length(object@parqual)==1|length(object@parquan)==1 #au moins un qualitatif ou un quantitatif
			if (!rep5) retValue="length(object@parqual)==1|length(object@parquan)==1 non respecte"  
			return(ifelse(rep4 & rep5,TRUE,retValue))
		}   )


#' command line interface for BilanMigrationCar class
#' @param object An object of class \link{BilanMigrationCar-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,RefDC-method}
#' @param taxons '2220=Salmo salar',
#' these should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,RefTaxon-method}
#' @param stades TODO
#' @param car Sample TODO
#' @param horodatedebut The starting date as a character, formats like \code{\%Y-\%m-\%d} or \code{\%d-\%m-\%Y} can be used as input
#' @param horodatefin The finishing date of the Bilan, for this class this will be used to calculate the number of daily steps.
#' @param echantillon Default TRUE, 
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @return An object of class \link{BilanAgedemer-class}
#' The choice_c method fills in the data slot for classes \link{RefDC-class}, \link{RefTaxon-class}, \link{RefStades-class}, \link{Refpar-class} and two slots of \link{RefHorodate-class} and then 
#' uses the choice_c methods of these object to select the data.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("choice_c",signature=signature("BilanMigrationCar"),definition=function(object,
				dc,
				taxons,
				stades,
				par,
				horodatedebut,
				horodatefin,
				echantillon=TRUE,
				silent=FALSE){
			# code for debug using example
			#horodatedebut="2012-01-01";horodatefin="2013-12-31";dc=c(107,108,101);taxons=2220;	stades=c('5','11','BEC','BER','IND');par=c('1786','1785','C001');silent=FALSE
			bmC<-object
			bmC@dc=charge(bmC@dc)
			bmC@dc<-choice_c(object=bmC@dc,dc)
			bmC@taxons<-charge_avec_filtre(object=bmC@taxons,bmC@dc@dc_selectionne)			
			bmC@taxons<-choice_c(bmC@taxons,taxons)
			bmC@stades<-charge_avec_filtre(object=bmC@stades,bmC@dc@dc_selectionne,bmC@taxons@data$tax_code)	
			bmC@stades<-choice_c(bmC@stades,stades,silent=silent)
			bmC@par<-charge_avec_filtre(object=bmC@par,bmC@dc@dc_selectionne,bmC@taxons@data$tax_code,bmC@stades@data$std_code)	
			bmC@par<-choice_c(bmC@par,par,silent=silent)
			bmC@horodatedebut<-choice_c(object=bmC@horodatedebut,
					nomassign="bmC_date_debut",
					funoutlabel=gettext("Beginning date has been chosen\n",domain="R-stacomiR"),
					horodate=horodatedebut, 
					silent=silent)
			bmC@horodatefin<-choice_c(bmC@horodatefin,
					nomassign="bmC_date_fin",
					funoutlabel=gettext("Ending date has been chosen\n",domain="R-stacomiR"),
					horodate=horodatefin,
					silent=silent)
			bmC@echantillon<-choice_c(bmC@echantillon,

			validObject(bmC)
			

			bilan_carlot@dc<-choice_c(object=bilan_carlot@dc,dc)
			# only taxa present in the bilanMigration are used
			bilan_carlot@taxons<-charge_avec_filtre(object=bilan_carlot@taxons,bilan_carlot@dc@dc_selectionne)			
			bilan_carlot@taxons<-choice_c(bilan_carlot@taxons,taxons)
			bilan_carlot@stades<-charge_avec_filtre(object=bilan_carlot@stades,bilan_carlot@dc@dc_selectionne,bilan_carlot@taxons@data$tax_code)	
			bilan_carlot@stades<-choice_c(bilan_carlot@stades,stades)
			bilan_carlot@par<-charge_avec_filtre(object=bilan_carlot@par,bilan_carlot@dc@dc_selectionne,bilan_carlot@taxons@data$tax_code,bilan_carlot@stades@data$std_code)	
			bilan_carlot@par<-choice_c(bilan_carlot@par,par,silent=silent)
			bilan_carlot@horodatedebut<-choice_c(object=bilan_carlot@horodatedebut,
					nomassign="bilan_carlot_date_debut",
					funoutlabel=gettext("Beginning date has been chosen\n",domain="R-stacomiR"),
					horodate=horodatedebut, 
					silent=silent)
			bilanFonctionnementDC@horodatefin<-choice_c(bilanFonctionnementDC@horodatefin,
					nomassign="bilan_carlot_date_fin",
					funoutlabel=gettext("Ending date has been chosen\n",domain="R-stacomiR"),
					horodate=horodatefin,
					silent=silent)
			return(bmC)
		})

setMethod("connect",signature=signature("BilanMigrationCar"),definition=function(object,silent=FALSE){
	if (bilanMigrationPar@parquan@data$par_nom=="aucune" & bilanMigrationPar@parqual@data$par_nom=="aucune") {
		stop("You need to choose at least one quantitative or qualitative attribute")
	} else if (bilanMigrationPar@parquan@data$par_nom=="aucune") {
		#caracteristique qualitative uniquement
		req@sql=paste("SELECT ope_date_debut, ope_date_fin, lot_methode_obtention, SUM(lot_effectif) AS effectif,",
				" car_val_identifiant_tous as car_val_identifiant",
				" FROM (SELECT *,", 
				" CASE when car_val_identifiant is not null then car_val_identifiant",
				" ELSE lot_pere_val_identifiant",
				" END as car_val_identifiant_tous", 
				" FROM ",get("sch",envir=envir_stacomi),"vue_ope_lot_ech_parqual", 
				" WHERE ope_dic_identifiant ='",bilanMigrationPar@dc@dc_selectionne,"'",
				echantillons,
				" AND lot_tax_code = '",bilanMigrationPar@taxons@data$tax_code,"'" ,
				" AND lot_std_code = '",bilanMigrationPar@stades@data$std_code,"'" ,
				" AND car_par_code = '",bilanMigrationPar@parqual@data$par_code,"'" ,
				" AND (ope_date_debut, ope_date_fin) OVERLAPS (TIMESTAMP '" , debutPas , "', TIMESTAMP '" , finPas , "')" ,
				" ) AS qan",
				" GROUP BY qan.ope_date_debut, qan.ope_date_fin, qan.lot_methode_obtention, qan.car_val_identifiant_tous " ,
				" ORDER BY qan.ope_date_debut",sep="")
	} else if (bilanMigrationPar@parqual@data$par_nom=="aucune") {
		# Caracteristique quantitative uniquement
		req@sql=paste("SELECT ope_date_debut, ope_date_fin, lot_methode_obtention, SUM(lot_effectif) AS effectif, SUM(car_valeur_quantitatif) AS quantite",
				" FROM ",get("sch",envir=envir_stacomi),"vue_ope_lot_ech_parquan",    
				" WHERE ope_dic_identifiant ='",bilanMigrationPar@dc@dc_selectionne,"'",
				echantillons,
				" AND lot_tax_code = '",bilanMigrationPar@taxons@data$tax_code,"'" ,
				" AND lot_std_code = '",bilanMigrationPar@stades@data$std_code,"'" ,
				" AND car_par_code = '",bilanMigrationPar@parquan@data$par_code,"'" ,
				" AND (ope_date_debut, ope_date_fin) OVERLAPS (TIMESTAMP '" , debutPas , "', TIMESTAMP '" , finPas , "')" ,
				" GROUP BY ope_date_debut, ope_date_fin, lot_methode_obtention" ,
				" ORDER BY ope_date_debut",sep="")
	} else {
		#les deux caracteristiques sont choisies, il faut faire un Bilancroise
		# attention je choisis un left  join ea veut dire certaines caracteristiques quant n'ont pas de contrepartie quantitative     
		req@sql=paste(
				" SELECT ope_date_debut,",
				" ope_date_fin,",  
				" SUM(lot_effectif) AS effectif,", 
				" SUM(car_valeur_quantitatif) AS quantite,",
				" car_val_identifiant_tous as car_val_identifiant",
				" FROM (",
				" SELECT *,",
				" CASE when car_val_identifiant is not null then car_val_identifiant",
				" ELSE lot_pere_val_identifiant",
				" END as car_val_identifiant_tous",
				" FROM (",
				" SELECT * FROM ",get("sch",envir=envir_stacomi),"vue_ope_lot_ech_parquan", 
				" WHERE ope_dic_identifiant ='",bilanMigrationPar@dc@dc_selectionne,"'",
				echantillons,
				" AND lot_tax_code = '",bilanMigrationPar@taxons@data$tax_code,"'" ,
				" AND lot_std_code = '",bilanMigrationPar@stades@data$std_code,"'" ,
				" AND car_par_code = '",bilanMigrationPar@parquan@data$par_code,"'" ,
				" AND (ope_date_debut, ope_date_fin) OVERLAPS (TIMESTAMP '",debutPas,"',TIMESTAMP '",finPas,"') " ,
				" ) AS qan",
				" LEFT JOIN", 
				" (SELECT lot_identifiant as lot_identifiant1,car_val_identifiant ",
				"  FROM vue_ope_lot_ech_parqual ", 
				" WHERE ope_dic_identifiant ='",bilanMigrationPar@dc@dc_selectionne,"'",
				echantillons,
				" AND lot_tax_code = '",bilanMigrationPar@taxons@data$tax_code,"'" ,     
				" AND lot_std_code = '",bilanMigrationPar@stades@data$std_code,"'" ,
				" AND car_par_code = '",bilanMigrationPar@parqual@data$par_code,"'" ,
				" AND (ope_date_debut, ope_date_fin) OVERLAPS (TIMESTAMP '",debutPas,"',TIMESTAMP '",finPas,"') " ,
				" )as qal ",
				" ON qan.lot_identifiant=qal.lot_identifiant1",
				" )as qanqal",
				" GROUP BY  qanqal.ope_date_debut, qanqal.ope_date_fin, qanqal.car_val_identifiant_tous",
				" ORDER BY qanqal.ope_date_debut",sep="")
	}
		
	})# end else
	
	#cat(paste("Requete SQL : \n" , sql))
	rs<-stacomirtools::connect(req)@query
	#cat(nrow(rs))
	if (nrow(rs)>0){
		
		debutOpe=as.POSIXlt(rs$ope_date_debut)
		finOpe= as.POSIXlt(rs$ope_date_fin)
		effectif=rs$effectif
		quantite=rs$quantite
		if (bilanMigrationPar@parqual@data$par_nom!="aucune") {
			rs$car_val_identifiant[is.na(rs$car_val_identifiant)]<-"autre"
		}
		# creation des sommes effectif_MESURE ...
		
		# Si l'operation commence avant le pas de temps courant, et ne se termine pas apres, il faut conserver une seule partie de l'operation
		# Si l'operation se termine apres la fin du pas mais ne debute pas avant, il faut conserver une seule partie de l'operation
		# Si l'operation commence avant le pas de temps et se termine apres, on ne conserve qu'une partie de l'operation
		# Cas ou l'operation est inferieure ou egale au pas de temps : pas de probleme, on compte l'operation complete
		# ce qui revient e dire que pour ce qui concerne la time.sequence de l'operation effectif sur le pas de temps
		# on prends le max du debut de ope et pas de temps (si l'ope commence avant on garde pas cette partie )
		# et pour la fin on prend le min si l'ope se termine apres on garde pas... ouf
		
		debut<-debutOpe
		fin<-finOpe
		debut[debut<debutPas]<-debutPas
		fin[fin>finPas]<-finPas
		
		# Repartition de l'effectif au prorata
		effectif = effectif *  as.double(difftime(time1=fin, time2=debut,units =  "secs"))/as.double(difftime(time1=finOpe,time2=debutOpe,units =  "secs")) 
		quantite=  quantite *  as.double(difftime(time1=fin, time2=debut,units =  "secs"))/as.double(difftime(time1=finOpe,time2=debutOpe,units =  "secs")) 
		if (bilanMigrationPar@parqual@data$par_nom!="aucune") { # il existe des caracteristiques qualitatives de lot			
			# i=c(valeurs_qal,"tous")[2]
			for (i in valeurs_qal){
				assign(eval(paste("effectif_",i,sep="")),sum(effectif[rs$car_val_identifiant==i]))
				assign(eval(paste("quantite_",i,sep="")),sum(quantite[rs$car_val_identifiant==i]))
			}
		} else {# pas de caracteristiques qualitatives de lot et pas de decoupage supplementaire
			effectif<-sum(effectif)
			quantite<-sum(quantite)
		}
	} else {
		# dans le cas ou le resultat de la requete est vide pas de ligne je met 0
		if (bilanMigrationPar@parqual@data$par_nom!="aucune") { # il existe des caracteristiques qualitatives de lot			
			for (i in valeurs_qal){
				assign(eval(paste("effectif_",i,sep="")),0)
				assign(eval(paste("quantite_",i,sep="")),0)
			}
		} else {# pas de caracteristiques qualitatives de lot et pas de decoupage supplementaire
			effectif<-0
			quantite<-0
		}
		
	}

#' charge method for BilanMigrationCar
#' 
#' Used by the graphical interface to collect and test objects in the environment envir_stacomi, 
#' fills also the data slot by the connect method
#' @param object An object of class \link{BilanMigrationMult-class}
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @return \link{BilanMigrationCar-class} with slots filled by user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("charge",signature=signature("BilanMigrationMult"),definition=function(object,silent=FALSE){ 
			bmC<-object  
			if (exists("refDC",envir_stacomi)) {
				bmC@dc<-get("refDC",envir_stacomi)
			} else {
				funout(gettext("You need to choose a counting device, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("refTaxon",envir_stacomi)) {
				bmC@taxons<-get("refTaxon",envir_stacomi)
			} else {      
				funout(gettext("You need to choose a taxa, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("refStades",envir_stacomi)){
				bmC@stades<-get("refStades",envir_stacomi)
			} else 
			{
				funout(gettext("You need to choose a stage, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("pasDeTemps",envir_stacomi)){
				bmC@pasDeTemps<-get("pasDeTemps",envir_stacomi)
				# pour permettre le fonctionnement de Fonctionnement DC
				assign("bilanFonctionnementDC_date_debut",get("pasDeTemps",envir_stacomi)@"dateDebut",envir_stacomi)
				assign("bilanFonctionnementDC_date_fin",as.POSIXlt(DateFin(get("pasDeTemps",envir_stacomi))),envir_stacomi)
			} else {
				funout(gettext("Attention, no time step selected, compunting with default value\n",domain="R-stacomiR"),arret=FALSE)
				warning("Attention, no time step selected, compunting with default value\n")
			}
			if (exists("refchoice",envir_stacomi)){
				bmC@echantillon<-get("refchoice",envir_stacomi)
			} else 
			{
				bmC@echantillon@listechoice<-"avec"
				bmC@echantillon@selected<-as.integer(1)
			}
			if (exists("refparquan",envir_stacomi)){
				bmC@parquan<-get("refparquan",envir_stacomi)
			} else 
			{
				funout(gettext("You need to choose a quantitative parameter\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("refparqual",envir_stacomi)){
				bmC@parqual<-get("refparqual",envir_stacomi)
			} else 
			{
				funout(gettext("You need to choose a qualitative parameter\n",domain="R-stacomiR"),arret=TRUE)
			}
			
			stopifnot(validObject(bmC, test=TRUE))
			funout(gettext("Attention, no time step selected, compunting with default value\n",domain="R-stacomiR"))
			
		})
		
#' handler for bilanmigrationpar
#' @param h handler
#' @param ... Additional parameters
		hbmCcalc=function(h,...){
			calcule(h$action)
		}			
		
		
#' calcule methode
#' 
#' 
#'@param object An object of class \code{\link{BilanMigrationCar-class}} 
setMethod("calcule",signature=signature("BilanMigrationCar"),definition=function(object){ 
			bmC<-object
		if (bmC@parquan@data$par_nom=="aucune" & bmC@parqual@data$par_nom=="aucune") {
				funout(gettext("You need to choose at least one quantitative or qualitative attribute\n",domain="R-stacomiR"),arret=TRUE)}
			res<-funSousListeBilanMigrationCar(bmC=bmC)
			if (exists("progres")) close(progres)
			data<-res[[1]]
			data[,"debut_pas"]<-as.POSIXct(strptime(x=data[,"debut_pas"],format="%Y-%m-%d"))   # je repasse de caractere 
			data[,"fin_pas"]<-as.POSIXct(strptime(data[,"fin_pas"],format="%Y-%m-%d"))
			bmC@valeurs_possibles<-res[[2]]   # definitions des niveaux de parametres qualitatifs rencontres.
			# funout("\n")
			#	assign("data",data,envir_stacomi)
			#funout(gettext("the migration summary table is stored in envir_stacomi\n",domain="R-stacomiR"))
			#data<-get("data",envir_stacomi)
			# chargement des donnees suivant le format chargement_donnees1  
			bmC@time.sequence=seq.POSIXt(from=min(data$debut_pas),to=max(data$debut_pas),by=as.numeric(bmC@pasDeTemps@stepDuration)) # il peut y avoir des lignes repetees poids effectif
			
			if (bmC@taxons@data$tax_nom_commun=="Anguilla anguilla"& bmC@stades@data$std_libelle=="civelle") 
			{
				funout(gettext("Be careful, the processing doesnt take lot\"s quantities into account \n",domain="R-stacomiR"))
			}
			funout(gettext("Writing data into envir_stacomi environment : write data=get(\"data\",envir_stacomi) \n",domain="R-stacomiR"))
			bmC@data<-data 
			###########################
			bmC<-x # ne pas passer dessus en debug manuel
			##########################
			colnames(bmC@data)<-gsub("debut_pas","Date",colnames(bmC@data))
			if (bmC@parqual@data$par_nom!="aucune"& bmC@parquan@data$par_nom!="aucune") {# il y a des qualites et des quantites de lots
				nmvarqan=gsub(" ","_",bmC@parquan@data$par_nom) # nom variable quantitative
				colnames(bmC@data)<-gsub("quantite",nmvarqan,colnames(bmC@data))
				mb=reshape2::melt(bmC@data,id.vars=c(1:4),measure.vars=grep(nmvarqan,colnames(bmC@data)))
				# ici je ne sors que les variables quantitatives pour les graphes ulterieurs (j'ignore les effectifs) 
			} else if (bmC@parqual@data$par_nom!="aucune"){ # c'est que des caracteristiques qualitatives
				mb=reshape2::melt(bmC@data,id.vars=c(1:4),measure.vars=grep("effectif",colnames(bmC@data)))  # effectifs en fonction des variables qualitatives, il n'y a qu'une seule colonne     
			} else if (bmC@parquan@data$par_nom!="aucune"){ # c'est que des caracteristiques quantitatives
				nmvarqan=gsub(" ","_",bmC@parquan@data$par_nom) # nom variable quantitative
				colnames(bmC@data)<-gsub("quantite",nmvarqan,colnames(bmC@data)) # je renomme la variable quant
				mb=reshape2::melt(bmC@data,id.vars=c(1:4),measure.vars=grep(nmvarqan,colnames(bmC@data))) # valeurs quantitatives (il n'y a qu'une) 
			} else if (bmC@parquan@data$par_nom=="aucune"&bmC@parqual@data$par_nom=="aucune"){
				stop("This shouldn't be possible")
				# ce cas est impossible
			}
			mb=stacomirtools::chnames(mb,"value","sum")
			mb=funtraitementdate(data=mb,nom_coldt="Date") 
			assign("bmC",bmC,envir_stacomi)
			assign("data",data,envir_stacomi)
			# graphiques (a affiner pb si autre chose que journalier)
			# pour sauvegarder sous excel
		})
#' le handler appelle la methode generique graphe sur l'object plot.type=1
#' 
#' @param h handler
#' @param ... Additional parameters
hbmCgraph = function(h,...) {
	if (exists("bmC",envir_stacomi)) {
		bmC<-get("bmC",envir_stacomi)
		plot(bmC,plot.type="barplot")
	} else {      
		funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
	}
}
#' le handler appelle la methode generique graphe sur l'object plot.type=2
#' 
#' @param h handler
#' @param ... Additional parameters
hbmCgraph2=function(h,...){
	if (exists("bmC",envir_stacomi)) {
		bmC<-get("bmC",envir_stacomi)
		plot(bmC,plot.type="xyplot")
	} else {      
		funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
	}
}
#' This handler calls the generic method graphe on object plot.type 3
#' 
#' 
#' @param h handler
#' @param ... Additional parameters
hbmCstat=function(h){
	if (exists("bmC",envir_stacomi)) {
		bmC<-get("bmC",envir_stacomi)
		plot(bmC,plot.type="summary")
	} else {      
		funout(gettext("You need to launch computation first, clic on calc\n",arret=TRUE)		)
	}
}

#' plot method for BilanMigrationCar
#' 
#' 
#' @param x An object of class BilanMigrationCar
#' @param y not used there
#' @param plot.type One of "barplot", "xyplot", "summary table
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("plot",signature=signature(x="BilanMigrationCar",y="ANY"),definition=function(x,y,plot.type="barplot",...){ 
		
			# transformation du tableau de donnees
			
			if (plot.type=="barplot") {
				
				g<-ggplot(mb)
				g<-g+geom_bar(aes(x=mois,y=sum,fill=variable),stat='identity',
						stack=TRUE)
				assign("g",g,envir_stacomi)
				funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi) \n",domain="R-stacomiR"))
				print(g)
			} #end plot.type = "barplot"
			if (plot.type=="xyplot") { 
				
				g<-ggplot(mb)
				g<-g+geom_point(aes(x=Date,y=sum,col=variable),stat='identity',stack=TRUE)
				assign("g",g,envir_stacomi)
				funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi) \n",domain="R-stacomiR"))
				print(g)
			} #end plot.type="xyplot"
			#TODO create summary method
			
		})


#' summary for BilanMigrationCar 
#' @param object An object of class \code{\link{BilanMigrationCar-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("summary",signature=signature(object="BilanMigrationCar"),definition=function(object,silent=FALSE,...){
					
		if (plot.type=="summary") {
			table=round(tapply(mb$sum,list(mb$mois,mb$variable),sum),1)
			table=as.data.frame(table)
			table[,"total"]<-rowSums(table)
			gdf(table, container=TRUE)
			nomdc=bmC@dc@data$df_code[match(bmC@dc@dc_selectionne,bmC@dc@data$dc)]
			annee=unique(strftime(as.POSIXlt(bmC@time.sequence),"%Y"))
			path1=file.path(path.expand(get("datawd",envir=envir_stacomi)),paste(nmvarqan,"_mensuel_",nomdc,"_",bmC@taxons@data$tax_nom_commun,"_",bmC@stades@data$std_libelle,"_",annee,".csv",sep=""),fsep ="\\")
			write.table(table,file=path1,row.names=FALSE,col.names=TRUE,sep=";")
			funout(gettextf("Writing of %s",path1))
			path1=file.path(path.expand(get("datawd",envir=envir_stacomi)),paste(nmvarqan,"_journalier_",nomdc,"_",bmC@taxons@data$tax_nom_commun,"_",bmC@stades@data$std_libelle,"_",annee,".csv",sep=""),fsep ="\\")
			write.table(bmC@data,file=path1,row.names=FALSE,col.names=TRUE,sep=";")
			funout(gettextf("Writing of %s",path1))
		} # end plot.type summary 
	})