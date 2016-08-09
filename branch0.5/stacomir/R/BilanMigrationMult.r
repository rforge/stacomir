#' class BilanMigrationMult 
#' 
#' Migration counts for several Fish counting devices (DC), several taxa and several stages.
#' This migration count can be built either by the graphical interface or in command line
#' \code{new("BilanMigrationMult",
#' dc=new("RefDC"),taxons=("RefTaxon"),stades=new("RefStades"),pasDeTemps=new("PasDeTempsJournalier"),data=data.frame(),calcdata=list(),
#' coef_conversion=data.frame()}.  
#' @note This class makes different calculations than BilanMigration, it does not handle escapement coefficients,
#' it uses quantities other than numbers if necessary (only filled in for glass eel in the connect method)
#' @name BilanMigrationMult-class
#' @aliases BilanMigrationMult BilanMigrationMult-class
#' @slot dc="RefDC" an objet of class RefDC
#' @slot taxons="RefTaxon"
#' @slot stades="RefStades"
#' @slot pasDeTemps="PasDeTempsJournalier"
#' @slot data="data.frame"
#' @slot calcdata="list" of calculated daily data, one per dc
#' @slot coef_conversion="data.frame"
#' @examples
#' 
#' showClass("BilanMigrationMult")
#' bilanMigration= new("BilanMigrationMult")
#' @export
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
setClass(Class="BilanMigrationMult",
		representation=
				representation(dc="RefDC",
						taxons="RefTaxon",
						stades="RefStades",
						pasDeTemps="PasDeTempsJournalier",
						data="data.frame",
						calcdata="list",
						coef_conversion="data.frame",
						time.sequence="POSIXct"),
		prototype=prototype(dc=new("RefDC"),
				taxons=new("RefTaxon"),
				stades=new("RefStades"),
				pasDeTemps=new("PasDeTempsJournalier"),
				data=data.frame(),
				calcdata=list(),
				coef_conversion=data.frame(),
				time.sequence=as.POSIXct(Sys.time())
		) 
)

#' validity check for class BilanMigrationMult
#' @describeIn BilanMigrationMult
setValidity("BilanMigrationMult",function(object)
		{
			rep1=length(object@dc)>=1
			rep2=length(object@taxons)>=1
			rep3=length(object@stades)>=1
			#	rep3=length(object@pasDeTemps)==1
			#rep4=(object@pasDeTemps@nbPas==365) # contrainte : pendant 365j
			#	rep5=as.numeric(strftime(object@pasDeTemps@dateDebut,'%d'))==1 # contrainte : depart = 1er janvier
			#	rep6=as.numeric(strftime(object@pasDeTemps@dateDebut,'%m'))==1			
			return(ifelse(rep1 & rep2 & rep3 , TRUE ,c(1:6)[!c(rep1, rep2, rep3)]))
		}   
)

#' initialize method for BilanMigrationMult, allows a more elaborate constuctor than new
#' @describeIn BilanMigrationMult
setMethod("initialize", "BilanMigrationMult", function(.Object, ...) {
			# callNextMethod() calls the method first inherited method, ie the
			# method that would have been called if the current method did not exist
			# here it calls the default constructor of the class (initialize as it would
			# have worked for new()
			.Object <- callNextMethod()
			.Object@taxons=charge(.Object@taxons)
			.Object@stades=charge(.Object@stades)
			.Object@dc=charge(.Object@dc)   
			fonctionnementDC=new("BilanFonctionnementDC")
			assign("fonctionnementDC",fonctionnementDC,envir = envir_stacomi)
			.Object
		})



#' charge method for BilanMigrationMult
#' @return BilanMigrationMult with slots filled by user choice
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @family charge methods
#' @describeIn BilanMigrationMult
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


#' command line interface for BilanMigrationMult class
#' The load method fills in the data slot for RefDC, RefTaxon, RefStades and then 
#' uses the load methods of these object to "select" the data to be used later.
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @describeIn BilanMigrationMult
#' @family load functions
#' @export
setMethod("load",signature=signature("BilanMigrationMult"),definition=function(objet,dc,taxons,stades,datedebut,datefin,...){
			bilanMigrationMult<-objet
			fonctionnementDC=new("BilanFonctionnementDC")
			# appel ici pour pouvoir utiliser les fonctions graphiques associees sur fonctionnement du DC
			assign("fonctionnementDC",fonctionnementDC,envir = envir_stacomi)
			bilanMigrationMult@dc=charge(bilanMigrationMult@dc)
			# loads and verifies the dc
			bilanMigrationMult@dc<-load(objet=bilanMigrationMult@dc,dc)
			# only taxa present in the bilanMigration are used
			bilanMigrationMult@taxons<-charge_avec_filtre(objet=bilanMigrationMult@taxons,bilanMigrationMult@dc@dc_selectionne)			
			bilanMigrationMult@taxons<-load(bilanMigrationMult@taxons,taxons)
			bilanMigrationMult@stades<-charge_avec_filtre(objet=bilanMigrationMult@stades,bilanMigrationMult@dc@dc_selectionne,bilanMigrationMult@taxons@data$tax_code)	
			bilanMigrationMult@stades<-load(bilanMigrationMult@stades,stades)
			bilanMigrationMult@pasDeTemps<-load(bilanMigrationMult@pasDeTemps,datedebut,datefin)
			assign("bilanMigrationMult",bilanMigrationMult,envir = envir_stacomi)
			return(bilanMigrationMult)
		})

#' calcule method for BilanMigrationMult, does the calculation once data are filled 
#' in with the connect method
#' @param negative a boolean indicating if a separate sum must be done for positive and negative values, if true, positive and negative counts return 
#' different rows
#' @note The class BilanMigrationMult does not handle  escapement rates. Use class BilanMigration if you want to handle them. The class does not handler
#' 'devenir' i.e. the destination of the fishes.
#' @return BilanMigrationMult with slots filled by user choice
#' @exportMethod 
#' @family calcule methods
#' @describeIn BilanMigrationMult
setMethod("calcule",signature=signature("BilanMigrationMult"),definition=function(objet,negative=FALSE,...){ 
			
			bilanMigrationMult<-objet
			bilanMigrationMult=connect(bilanMigrationMult)
			if (nrow(bilanMigrationMult@data)==0) {
				funout("Message temporaire, Aucune donnée")
			}
			bilanMigrationMult@data$duree=difftime(bilanMigrationMult@data$ope_date_fin,bilanMigrationMult@data$ope_date_debut,unit="days")
			debut=bilanMigrationMult@pasDeTemps@dateDebut
			fin=DateFin(bilanMigrationMult@pasDeTemps)
			time.sequence<-seq.POSIXt(from=debut,to=fin,
					by=as.numeric(bilanMigrationMult@pasDeTemps@dureePas))
			bilanMigrationMult@time.sequence<-time.sequence
			lestableaux<-list()
			for (dic in unique(bilanMigrationMult@data$ope_dic_identifiant))	{
				datasub<-bilanMigrationMult@data[bilanMigrationMult@data$ope_dic_identifiant==dic,]
				
				if (any(datasub$duree>(bilanMigrationMult@pasDeTemps@dureePas/86400))){				
					#----------------------
					# bilans avec overlaps
					#----------------------
					data<-fun_bilanMigrationMult_Overlaps(time.sequence = time.sequence, datasub = datasub,negative=negative)
					# pour compatibilité avec les bilanMigration
					data$taux_d_echappement=-1					
					lestableaux[[str_c("dc_",dic)]][["data"]]<-data
					lestableaux[[str_c("dc_",dic)]][["method"]]<-"overlaps"
					contient_poids<-"poids"%in%datasub$type_de_quantite
					lestableaux[[str_c("dc_",dic)]][["contient_poids"]]<-contient_poids
					if (contient_poids){
						coe<-bilanMigrationMult@coef_conversion[,c("coe_date_debut","coe_valeur_coefficient")]
						data$coe_date_debut<-as.Date(data$debut_pas)
						data<-merge(data,coe,by="coe_date_debut")
						data<-data[,-1] # removing coe_date_debut
						data <-fun_weight_conversion(tableau=data,duree=bilanMigrationMult@time.sequence)
					}
					
					lestableaux[[str_c("dc_",dic)]][["data"]]<-data
					
				} else {
					#----------------------
					#bilan simple
					#----------------------
					data<-fun_bilanMigrationMult(time.sequence = time.sequence,datasub=datasub,negative=negative)
					data$taux_d_echappement=-1
					data$coe_valeur_coefficient=NA
					lestableaux[[str_c("dc_",dic)]][["data"]]<-data
					lestableaux[[str_c("dc_",dic)]][["method"]]<-"sum"
					lestableaux[[str_c("dc_",dic)]][["contient_poids"]]<-contient_poids
				}
			}	# end for dic
			# TODO developper une méthode pour sumneg 
			bilanMigrationMult@calcdata<-lestableaux
			assign("bilanMigrationMult",bilanMigrationMult,envir_stacomi)
			funout(get("msg",envir_stacomi)$BilanMigrationMult.3)
			funout(get("msg",envir_stacomi)$BilanMigrationMult.4)
			return(bilanMigrationMult)
		})

#' connect method for BilanMigrationMult,
#' a single query collects data from the database
#' @return BilanMigrationMult with slot @data filled from the database
#' @exportMethod 
#' @family connect methods
#' @describeIn BilanMigrationMult
setMethod("connect",signature=signature("BilanMigrationMult"),definition=function(objet,...){ 
			# récuperation du BilanMigration
			bilanMigrationMult<-objet
			# retrieve the argument of the function and passes it to bilanMigrationMult
			# easier to debug
			req=new("RequeteODBCwheredate")
			req@baseODBC<-get("baseODBC", envir=envir_stacomi)			
			req@colonnedebut<-"ope_date_debut"
			req@colonnefin<-"ope_date_fin"
			req@datedebut=as.POSIXlt(bilanMigrationMult@pasDeTemps@dateDebut)
			req@datefin=as.POSIXlt(DateFin(bilanMigrationMult@pasDeTemps))
			dc = vector_to_listsql(bilanMigrationMult@dc@dc_selectionne)
			tax=vector_to_listsql(bilanMigrationMult@taxons@data$tax_code)
			std=vector_to_listsql(bilanMigrationMult@stades@data$std_code)
			sch=get("sch",envir=envir_stacomi)
			req@select = str_c("SELECT 
							ope_identifiant,
							lot_identifiant,
							ope_date_debut,
							ope_date_fin,
							ope_dic_identifiant,
							lot_tax_code,
							lot_std_code,
							CASE WHEN lot_effectif is not NULL then lot_effectif  
							WHEN lot_effectif is null then lot_quantite 
							end as value,
							case when lot_effectif is not NULL  then 'effectif' 
							when lot_effectif is null and lot_qte_code='1' then 'poids' 
							when lot_effectif is null and lot_qte_code='2' then 'volume' 
							else 'quantite' end as type_de_quantite,
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
			bilanMigrationMult@data=req@query	
			
			# récuperation des coefficients si il y a des civelles dans le bilan
			if (2038%in%bilanMigrationMult@taxons@data$tax_code&'CIV'%in%bilanMigrationMult@stades@data$std_code){
				req=new("RequeteODBCwheredate")
				req@baseODBC<-get("baseODBC",envir=envir_stacomi)
				req@select="select * from tj_coefficientconversion_coe"
				req@datedebut=as.POSIXlt(bilanMigrationMult@pasDeTemps@dateDebut)
				req@datefin=as.POSIXlt(DateFin(bilanMigrationMult@pasDeTemps))
				req@colonnedebut<-"coe_date_debut"
				req@colonnefin<-"coe_date_fin"
				req@and<-c("and coe_tax_code='2038'","and coe_std_code='CIV'")
				req@order_by<-"order by coe_date_debut"
				req<-connect(req)
				bilanMigrationMult@coef_conversion<-req@query
				
			}
			
			return(bilanMigrationMult)
			
			
		})				

#' handler du calcul hBilanMigrationgraph
#' appelle les fonctions fungraph pour faire le bilan des migrations
#' et permet la sauvegarde des bilans journaliers dans la base
#' @note pb si autre chose que journalier les pas de temps ont été contraints à des pas de temps journaliers pour ce graphique
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @seealso BilanMigrationMult

hbilanMigrationMultgraph = function(h=null,...) {
	if (exists("bilanMigrationMult",envir_stacomi)) {
		bilanMigration<-get("bilanMigrationMult",envir_stacomi)
	} else {      
		funout(get("msg",envir_stacomi)$BilanMigration.5,arret=TRUE)
	}
	lestaxons= bilanMigrationMult@taxons@data
	lesstades= bilanMigrationMult@stades@data
	lesdc=as.numeric(bilanMigrationMult@dc@dc_selectionne)	
	funout(get("msg",envir_stacomi)$BilanMigration.9)
	#&&&&&&&&&&&&&&&&&&&&&&&&&debut de boucle&&&&&&&&&&&&&&&&&&&&&&&&&&&
	for (dcnum in 1:length(lesdc)){
		for (taxonnum in 1:nrow(lestaxons)){
			for (stadenum in 1:nrow(lesstades)){
		
				taxon=lestaxons[taxonnum,"tax_nom_latin"]
				stade=lesstades[stadenum,"std_libelle"]
				dc=lesdc[dcnum]
							
				# préparation du jeu de données pour la fonction fungraph_civ
				#developpée pour la classe BilanMigration
				data<-bilanMigrationMult@calcdata[[str_c("dc_",dc)]][["data"]]
				data<-data[data$lot_tax_code==lestaxons[taxonnum,"tax_code"] &
								data$lot_std_code==lesstades[stadenum,"std_code"],]
				
				if (nrow(data)>0){
				
					funout(paste("dc=",dc,
									taxon,
									stade))	
					if (any(duplicated(data$No.pas))) stop("duplicated values in No.pas")
					data_without_hole<-merge(
							data.frame(No.pas=as.numeric(strftime(bilanMigrationMult@time.sequence,format="%j"))-1,
									debut_pas=bilanMigrationMult@time.sequence),
							data,
							by=c("No.pas","debut_pas"),
							all.x=TRUE
					)
					data_without_hole$CALCULE[is.na(data_without_hole$CALCULE)]<-0
					data_without_hole$MESURE[is.na(data_without_hole$MESURE)]<-0
					data_without_hole$EXPERT[is.na(data_without_hole$EXPERT)]<-0
					data_without_hole$PONCTUEL[is.na(data_without_hole$PONCTUEL)]<-0
					if (bilanMigrationMult@calcdata[[str_c("dc_",dc)]][["contient_poids"]]&
							taxon=="Anguilla anguilla"&
							stade=="civelle") {
					#----------------------------------
						# bilan migration avec poids (civelles
					#-----------------------------------------
						x11()
						fungraph_civelle(bilanMigration=bilanMigrationMult,
								table=data_without_hole,
								duree=bilanMigrationMult@time.sequence,
								taxon=taxon,
								stade=stade,
								dc=dc)
					}	else {
						#----------------------------------
						# bilan migration standard
						#-----------------------------------------
						x11()
						fungraph(bilanMigration=bilanMigrationMult,
								tableau=data_without_hole,
								duree=bilanMigrationMult@time.sequence,
								taxon,
								stade,
								dc)
					}
				} # end nrow(data)>0		
				# ecriture du bilan journalier, ecrit aussi le bilan mensuel
				#fn_EcritBilanJournalier(bilanMigrationMult)
				
			}
		}
	}
	#&&&&&&&&&&&&&&&&&&&&&&&&&fin de boucle&&&&&&&&&&&&&&&&&&&&&&&&&&&
	
}
#' handler du graphique BilanMigrationMult
#' realise le calcul du bilan migration, l'ecrit dans l'environnement envir_stacomi
#' traite eventuellement les quantites de lots (si c'est des civelles)
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @seealso BilanMigrationMult
hbilanMigrationMultcalc=function(h=null,...){
	bilanMigrationMult<-get("bilanMigrationMult",envir=envir_stacomi)
	bilanMigrationMult<-charge(bilanMigrationMult)
	bilanMigrationMult<-connect(bilanMigrationMult)
	bilanMigrationMult<-calcule(bilanMigrationMult)
}

#' handler du calcul hBilanMigrationgraph2
#' appelle les fonctions fungraph pour faire un graphe annuel des 
#' cumuls de migration au cours du temps
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
hbilanMigrationMultgraph2 = function(h=null,...) {
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
hTableBilanMigrationMult=function(h=null,...) {
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

#' handler to print the command line
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}

houtBilanMigrationMult=function(h=null,...) {
	if (exists("refStades",envir_stacomi)) 	{
		bilanMigrationMult<-get("bilanMigrationMult",envir_stacomi)
		out(bilanMigrationMult)
	} 
	else 
	{      
		funout(get("msg",envir_stacomi)$BilanMigrationMult.2,arret=TRUE)
	}
}

#' Method to print the command line of the object
#' @name out
#' @alias out,-method
#' 
#' @returnType 
#' @return NULL
#' 
#' @author cedric.briand
#' @docType methods
#' @export
setMethod("out",signature=signature("BilanMigrationMult"),definition=function(objet,...){ 
			sortie1<-"bilanMigrationMult=new(bilanMigrationMult)\n"
			sortie2<-str_c("bilanMigrationMult=load(bilanMigrationMult,",
					"dc=c(",str_c(objet@dc@dc_selectionne,collapse=","),"),",
					"taxons=c(",str_c(shQuote(objet@taxons@data$tax_nom_latin),collapse=","),"),",
					"stades=c(",str_c(shQuote(objet@stades@data$std_code),collapse=","),"),",			
					"datedebut=",shQuote(strftime(objet@pasDeTemps@dateDebut,format="%d/%m/%Y")),
					",datefin=",shQuote(strftime(DateFin(objet@pasDeTemps),format="%d/%m/%Y")),")")
			# removing backslashes
			funout(str_c(sortie1,sortie2))
			return(NULL)
		})

#' Function to calculate daily migration from migration monitoring whose length is more than one day
#' @param time.sequence the time sequence to be filled in with new data
#' @param datasub the initial dataset
#' @param negative "boolean", default FALSE, TRUE indicates a separate sum for negative and positive migrations
#' @returnType data.frame
#' @return A data.frame, with numbers split from operation period
#' to time.sequence period and summed over the new sequence. A migration operation spanning several days will
#' be converted to "daily" values assuming that the migration was regular over time. The function
#' returns one row per taxa, stages, counting device. It does not account for the destination of taxa. It returns
#' separate rows for quantities and numbers. Several columns are according to the type of measure (MESURE, CALCULE, PONCTUEL or EXPERT).
#' @seealso calcule,BilanMigrationMult-method
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
fun_bilanMigrationMult_Overlaps <- function(time.sequence, datasub,negative=FALSE) {
	mat1<-as.data.frame(cbind(as.numeric(time.sequence),as.numeric(time.sequence+as.difftime(1,units="days"))))
	mat2<-as.data.frame(cbind(as.numeric(datasub$ope_date_debut),as.numeric(datasub$ope_date_fin)))
	rownames(mat1)<-as.character(time.sequence)
	rownames(mat2)<-datasub$lot_identifiant
	imat1<-Intervals(mat1)
	closed(imat1)<-c(FALSE,FALSE)
	imat2<-Intervals(mat2)
	closed(imat2)<-c(FALSE,FALSE)
	listei<-interval_overlap(imat2,imat1)
	listei2<-listei # copie de la liste pour l'écraser
	for (i in 1:length(listei)){
		vec<-listei[[i]]
		if (length(vec)==0){
			# pas de lot
			listei2[[i]]=0
		} else 	if (length(vec)==1){
			# l'ensemble du lot est inclus dans la journée
			listei2[[i]]=1
		} else {
			# le premier jour va du début de l'opé à la fin de la première date
			# puis n-2 jour
			# puis le dernier jour de la date de début à la fin de l'ope
			idlot=names(listei)[i]
			tps=c(
					difftime(
							time.sequence[vec[1]]+as.difftime(1,units="days"),
							datasub[datasub$lot_identifiant==idlot,"ope_date_debut"],
							unit="days"),
					rep(1,length(vec)-2),
					difftime(
							datasub[datasub$lot_identifiant==idlot,"ope_date_fin"],
							time.sequence[vec[length(vec)]],
							unit="days")
			)
			listei2[[i]]<-as.numeric(tps)/(as.numeric(sum(tps))) # on ramène à 1
			stopifnot(all.equal(as.numeric(sum(listei2[[i]])),1))					
		}
	}
	# df ["lot_identifiant","coef","ts.id"]
	# lot_identifiant= identifiant du lot, coef = part du lot dans chaque id_seq (sequence de jours), "id_seq" numero du jour
	# creating a table with lot_identifiant, sequence, and the coeff to apply
	df<-data.frame(lot_identifiant = rep(names(listei2), sapply(listei2, length)),
			coef = unlist(listei2),ts_id=unlist(listei)	)
	# dataframe corresponding to the whole time sequence
	df.ts=data.frame(debut_pas=time.sequence,
			fin_pas=time.sequence+as.difftime(1,units="days"),
			ts_id=as.numeric(strftime(time.sequence,format="%j")),stringsAsFactors =FALSE)
	dfts<-merge(df.ts,df,by="ts_id")
	datasub1<-merge(dfts,datasub,by="lot_identifiant")
	# ci dessous pour faire du group by c'est quand même bien de passer par sqldf
	if (negative){
		datasub2<-sqldf("SELECT  debut_pas,
						fin_pas,
						sum(value*coef) as value,
						type_de_quantite,
						ope_dic_identifiant,
						lot_tax_code,
						lot_std_code,
						lot_methode_obtention 	
						FROM datasub1 
						where value<0		
						GROUP BY ope_dic_identifiant,lot_tax_code, lot_std_code, lot_methode_obtention, debut_pas,fin_pas,type_de_quantite
						ORDER BY ope_dic_identifiant,debut_pas, lot_tax_code, lot_std_code,type_de_quantite 
						UNION
						SELECT  debut_pas,
						fin_pas,
						sum(value*coef) as value,
						type_de_quantite,
						ope_dic_identifiant,
						lot_tax_code,
						lot_std_code,
						lot_methode_obtention 	
						FROM datasub1 		
						where value>=0
						GROUP BY ope_dic_identifiant,lot_tax_code, lot_std_code, lot_methode_obtention, debut_pas,fin_pas,type_de_quantite
						ORDER BY ope_dic_identifiant,debut_pas, lot_tax_code, lot_std_code,type_de_quantite"
		)
	} else {
		datasub2<-sqldf("SELECT  debut_pas,
						fin_pas,
						sum(value*coef) as value,
						type_de_quantite,
						ope_dic_identifiant,
						lot_tax_code,
						lot_std_code,
						lot_methode_obtention 	
						FROM datasub1 		
						GROUP BY ope_dic_identifiant,lot_tax_code, lot_std_code, lot_methode_obtention, debut_pas,fin_pas,type_de_quantite
						ORDER BY ope_dic_identifiant,debut_pas, lot_tax_code, lot_std_code,type_de_quantite ")
	}
	stopifnot(all.equal(sum(datasub$value,na.rm=TRUE),sum(datasub2$value,na.rm=TRUE)))
	datasub3<-dcast(datasub2, debut_pas+fin_pas+ope_dic_identifiant+lot_tax_code+lot_std_code+type_de_quantite~lot_methode_obtention,value.var="value")
	if (!"MESURE"%in%colnames(datasub3)) 	datasub3$MESURE=0
	if (!"CALCULE"%in%colnames(datasub3)) 	datasub3$CALCULE=0
	if (!"EXPERT"%in%colnames(datasub3)) 	datasub3$EXPERT=0
	if (!"PONCUTEL"%in%colnames(datasub3)) 	datasub3$PONCTUEL=0
	datasub3$MESURE[is.na(datasub3$MESURE)]<-0
	datasub3$CALCULE[is.na(datasub3$CALCULE)]<-0
	datasub3$EXPERT[is.na(datasub3$EXPERT)]<-0
	datasub3$PONCTUEL[is.na(datasub3$PONCTUEL)]<-0
	# pour compatibilité
	datasub3<-cbind(data.frame("No.pas"=as.numeric(strftime(datasub3$debut_pas,format="%j"))-1),datasub3)
	datasub3$Effectif_total=rowSums(datasub3[,c("MESURE","CALCULE","EXPERT","PONCTUEL")])
	return(datasub3)
}



#' Function to calculate daily migration from migration monitoring whose length is less than one day,
#'  typically video recording whose period are instant events.
#' @param time.sequence the time sequence to be filled in with new data
#' @param datasub the initial dataset
#' #' @param negative "boolean", default FALSE, TRUE indicates a separate sum for negative and positive migrations
#' @returnType data.frame
#' @return A data.frame with number summed over over the time.sequence. 
#' The function returns the same output than \link{fun_bilanMigrationMult_Overlaps}
#' but is intended to work faster
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
fun_bilanMigrationMult <- function(time.sequence, datasub,negative=FALSE) {
	df.ts=data.frame(debut_pas=time.sequence,
			fin_pas=time.sequence+as.difftime(1,units="days"),
			ts_id=strftime(time.sequence,format="%j"),stringsAsFactors =FALSE)
	datasub$ts_id<-strftime(datasub$ope_date_debut,format="%j")
	datasub1<-merge(df.ts,datasub,by="ts_id")
	# ci dessous pour faire du group by c'est quand même bien de passer par sqldf
	if (negative){
		datasub2<-sqldf("SELECT  debut_pas,
						fin_pas,
						sum(value) as value,
						type_de_quantite,
						ope_dic_identifiant,
						lot_tax_code,
						lot_std_code,
						lot_methode_obtention
						FROM datasub1 
						WHERE value>=0
						GROUP BY ope_dic_identifiant,lot_tax_code, lot_std_code, lot_methode_obtention, debut_pas,fin_pas,type_de_quantite
						ORDER BY ope_dic_identifiant,debut_pas, lot_tax_code, lot_std_code,type_de_quantite 
						UNION
						SELECT  debut_pas,
						fin_pas,
						sum(value) as value,
						type_de_quantite,
						ope_dic_identifiant,
						lot_tax_code,
						lot_std_code,
						lot_methode_obtention
						FROM datasub1 
						WHERE value<0
						GROUP BY ope_dic_identifiant,lot_tax_code, lot_std_code, lot_methode_obtention, debut_pas,fin_pas,type_de_quantite
						ORDER BY ope_dic_identifiant,debut_pas, lot_tax_code, lot_std_code,type_de_quantite ")
	} else {
		datasub2<-sqldf("SELECT  debut_pas,
						fin_pas,
						sum(value) as value,
						type_de_quantite,
						ope_dic_identifiant,
						lot_tax_code,
						lot_std_code,
						lot_methode_obtention
						FROM datasub1 
						GROUP BY ope_dic_identifiant,lot_tax_code, lot_std_code, lot_methode_obtention, debut_pas,fin_pas,type_de_quantite
						ORDER BY ope_dic_identifiant,debut_pas, lot_tax_code, lot_std_code,type_de_quantite")
	}
	stopifnot(all.equal(sum(datasub$value,na.rm=TRUE),sum(datasub2$value,na.rm=TRUE)))
	datasub3<-dcast(datasub2, debut_pas+fin_pas+ope_dic_identifiant+lot_tax_code+lot_std_code+type_de_quantite~lot_methode_obtention,value.var="value")
	if (!"MESURE"%in%colnames(datasub3)) 	datasub3$MESURE=0
	if (!"CALCULE"%in%colnames(datasub3)) 	datasub3$CALCULE=0
	if (!"EXPERT"%in%colnames(datasub3)) 	datasub3$EXPERT=0
	if (!"PONCUTEL"%in%colnames(datasub3)) 	datasub3$PONCTUEL=0
	datasub3$MESURE[is.na(datasub3$MESURE)]<-0
	datasub3$CALCULE[is.na(datasub3$CALCULE)]<-0
	datasub3$EXPERT[is.na(datasub3$EXPERT)]<-0
	datasub3$PONCTUEL[is.na(datasub3$PONCTUEL)]<-0
	datasub3<-cbind(data.frame("No.pas"=as.numeric(strftime(datasub3$debut_pas,format="%j"))-1),datasub3)	
	datasub3$Effectif_total=rowSums(datasub3[,c("MESURE","CALCULE","EXPERT","PONCTUEL")])
	return(datasub3)
}

#' returns a table where weights and number are calculated from number and weights respectively
#' performs a query to collect the conversion coefficients
#' @param tableau 
#' @param duree 
#' @returnType data.frame
#' @return tableau
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
fun_weight_conversion=function(tableau,duree) { 
	funout(paste("dc=",unique(tableau$ope_dic_identifiant),get("msg",envir=envir_stacomi)$funtraitement_poids.1))
	nr<-table(tableau$type_de_quantite)[1]
	tableaupoids=subset(tableau,tableau$type_de_quantite==unique(tableau$type_de_quantite)[2])
	tableaueffectif=subset(tableau,tableau$type_de_quantite==unique(tableau$type_de_quantite)[1])
	tableaueffectif= tableaueffectif[,c("No.pas", "lot_tax_code","lot_std_code","CALCULE","MESURE","EXPERT","PONCTUEL","Effectif_total")]       
	tableaudesdeux=tableau[,c("No.pas","debut_pas","fin_pas","ope_dic_identifiant","lot_tax_code","lot_std_code","coe_valeur_coefficient")]
	tableaudesdeux=tableaudesdeux[!duplicated(tableaudesdeux[,c("No.pas","lot_tax_code","lot_std_code")]),]
	# Conversion des  poids en effectifs
	tableauconvert=tableaupoids[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total")]
	tableauconvert=tableauconvert*tableaupoids$coe_valeur_coefficient       # les coeff sont du type 2.54 et non 0.3
	if (sum(tableaupoids$coe_valeur_coefficient)==0) funout(get("msg",envir=envir_stacomi)$funtraitement_poids.2)
	# creation d'une tableau (matricepoids) a 5 colonnes comprenant les effectifs convertis
	matricepoids=cbind(tableaupoids[,c("No.pas", "lot_tax_code","lot_std_code")],tableauconvert,tableaupoids[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total")])
	dimnames(matricepoids)=list(1:length(tableaupoids[,1]),c(
					"No.pas",
					"lot_tax_code",
					"lot_std_code",
					"MESURE",
					"CALCULE",
					"EXPERT",
					"PONCTUEL",
					"Effectif_total",
					"poids_MESURE",
					"poids_CALCULE",
					"poids_EXPERT",
					"poids_PONCTUEL",
					"Poids_total"
			))
	tableau=merge(tableaudesdeux,tableaueffectif,by=c("No.pas","lot_tax_code","lot_std_code"))
	tableau=merge(tableau, matricepoids, all.x = TRUE,by=c("No.pas","lot_tax_code","lot_std_code"),
			sort = TRUE, suffixes=c(".e",".p"))
	# je vire les NA
	tableau[is.na(tableau)]=0
	tableau$MESURE=tableau$MESURE.e+tableau$MESURE.p        
	tableau$CALCULE=tableau$CALCULE.e+tableau$CALCULE.p 
	tableau$EXPERT=tableau$EXPERT.e+tableau$EXPERT.p
	tableau$PONCTUEL=tableau$PONCTUEL.e+tableau$PONCTUEL.p 
	tableau$Effectif_total=tableau$Effectif_total.e+tableau$Effectif_total.p
	tableau[,"poids_depuis_effectifs"]=tableau[,"Effectif_total.e"]/
			tableau$coe_valeur_coefficient		
	stopifnot(nr==nrow(tableau))
	return(tableau)
}
