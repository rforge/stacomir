#' Migration reports for multiple DC / species / stages
#' 
#' Migration counts for several Fish counting devices (DC), several taxa and several stages.
#' This migration count can be built either by the graphical interface or from the command line 
#' (see examples).

#' @note A Migration Bilan comes from a migration monitoring : the fishes are monitored in a section of river, this section is
#' called a control station (station). Most often, there is a dam, one or several fishways (DF) which comprise one or several counting devices (DC).
#' On each counting device, the migration is recorded. It can be either an instant recording (video control) or the use of traps,
#' Operations are monitoring operations during a period. For each operation, several species of fishes can be recorded (samples). In the case of migratory 
#' fishes the stage of development is important as it may indicate generic migrations, to and fro, between the river and the sea.
#' 
#' Hence a Multiple Migration Bilan is built from several one or several counting devices (DC), one or several Taxa (Taxon), one or several stages
#' (stage). The migration can be also recorded not as numbers, but in the case of glass eels, as weight, which will be later transformed to number, 
#' from daily conversion coefficients. The methods in this class test whether the counts are numbers or another type of quantity.
#' This class makes different calculations than BilanMigration, it does not handle escapement coefficients,
#' it uses quantities other than numbers if necessary (only used for glass eel in the connect method).
#' @slot dc An object of class \code{RefDC-class}
#' @slot taxons An object of class \code{\link{RefTaxon-class}}
#' @slot stades An object of class \code{\link{RefStades-class}}
#' @slot pasDeTemps An object of class \code{\link{PasDeTempsJournalier-class}}
#' @slot data A data.frame containing raw data filled by the connect method
#' @slot calcdata A "list" of calculated daily data, one per dc, filled in by the calcule method
#' @slot coef_conversion A data.frame of daily weight to number conversion coefficients, filled in by the connect
#' method if any weight are found in the data slot.
#' @slot time.sequence A POSIXt time sequence
#' @export
#' @example examples/01_BilanMigrationMult/bilanMigrationMult_Arzal.R
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
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

setValidity("BilanMigrationMult",function(object)
		{
			rep1=length(object@dc)>=1
			rep2=length(object@taxons)>=1
			rep3=length(object@stades)>=1
			#	rep3=length(object@pasDeTemps)==1
			#rep4=(object@pasDeTemps@nbStep==365) # contrainte : pendant 365j
			#	rep5=as.numeric(strftime(object@pasDeTemps@dateDebut,'%d'))==1 # contrainte : depart = 1er janvier
			#	rep6=as.numeric(strftime(object@pasDeTemps@dateDebut,'%m'))==1			
			return(ifelse(rep1 & rep2 & rep3 , TRUE ,c(1:6)[!c(rep1, rep2, rep3)]))
		}   
)




#' charge method for BilanMigrationMult
#' 
#' Used by the graphical interface to collect and test objects in the environment envir_stacomi
#' @return BilanMigrationMult with slots filled by user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("charge",signature=signature("BilanMigrationMult"),definition=function(object,...){ 
			bilanMigrationMult<-object
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
#' 
#' The choice_c method fills in the data slot for RefDC, RefTaxon, RefStades and then 
#' uses the choice_c methods of these object to "select" the data.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("choice_c",signature=signature("BilanMigrationMult"),definition=function(object,dc,taxons,stades,datedebut,datefin,...){
			bilanMigrationMult<-object
			fonctionnementDC=new("BilanFonctionnementDC")
			# appel ici pour pouvoir utiliser les fonctions graphiques associees sur fonctionnement du DC
			assign("fonctionnementDC",fonctionnementDC,envir = envir_stacomi)
			bilanMigrationMult@dc=charge(bilanMigrationMult@dc)
			# loads and verifies the dc
			bilanMigrationMult@dc<-choice_c(object=bilanMigrationMult@dc,dc)
			# only taxa present in the bilanMigration are used
			bilanMigrationMult@taxons<-charge_avec_filtre(object=bilanMigrationMult@taxons,bilanMigrationMult@dc@dc_selectionne)			
			bilanMigrationMult@taxons<-choice_c(bilanMigrationMult@taxons,taxons)
			bilanMigrationMult@stades<-charge_avec_filtre(object=bilanMigrationMult@stades,bilanMigrationMult@dc@dc_selectionne,bilanMigrationMult@taxons@data$tax_code)	
			bilanMigrationMult@stades<-choice_c(bilanMigrationMult@stades,stades)
			bilanMigrationMult@pasDeTemps<-choice_c(bilanMigrationMult@pasDeTemps,datedebut,datefin)
			assign("bilanMigrationMult",bilanMigrationMult,envir = envir_stacomi)
			return(bilanMigrationMult)
		})

#' calcule method for BilanMigrationMult
#' 
#'  does the calculation once data are filled. It also performs conversion from weight to numbers
#' in with the connect method
#' @param object An object of class \code{\link{BilanMigrationMult-class}}
#' @param negative a boolean indicating if a separate sum must be done for positive and negative values, if true, positive and negative counts return 
#' different rows
#' @param silent Defautl FALSE, should messages be stopped
#' @note The class BilanMigrationMult does not handle  escapement rates. Use class BilanMigration if you want to handle them. The class does not handler
#' 'devenir' i.e. the destination of the fishes.
#' @return BilanMigrationMult with a list in calcdata, one for each triplet (dc/taxa/stage) with data
#' @export
setMethod("calcule",signature=signature("BilanMigrationMult"),definition=function(object,negative=FALSE,silent=FALSE){ 
			
			bilanMigrationMult<-object
			bilanMigrationMult=connect(bilanMigrationMult)
			if (!silent) cat(stringr::str_c("data collected from the database nrow=",nrow(bilanMigrationMult@data),"\n"))
			
			bilanMigrationMult@data$time.sequence=difftime(bilanMigrationMult@data$ope_date_fin,
					bilanMigrationMult@data$ope_date_debut,
					units="days")
			debut=bilanMigrationMult@pasDeTemps@dateDebut
			fin=DateFin(bilanMigrationMult@pasDeTemps)
			time.sequence<-seq.POSIXt(from=debut,to=fin,
					by=as.numeric(bilanMigrationMult@pasDeTemps@stepDuration))
			bilanMigrationMult@time.sequence<-time.sequence
			lestableaux<-list()
			for (dic in unique(bilanMigrationMult@data$ope_dic_identifiant))	{
				datasub<-bilanMigrationMult@data[bilanMigrationMult@data$ope_dic_identifiant==dic,]
				
				if (any(datasub$time.sequence>(bilanMigrationMult@pasDeTemps@stepDuration/86400))){				
					#----------------------
					# bilans avec overlaps
					#----------------------
					data<-fun_bilanMigrationMult_Overlaps(time.sequence = time.sequence, datasub = datasub,negative=negative)
					# pour compatibilite avec les bilanMigration
					data$taux_d_echappement=-1					
					lestableaux[[stringr::str_c("dc_",dic)]][["data"]]<-data
					lestableaux[[stringr::str_c("dc_",dic)]][["method"]]<-"overlaps"
					contient_poids<-"poids"%in%datasub$type_de_quantite
					lestableaux[[stringr::str_c("dc_",dic)]][["contient_poids"]]<-contient_poids
					lestableaux[[stringr::str_c("dc_",dic)]][["negative"]]<-negative
					if (contient_poids){
						coe<-bilanMigrationMult@coef_conversion[,c("coe_date_debut","coe_valeur_coefficient")]
						data$coe_date_debut<-as.Date(data$debut_pas)
						data<-merge(data,coe,by="coe_date_debut")
						data<-data[,-1] # removing coe_date_debut
						data <-fun_weight_conversion(tableau=data,time.sequence=bilanMigrationMult@time.sequence,silent)
					}
					
					lestableaux[[stringr::str_c("dc_",dic)]][["data"]]<-data
					
				} else {
					#----------------------
					#bilan simple
					#----------------------
					data<-fun_bilanMigrationMult(time.sequence = time.sequence,datasub=datasub,negative=negative)
					data$taux_d_echappement=-1
					data$coe_valeur_coefficient=NA
					contient_poids<-"poids"%in%datasub$type_de_quantite
					if (contient_poids){
						coe<-bilanMigrationMult@coef_conversion[,c("coe_date_debut","coe_valeur_coefficient")]
						data$coe_date_debut<-as.Date(data$debut_pas)
						data<-merge(data,coe,by="coe_date_debut")
						data<-data[,-1] # removing coe_date_debut
						data <-fun_weight_conversion(tableau=data,time.sequence=bilanMigrationMult@time.sequence,silent)
					}
					lestableaux[[stringr::str_c("dc_",dic)]][["data"]]<-data
					lestableaux[[stringr::str_c("dc_",dic)]][["method"]]<-"sum"
					lestableaux[[stringr::str_c("dc_",dic)]][["contient_poids"]]<-contient_poids
					lestableaux[[stringr::str_c("dc_",dic)]][["negative"]]<-negative
				}
			}	# end for dic
			# TODO developper une methode pour sumneg 
			bilanMigrationMult@calcdata<-lestableaux
			assign("bilanMigrationMult",bilanMigrationMult,envir_stacomi)
			if (!silent){
				funout(get("msg",envir_stacomi)$BilanMigrationMult.3)
				funout(get("msg",envir_stacomi)$BilanMigrationMult.4)
			}
			return(bilanMigrationMult)
		})

#' connect method for BilanMigrationMult
#' 
#' 
#' a single query collects data from the database
#' @return BilanMigrationMult with slot @data filled from the database
#' @export
setMethod("connect",signature=signature("BilanMigrationMult"),definition=function(object,...){ 
			# recuperation du BilanMigration
			bilanMigrationMult<-object
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
			req@select = stringr::str_c("SELECT 
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
			req@select<-stringr::str_replace_all(req@select,"[\r\n\t]" , "")
			# the where clause is returned by ODBCWheredate
			req@and=stringr::str_c(" AND ope_dic_identifiant in",dc,
					" AND lot_tax_code in ",tax,
					" AND lot_std_code in ",std,
					" AND lot_lot_identifiant IS NULL")
			req<-stacomirtools::connect(req)
			bilanMigrationMult@data=req@query	
			
			# recuperation des coefficients si il y a des civelles dans le bilan
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
				req<-stacomirtools::connect(req)
				bilanMigrationMult@coef_conversion<-req@query
				
			}
			
			return(bilanMigrationMult)
			
			
		})				



#' handler for graphe method in BilanMigrationMult class
#' 
#' internal use
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
hbilanMigrationMult_graph=function(h=null,...){
	if (exists("bilanMigrationMult",envir_stacomi)) {
		bilanMigrationMult<-get("bilanMigrationMult",envir_stacomi)
	} else {      
		funout(get("msg",envir_stacomi)$BilanMigration.5,arret=TRUE)
	}
	plot(x=bilanMigrationMult,plot.type="standard")
}


#' Plots of various type for BilanMigrationMult
#' 
#' \itemize{
#' 		\item{plot.type="standard"}{calls \code{\link{fungraph}} and \code{\link{fungraph_civelle}} functions to plot as many "bilanmigration"
#' 			as needed, the function will test for the existence of data for one dc, one taxa, and one stage}
#' 		\item{plot.type="step"}{creates Cumulated graphs for BilanMigrationMult.  Data are summed per day for different dc taxa and stages}
#' 		\item{plot.type="multiple"}{Method to overlay graphs for BilanMigrationMult (multiple dc/taxa/stage in the same plot)}
#' }
#' @param x An object of class BilanMigrationMult
#' @param y From the formals but missing
#' @param plot.type One of "standard","step","multiple". Defaut to \code{standard} the standard BilanMigration with dc and operation displayed, can also be \code{step} or 
#' \code{multiple} 
#' @param silent Stops most messages from being displayed
#' @param ... Additional arguments, see \code{plot}, \code{plot.default} and \code{par}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
#method.skeleton("plot", "BilanMigrationMult") 
# getGeneric("plot")
# showMethods("plot")
# methods("plot")
setMethod("plot",signature(x = "BilanMigrationMult", y = "ANY"),definition=function(x, y,plot.type="standard",silent=FALSE,...){ 
			#browser()
			#print("entering plot function")
			#bilanMigrationMult<-bMM_Arzal
			bilanMigrationMult<-x
			lestaxons= bilanMigrationMult@taxons@data
			lesstades= bilanMigrationMult@stades@data
			lesdc=as.numeric(bilanMigrationMult@dc@dc_selectionne)
			#==========================type=1=============================
			if (plot.type=="standard"){
				if (!silent) print("plot type standard")
				if (!silent) funout(get("msg",envir_stacomi)$BilanMigration.9)
				#dcnum=1;taxonnum=1;stadenum=2
				#&&&&&&&&&&&&&&&&&&&&&&&&&debut de boucle&&&&&&&&&&&&&&&&&&&&&&&&&&&
				for (dcnum in 1:length(lesdc)){
					for (taxonnum in 1:nrow(lestaxons)){
						for (stadenum in 1:nrow(lesstades)){
							
							taxon=lestaxons[taxonnum,"tax_nom_latin"]
							stade=lesstades[stadenum,"std_libelle"]
							dc=lesdc[dcnum]
							#print(paste(taxon,stade,dc))
							# preparation du jeu de donnees pour la fonction fungraph_civ
							#developpee pour la classe BilanMigration
							data<-bilanMigrationMult@calcdata[[stringr::str_c("dc_",dc)]][["data"]]
							data<-data[data$lot_tax_code==lestaxons[taxonnum,"tax_code"] &
											data$lot_std_code==lesstades[stadenum,"std_code"],]
							
							if (!is.null(data)){
								if	(nrow(data)>0){
									
									if (!silent) {
										funout(paste("dc=",dc,
														"taxon"=taxon,
														"stade"=stade,"\n"))
										funout("---------------------\n")
									}
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
									if (bilanMigrationMult@calcdata[[stringr::str_c("dc_",dc)]][["contient_poids"]]&
											taxon=="Anguilla anguilla"&
											(stade=="civelle"|stade=="Anguilla jaune")) {
										
										#----------------------------------
										# bilan migration avec poids (civelles
										#-----------------------------------------
										grDevices::X11()
										fungraph_civelle(bilanMigration=bilanMigrationMult,
												table=data_without_hole,
												time.sequence=bilanMigrationMult@time.sequence,
												taxon=taxon,
												stade=stade,
												dc=dc,
												silent,
												...)
									}	else {
										
										#----------------------------------
										# bilan migration standard
										#-----------------------------------------
										grDevices::X11()
										#silent=TRUE
										fungraph(bilanMigration=bilanMigrationMult,
												tableau=data_without_hole,
												time.sequence=bilanMigrationMult@time.sequence,
												taxon,
												stade,
												dc,
												silent)
									}
								} # end nrow(data)>0		
								# ecriture du bilan journalier, ecrit aussi le bilan mensuel
								#fn_EcritBilanJournalier(bilanMigrationMult)
								
							}
						}
					}
				}
				#&&&&&&&&&&&&&&&&&&&&&&&&&fin de boucle&&&&&&&&&&&&&&&&&&&&&&&&&&&
			} 
			
#==========================type=2=============================
			if (plot.type=="step"){
				lestaxons= paste(bilanMigrationMult@taxons@data$tax_nom_latin,collapse=",")
				lesstades=  paste(bilanMigrationMult@stades@data$std_code,collapse=",")
				grdata<-data.frame()
				for (i in 1:length(bilanMigrationMult@calcdata)){
					data<-bilanMigrationMult@calcdata[[i]]$data
					# extracting similar columns (not those calculated)
					data<-data[,c(
									"No.pas","debut_pas","fin_pas","ope_dic_identifiant","lot_tax_code","lot_std_code",
									"MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total"
							)]
					grdata<-rbind(grdata,data)
				}
				names(grdata)<-tolower(names(grdata))
				grdata<-sqldf::sqldf("select sum(effectif_total) as effectif_total,
								\"no.pas\",
								debut_pas
								from grdata
								group by debut_pas,\"no.pas\"
								order by debut_pas")
				grdata_without_hole<-merge(
						data.frame(no.pas=as.numeric(strftime(bilanMigrationMult@time.sequence,format="%j"))-1,
								debut_pas=bilanMigrationMult@time.sequence),
						grdata,
						by=c("no.pas","debut_pas"),
						all.x=TRUE
				)
				grdata_without_hole<-funtraitementdate(grdata_without_hole,
						nom_coldt="debut_pas",
						annee=FALSE,
						mois=TRUE,
						quinzaine=TRUE,
						semaine=TRUE,
						jour_an=TRUE,
						jour_mois=FALSE,
						heure=FALSE)
				grdata_without_hole<-grdata_without_hole[order(grdata_without_hole$no.pas),]
				grdata_without_hole$effectif_total[is.na(grdata_without_hole$effectif_total)]<-0
				
				grdata_without_hole$cumsum=cumsum(grdata_without_hole$effectif_total)
				annee=unique(strftime(as.POSIXlt(bilanMigrationMult@time.sequence),"%Y"))
				dis_commentaire=  paste(as.character(bilanMigrationMult@dc@dc_selectionne),collapse=",") 
				update_geom_defaults("step", aes(size = 3))
				
				p<-ggplot(grdata_without_hole)+
						geom_step(aes(x=debut_pas,y=cumsum,colour=mois))+
						ylab(get("msg",envir_stacomi)$BilanMigration.6)+
						theme(plot.title=element_text(size=10,colour="deepskyblue"))+
						xlab("mois")+
						scale_colour_manual(values=c("01"="#092360",
										"02"="#1369A2",
										"03"="#0099A9",
										"04"="#009780",
										"05"="#67B784",
										"06"="#CBDF7C",
										"07"="#FFE200",
										"08"="#DB9815",
										"09"="#E57B25",
										"10"="#F0522D",
										"11"="#912E0F",
										"12"="#33004B"
								))+
						ggtitle(paste(get("msg",envir_stacomi)$BilanMigration.7,"dc=",dis_commentaire,", tax=",lestaxons,", srd=",lesstades,", ",annee,sep="") )  
				print(p)	
			}
#==========================type=3=============================
			if (plot.type=="multiple"){
				lestaxons= paste(bilanMigrationMult@taxons@data$tax_nom_latin,collapse=",")
				lesstades=  paste(bilanMigrationMult@stades@data$std_code,collapse=",")
				grdata<-data.frame()
				for (i in 1:length(bilanMigrationMult@calcdata)){
					data<-bilanMigrationMult@calcdata[[i]]$data
					# extracting similar columns (not those calculated)
					data<-data[,c(
									"No.pas","debut_pas","fin_pas","ope_dic_identifiant","lot_tax_code","lot_std_code",
									"MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total"
							)]
					grdata<-rbind(grdata,data)
				}
				names(grdata)<-tolower(names(grdata))	
				grdata<-funtraitementdate(grdata,
						nom_coldt="debut_pas",
						annee=FALSE,
						mois=TRUE,
						quinzaine=TRUE,
						semaine=TRUE,
						jour_an=TRUE,
						jour_mois=FALSE,
						heure=FALSE)
				annee=unique(strftime(as.POSIXlt(bilanMigrationMult@time.sequence),"%Y"))
				dis_commentaire=  paste(as.character(bilanMigrationMult@dc@dc_selectionne),collapse=",") 
				grdata<-stacomirtools::chnames(grdata,c("ope_dic_identifiant","lot_tax_code","lot_std_code"),c("DC","taxon","stade"))
				grdata$DC<-as.factor(grdata$DC)
				grdata$taxon<-as.factor(grdata$taxon)
				if (length(unique(grdata$taxon))==1){
					p<-ggplot(grdata,aes(x=debut_pas,y=effectif_total,fill=stade))+
							geom_bar(position="stack", stat="identity")+
							facet_grid(DC~.,scales="free_y")+
							scale_fill_brewer(palette="Set2")
				} else if  (length(unique(grdata$stade))==1){
					p<-ggplot(grdata,aes(x=debut_pas,y=effectif_total,fill=taxon))+
							geom_bar(position="stack", stat="identity")+
							facet_grid(DC~.,scales="free_y")+
							scale_fill_brewer(palette="Set2")	
				} else {
					p<-ggplot(grdata,aes(x=debut_pas,y=effectif_total,fill=stade))+
							geom_bar(position="stack", stat="identity")+
							facet_grid(DC+taxon~.,scales="free_y")+
							scale_fill_brewer(palette="Set2")		
				}
				
				print(p)	
			}
#==========================end / type=3=============================			
		})


#' handler for calculations
#' 
#'  internal use for graphical interface
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso BilanMigrationMult
hbilanMigrationMultcalc=function(h=null,...){
	bilanMigrationMult<-get("bilanMigrationMult",envir=envir_stacomi)
	bilanMigrationMult<-charge(bilanMigrationMult)
	bilanMigrationMult<-calcule(bilanMigrationMult)
}

#' handler du calcul hBilanMigrationgraph2
#' 
#' cumuls de migration au cours du temps
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
hbilanMigrationMultgraph2 = function(h=null,...) {
	if (exists("bilanMigrationMult",envir_stacomi)) {
		bilanMigrationMult<-get("bilanMigrationMult",envir_stacomi)
	} else {      
		funout(get("msg",envir_stacomi)$BilanMigration.5,arret=TRUE)
	}
	plot(bilanMigrationMult,plot.type="step")
}


#' hanler
#' 
#' internal use
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
hbilanMigrationMultgraph3 = function(h=null,...) {
	if (exists("bilanMigrationMult",envir_stacomi)) {
		bilanMigrationMult<-get("bilanMigrationMult",envir_stacomi)
	} else {      
		funout(get("msg",envir_stacomi)$BilanMigration.5,arret=TRUE)
	}
	plot(bilanMigrationMult,plot.type="multiple")
}


#' 
#' 

#' handler function 
#' 
#' internal use
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
hTableBilanMigrationMult=function(h=null,...) {
	funout("Tableau de sortie \n")
	if (exists("bilanMigrationMult",envir_stacomi)) 
	{
		bilanMigrationMult<-get("bilanMigrationMult",envir_stacomi)
	} 
	else 
	{      
		funout(get("msg",envir_stacomi)$BilanMigration.5,arret=TRUE)
	}
	summary(bilanMigrationMult)
}

#' summary for bilanMigrationMult 
#' calls functions funstat and funtable to create migration overviews
#' and generate csv and html output in the user data directory
#' @param object An object of class \code{\link{BilanMigrationMult-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @param ... Additional parameters (not used there)
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("summary",signature=signature(object="BilanMigrationMult"),definition=function(object,silent=FALSE,...){
			bilanMigrationMult<-object		
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
						DC=lesdc[dcnum]
						
						# preparation du jeu de donnees pour la fonction fungraph_civ
						#developpee pour la classe BilanMigration
						data<-bilanMigrationMult@calcdata[[stringr::str_c("dc_",DC)]][["data"]]
						data<-data[data$lot_tax_code==lestaxons[taxonnum,"tax_code"] &
										data$lot_std_code==lesstades[stadenum,"std_code"],]
						
						if (!is.null(data)){
							if	(nrow(data)>0){
								
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
								
								resum=funstat(tableau=data_without_hole,
										time.sequence=bilanMigrationMult@time.sequence,taxon,stade,DC,silent)
								funtable(tableau=data_without_hole,
										time.sequence=bilanMigrationMult@time.sequence,
										taxon,stade,DC,resum,silent)
								
							}
						}
					}
				}
			}
			
			
		})

#' handler to print the command line
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
houtBilanMigrationMult=function(h=null,...) {
	if (exists("refStades",envir_stacomi)) 	{
		bilanMigrationMult<-get("bilanMigrationMult",envir_stacomi)
		print(bilanMigrationMult)
	} 
	else 
	{      
		funout(get("msg",envir_stacomi)$BilanMigrationMult.2,arret=TRUE)
	}
}

#' Method to print the command line of the object
#' @param x An object of class BilanMigrationMult
#' @param ... Additional parameters passed to print
#' @return NULL
#' @author cedric.briand
#' @docType methods
#' @export
setMethod("print",signature=signature("BilanMigrationMult"),definition=function(x,...){ 
			sortie1<-"bilanMigrationMult=new(bilanMigrationMult)\n"
			sortie2<-stringr::str_c("bilanMigrationMult=choice_c(bilanMigrationMult,",
					"dc=c(",stringr::str_c(x@dc@dc_selectionne,collapse=","),"),",
					"taxons=c(",stringr::str_c(shQuote(x@taxons@data$tax_nom_latin),collapse=","),"),",
					"stades=c(",stringr::str_c(shQuote(x@stades@data$std_code),collapse=","),"),",			
					"datedebut=",shQuote(strftime(x@pasDeTemps@dateDebut,format="%d/%m/%Y")),
					",datefin=",shQuote(strftime(DateFin(x@pasDeTemps),format="%d/%m/%Y")),")")
			# removing backslashes
			funout(stringr::str_c(sortie1,sortie2),...)
			return(invisible(NULL))
		})

#' Function to calculate daily migration using overlaps functions
#' 
#' Function to calculate daily migration from migration monitoring whose length is more than one day,
#' this calculation relies on the (false) assumption that migration is evenly spread over time. 
#' @param time.sequence the time sequence to be filled in with new data
#' @param datasub the initial dataset
#' @param negative "boolean", default FALSE, TRUE indicates a separate sum for negative and positive migrations
#' to time.sequence period and summed over the new sequence. A migration operation spanning several days will
#' be converted to "daily" values assuming that the migration was regular over time. The function
#' returns one row per taxa, stages, counting device. It does not account for the destination of taxa. It returns
#' separate rows for quantities and numbers. Several columns are according to the type of measure (MESURE, CALCULE, PONCTUEL or EXPERT).
#' @seealso calcule,BilanMigrationMult-method
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
fun_bilanMigrationMult_Overlaps <- function(time.sequence, datasub,negative=FALSE) {
	mat1<-as.data.frame(cbind(as.numeric(time.sequence),as.numeric(time.sequence+as.difftime(1,units="days"))))
	mat2<-as.data.frame(cbind(as.numeric(datasub$ope_date_debut),as.numeric(datasub$ope_date_fin)))
	rownames(mat1)<-as.character(time.sequence)
	rownames(mat2)<-datasub$lot_identifiant
	imat1<-intervals::Intervals(mat1)
	intervals::closed(imat1)<-c(FALSE,FALSE)
	imat2<-intervals::Intervals(mat2)
	intervals::closed(imat2)<-c(FALSE,FALSE)
	listei<-intervals::interval_overlap(imat2,imat1)
	listei2<-listei # copie de la liste pour l'ecraser
	for (i in 1:length(listei)){
		vec<-listei[[i]]
		if (length(vec)==0){
			# pas de lot
			listei2[[i]]=0
		} else 	if (length(vec)==1){
			# l'ensemble du lot est inclus dans la journee
			listei2[[i]]=1
		} else {
			# le premier jour va du debut de l'ope e la fin de la premiere date
			# puis n-2 jour
			# puis le dernier jour de la date de debut e la fin de l'ope
			idlot=names(listei)[i]
			tps=c(
					difftime(
							time.sequence[vec[1]]+as.difftime(1,units="days"),
							datasub[datasub$lot_identifiant==idlot,"ope_date_debut"],
							units="days"),
					rep(1,length(vec)-2),
					difftime(
							datasub[datasub$lot_identifiant==idlot,"ope_date_fin"],
							time.sequence[vec[length(vec)]],
							units="days")
			)
			listei2[[i]]<-as.numeric(tps)/(as.numeric(sum(tps))) # on ramene e 1
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
	# ci dessous pour faire du group by c'est quand meme bien de passer par sqldf
	datasub1$value<-as.numeric(datasub1$value) # sinon arrondis e des entiers
	if (negative){
		datasub2<-sqldf::sqldf("SELECT  debut_pas,
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
		datasub2<-sqldf::sqldf("SELECT  debut_pas,
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
	datasub3<-reshape2::dcast(datasub2, debut_pas+fin_pas+ope_dic_identifiant+lot_tax_code+lot_std_code+type_de_quantite~lot_methode_obtention,value.var="value")
	if (!"MESURE"%in%colnames(datasub3)) 	datasub3$MESURE=0
	if (!"CALCULE"%in%colnames(datasub3)) 	datasub3$CALCULE=0
	if (!"EXPERT"%in%colnames(datasub3)) 	datasub3$EXPERT=0
	if (!"PONCUTEL"%in%colnames(datasub3)) 	datasub3$PONCTUEL=0
	datasub3$MESURE[is.na(datasub3$MESURE)]<-0
	datasub3$CALCULE[is.na(datasub3$CALCULE)]<-0
	datasub3$EXPERT[is.na(datasub3$EXPERT)]<-0
	datasub3$PONCTUEL[is.na(datasub3$PONCTUEL)]<-0
	# pour compatibilite
	datasub3<-cbind(data.frame("No.pas"=as.numeric(strftime(datasub3$debut_pas,format="%j"))-1),datasub3)
	datasub3$Effectif_total=rowSums(datasub3[,c("MESURE","CALCULE","EXPERT","PONCTUEL")])
	return(datasub3)
}



#' Calculate daily migration by simple repartition
#' 
#' Function to calculate daily migration from migration monitoring whose length is less than one day,
#'  typically video recording whose period are instant events.
#' @param time.sequence the time sequence to be filled in with new data
#' @param datasub the initial dataset
#' @param negative "boolean", default FALSE, TRUE indicates a separate sum for negative and positive migrations
#' @return A data.frame with number summed over over the time.sequence. 
#' The function returns the same output than \link{fun_bilanMigrationMult_Overlaps}
#' but is intended to work faster
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
fun_bilanMigrationMult <- function(time.sequence, datasub,negative=FALSE) {
	df.ts=data.frame(debut_pas=time.sequence,
			fin_pas=time.sequence+as.difftime(1,units="days"),
			ts_id=strftime(time.sequence,format="%j"),stringsAsFactors =FALSE)
	datasub$ts_id<-strftime(datasub$ope_date_debut,format="%j")
	datasub1<-merge(df.ts,datasub,by="ts_id")
	# ci dessous pour faire du group by c'est quand meme bien de passer par sqldf
	if (negative){
		datasub2<-sqldf::sqldf("SELECT  debut_pas,
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
		datasub2<-sqldf::sqldf("SELECT  debut_pas,
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
	datasub3<-reshape2::dcast(datasub2, debut_pas+fin_pas+ope_dic_identifiant+lot_tax_code+lot_std_code+type_de_quantite~lot_methode_obtention,value.var="value")
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
#' @param tableau Table issued from BilanMigration
#' @param time.sequence Time sequence from BilanMigration
#' @param silent If silent=TRUE do not display messages
#' @return tableau, the data frame
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
fun_weight_conversion=function(tableau,time.sequence,silent) { 
	if (!silent) funout(paste("dc=",unique(tableau$ope_dic_identifiant),get("msg",envir=envir_stacomi)$funtraitement_poids.1))
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
