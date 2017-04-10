#' Migration report for one DC, one species and one stage
#' 
#' @include RefTaxon.r
#' @include RefStades.r
#' @include PasDeTempsJournalier.r
#' @slot dc Object of class \link{RefDC-class}: the control device 
#' @slot taxons Object of class \link{RefTaxon-class}: the species
#' @slot stades Object of class \link{RefStades-class} : the stage of the fish
#' @slot pasDeTemps Object of class \link{PasDeTempsJournalier-class} : the time step 
#' constrained to daily value and 365 days
#' @slot data Object of class \code{data.frame} with data filled in from the connect method
#' @slot calcdata A "list" of calculated daily data, one per dc, filled in by the calcule method
#' @slot coef_conversion A data.frame of daily weight to number conversion coefficients, filled in by the connect 
#' method if any weight are found in the data slot.
#' @slot time.sequence Object of class \code{POSIXct} : a time sequence of days generated by the calcule method
#' @note Method \code{plot(...,type="standard")} also calls a function that will write to the database if
#' a connection to the database is expected.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @family Bilan Objects
#' @keywords classes
#' @aliases BilanMigration bilanMigration
#' @example inst/examples/bilanMigration_Arzal.R
#' @export 
setClass(Class="BilanMigration",
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
		))
# bilanMigration= new("BilanMigration")

setValidity("BilanMigration",function(object)
		{
			rep1=length(object@dc)==1
			rep2=length(object@taxons)==1
			rep3=length(object@stades)==1
			rep3=length(object@pasDeTemps)==1
			rep4=(object@pasDeTemps@nbStep==365|object@pasDeTemps@nbStep==366) # constraint 365 to 366 days
			rep5=as.numeric(strftime(object@pasDeTemps@dateDebut,'%d'))==1 # contrainte : depart = 1er janvier
			rep6=as.numeric(strftime(object@pasDeTemps@dateDebut,'%m'))==1
			return(ifelse(rep1 & rep2 & rep3 & rep4 & rep5 & rep6 , TRUE ,c(1:6)[!c(rep1, rep2, rep3, rep4, rep5, rep6)]))
		}   
)

#' handler for calculations BilanMigration
#' 
#'  internal use
#' @param h handler
#' @param ... additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
hbilanMigrationcalc=function(h,...){
	bilanMigration<-get("bilanMigration",envir=envir_stacomi)
	bilanMigration<-charge(bilanMigration)
	bilanMigration<-connect(bilanMigration)
	bilanMigration<-calcule(bilanMigration)
}

#' connect method for BilanMigration
#' 
#' 
#' uses the BilanMigrationMult method
#' @param object An object of class \link{BilanMigration-class}
#' @param silent Boolean default FALSE, if TRUE information messages not displayed
#' @return BilanMigration with slot @data filled from the database
#' @export
setMethod("connect",signature=signature("BilanMigration"),definition=function(object,silent=FALSE){ 
			bilanMigration<-object
			bilanMigrationMult<-as(bilanMigration,"BilanMigrationMult")
			bilanMigrationMult<-connect(bilanMigrationMult,silent=silent)
			bilanMigration@data<-bilanMigrationMult@data
			bilanMigration@coef_conversion<-bilanMigrationMult@coef_conversion
			return(bilanMigration)
		})
#' command line interface for BilanMigration class
#' @param object An object of class \link{BilanMigration-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,RefDC-method}
#' @param taxons Either a species name in latin or the SANDRE code for species (ie 2038=Anguilla anguilla),
#' these should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,RefTaxon-method}
#' @param stades A stage code matching the ref.tr_stadedeveloppement_std table in the stacomi database see \link{choice_c,RefStades-method}
#' @param datedebut The starting date as a character, formats like \code{\%Y-\%m-\%d} or \code{\%d-\%m-\%Y} can be used as input
#' @param datefin The finishing date of the Bilan, for this class this will be used to calculate the number of daily steps.
#' @return An object of class \link{BilanMigration-class}
#' The choice_c method fills in the data slot for RefDC, RefTaxon, RefStades, and RefPasDeTempsJournalier and then 
#' uses the choice_c methods of these object to select the data.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("choice_c",signature=signature("BilanMigration"),definition=function(object,dc,taxons,stades,datedebut,datefin){
			# code for debug using bM_Arzal example
			#bilanMigration<-bM_Arzal;dc=5;taxons="Liza ramada";stades="IND";datedebut="2015-01-01";datefin="2015-12-31"
			bilanMigration<-object
			bilanMigration@dc=charge(bilanMigration@dc)
			# loads and verifies the dc
			# this will set dc_selectionne slot
			bilanMigration@dc<-choice_c(object=bilanMigration@dc,dc)
			# only taxa present in the bilanMigration are used
			bilanMigration@taxons<-charge_avec_filtre(object=bilanMigration@taxons,bilanMigration@dc@dc_selectionne)			
			bilanMigration@taxons<-choice_c(bilanMigration@taxons,taxons)
			bilanMigration@stades<-charge_avec_filtre(object=bilanMigration@stades,bilanMigration@dc@dc_selectionne,bilanMigration@taxons@data$tax_code)	
			bilanMigration@stades<-choice_c(bilanMigration@stades,stades)
			bilanMigration@pasDeTemps<-choice_c(bilanMigration@pasDeTemps,datedebut,datefin)
			return(bilanMigration)
		})

#' charge method for BilanMigration
#' 
#' this method creates additional classes in envir_stacomi for later use in plot (operations, 
#' DF operation, DC operation.
#' @param object An object of class \code{\link{BilanMigration-class}}
#' @param silent Should the program be returning messages
#' @return An object of class \link{BilanMigration-class} with slots filled by user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("charge",signature=signature("BilanMigration"),definition=function(object,silent=FALSE){ 
			bilanMigration<-object
			#bilanMigration<-bM_Arzal
			#pour l'instant ne lancer que si les fenetre sont fermees
			# funout(gettext("launching updateplot \n",domain="R-stacomiR"))
			if (exists("refDC",envir_stacomi)) {
				bilanMigration@dc<-get("refDC",envir_stacomi)
				dc<-bilanMigration@dc@dc_selectionne
				df<-bilanMigration@dc@data$df[bilanMigration@dc@data$dc%in%dc]
			} else {
				funout(gettext("You need to choose a counting device, clic on validate\n",domain="R-stacomiR"),arret=TRUE)	
			}
			if (exists("refTaxon",envir_stacomi)) {
				bilanMigration@taxons<-get("refTaxon",envir_stacomi)
			} else {      
				funout(gettext("You need to choose a taxa, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("refStades",envir_stacomi)){
				bilanMigration@stades<-get("refStades",envir_stacomi)
			} else 
			{
				funout(gettext("You need to choose a stage, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("pasDeTemps",envir_stacomi)){
				bilanMigration@pasDeTemps<-get("pasDeTemps",envir_stacomi)
				# pour permettre le fonctionnement de Fonctionnement DC
			} else {
				funout(gettext("Attention, no time step selected, compunting with default value\n",domain="R-stacomiR"),arret=FALSE)
				warning(gettext("Attention, no time step selected, compunting with default value\n",domain="R-stacomiR"))
			}
			
			#################################
			# loading data for other classes associated with bilanMigrationMult
			#################################
			bilanFonctionnementDF=new("BilanFonctionnementDF")		
			bilanFonctionnementDC=new("BilanFonctionnementDC")
			bilanOperation=new("BilanOperation")
			assign("bilanFonctionnementDC_date_debut",get("pasDeTemps",envir_stacomi)@"dateDebut",envir_stacomi)
			assign("bilanFonctionnementDC_date_fin",as.POSIXlt(DateFin(get("pasDeTemps",envir_stacomi))),envir_stacomi)
			assign("bilanFonctionnementDF_date_debut",get("pasDeTemps",envir_stacomi)@"dateDebut",envir_stacomi)
			assign("bilanFonctionnementDF_date_fin",as.POSIXlt(DateFin(get("pasDeTemps",envir_stacomi))),envir_stacomi)
			assign("bilanOperation_date_debut",get("pasDeTemps",envir_stacomi)@"dateDebut",envir_stacomi)
			assign("bilanOperation_date_fin",as.POSIXlt(DateFin(get("pasDeTemps",envir_stacomi))),envir_stacomi)
			
			bilanOperation<-charge(bilanOperation) 
			# charge will search for refDC (possible multiple choice), bilanOperation_date_debut
			# and bilanOperation_date_fin in envir_stacomi
			# charge will search for refDC (possible multiple choice), bilanFonctionnementDC_date_debut
			# and bilanFonctionnementDC_date_fin in envir_stacomi
			bilanFonctionnementDC<-charge(bilanFonctionnementDC)
			refDF=new("RefDF")
			refDF<-charge(refDF)
			refDF<-choice_c(refDF,df)
			
			assign("refDF",refDF,envir=envir_stacomi)
			
			# charge will search for refDF (possible multiple choice), bilanFonctionnementDF_date_debut
			# and bilanFonctionnementDF_date_fin in envir_stacomi
			bilanFonctionnementDF<-charge(bilanFonctionnementDF)
			# the object are assigned to the envir_stacomi for later use by the connect method
			assign("bilanFonctionnementDF",bilanFonctionnementDF,envir=envir_stacomi)
			assign("bilanFonctionnementDC",bilanFonctionnementDC,envir=envir_stacomi)
			assign("bilanOperation",bilanOperation,envir=envir_stacomi)					
			return(bilanMigration)
		})


#' calcule method for BilanMigration
#' 
#'  does the calculation once data are filled,. It also performs conversion from weight to numbers
#' in with the connect method
#' @param object An object of class \code{\link{BilanMigration-class}}
#' @param negative a boolean indicating if a separate sum must be done for positive and negative values, if true, positive and negative counts return 
#' different rows
#' @param silent Boolean, if TRUE, information messages are not displayed, only warnings and errors
#' @note The class BilanMigration does not handle escapement rates nor 
#' 'devenir' i.e. the destination of the fishes.
#' @return BilanMigration with calcdata slot filled.
#' @export
setMethod("calcule",signature=signature("BilanMigration"),definition=function(object,negative=FALSE,silent=FALSE){ 
			#bilanMigration<-bM_Arzal
			#bilanMigration<-bM_Arzal_civ
			#negative=FALSE;silent=FALSE
			if (!silent){
				funout(gettext("Starting migration summary ... be patient\n",domain="R-stacomiR"))
			}
			bilanMigration<-object
			
			if (nrow(bilanMigration@data)>0){
#				bilanMigration@data$time.sequence=difftime(bilanMigration@data$ope_date_fin,
#						bilanMigration@data$ope_date_debut,
#						units="days")
				debut=bilanMigration@pasDeTemps@dateDebut
				fin=DateFin(bilanMigration@pasDeTemps)
				time.sequence<-seq.POSIXt(from=debut,to=fin,
						by=as.numeric(bilanMigration@pasDeTemps@stepDuration))
				bilanMigration@time.sequence<-time.sequence
				lestableaux<-list()			
				datasub<-bilanMigration@data	
				dic<-unique(bilanMigration@data$ope_dic_identifiant)
				stopifnot(length(dic)==1)
				datasub$duree=difftime(datasub$ope_date_fin,datasub$ope_date_debut,units="days")
				if (any(datasub$duree>(bilanMigration@pasDeTemps@stepDuration/86400))){				
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
						coe<-bilanMigration@coef_conversion[,c("coe_date_debut","coe_valeur_coefficient")]
						data$coe_date_debut<-as.Date(data$debut_pas)
						data<-merge(data,coe,by="coe_date_debut")
						data<-data[,-1] # removing coe_date_debut
						data <-fun_weight_conversion(tableau=data,time.sequence=bilanMigration@time.sequence,silent)
					}
					
					lestableaux[[stringr::str_c("dc_",dic)]][["data"]]<-data
					
				} else {
					#----------------------
					#bilan simple
					#----------------------
					data<-fun_bilanMigrationMult(time.sequence = time.sequence,datasub=datasub,negative=negative)
					data$taux_d_echappement=-1					
					contient_poids<-"poids"%in%datasub$type_de_quantite
					if (contient_poids){
						coe<-bilanMigration@coef_conversion[,c("coe_date_debut","coe_valeur_coefficient")]
						data$coe_date_debut<-as.Date(data$debut_pas)
						data<-merge(data,coe,by="coe_date_debut")
						data<-data[,-1] # removing coe_date_debut
						data <-fun_weight_conversion(tableau=data,time.sequence=bilanMigration@time.sequence,silent)
					} else {
						data$coe_valeur_coefficient=NA
					}
					lestableaux[[stringr::str_c("dc_",dic)]][["data"]]<-data
					lestableaux[[stringr::str_c("dc_",dic)]][["method"]]<-"sum"
					lestableaux[[stringr::str_c("dc_",dic)]][["contient_poids"]]<-contient_poids
					lestableaux[[stringr::str_c("dc_",dic)]][["negative"]]<-negative
				}
				# TODO developper une methode pour sumneg 
				bilanMigration@calcdata<-lestableaux
				assign("bilanMigration",bilanMigration,envir_stacomi)
				if (!silent){
					funout(gettext("Summary object is stocked into envir_stacomi environment : write bilanMigration=get('bilanMigration',envir_stacomi) \n",domain="R-stacomiR"))
					funout(gettext("To access calculated data, type bilanMigration@calcdata\n",domain="R-stacomiR"))
				}
				
				
				
			} else {
				# no fish...
				funout(gettext("There are no values for the taxa, stage and selected period\n",domain="R-stacomiR"))
			}
			return(bilanMigration)
		})



#' handler to print the command line
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
houtBilanMigration=function(h=null,...) {
	if (exists("refStades",envir_stacomi)) 	{
		bilanMigration<-get("bilanMigration",envir_stacomi)
		print(bilanMigration)
	} 
	else 
	{      
		funout(gettext("Please select DC, taxa, and stages for a complete command\n",domain="R-stacomiR"),arret=TRUE)
	}
}

#' Method to print the command line of the object
#' @param x An object of class BilanMigration
#' @param ... Additional parameters passed to print
#' @return NULL
#' @author cedric.briand
#' @export
setMethod("print",signature=signature("BilanMigration"),definition=function(x,...){ 
			sortie1<-"bilanMigration=new('BilanMigration');"
			sortie2<-stringr::str_c("bilanMigration=choice_c(bilanMigration,",
					"dc=c(",stringr::str_c(x@dc@dc_selectionne,collapse=","),"),",
					"taxons=c(",stringr::str_c(shQuote(x@taxons@data$tax_nom_latin),collapse=","),"),",
					"stades=c(",stringr::str_c(shQuote(x@stades@data$std_code),collapse=","),"),",			
					"datedebut=",shQuote(strftime(x@pasDeTemps@dateDebut,format="%d/%m/%Y")),
					",datefin=",shQuote(strftime(DateFin(x@pasDeTemps),format="%d/%m/%Y")),")")
			# removing backslashes
			funout(stringr::str_c(sortie1,sortie2),...)
			return(invisible(NULL))
		})




#' Plots of various type for BilanMigration.
#' 
#' \itemize{
#' 		\item{plot.type="standard"}{calls \code{\link{fungraph}} and \code{\link{fungraph_civelle}} functions to plot as many "bilanmigration"
#' 			as needed, the function will test for the existence of data for one dc, one taxa, and one stage}
#' 		\item{plot.type="step"}{creates Cumulated graphs for BilanMigrationMult.  Data are summed per day for different dc taxa and stages}
#' 		\item{plot.type="multiple"}{Method to overlay graphs for BilanMigrationMult (multiple dc/taxa/stage in the same plot)}
#' }
#' @param x An object of class BilanMigration
#' @param y From the formals but missing
#' @param plot.type One of "standard","step". Defaut to \code{standard} the standard BilanMigration with dc and operation displayed, can also be \code{step} or 
#' \code{multiple} 
#' @param silent Stops displaying the messages.
#' @param ... Additional arguments, see \code{plot}, \code{plot.default} and \code{par}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("plot",signature(x = "BilanMigration", y = "ANY"),definition=function(x, y,plot.type="standard",silent=FALSE,...){ 
			#bilanMigration<-bM_Arzal
			#bilanMigration<-x
			if (exists("bilanMigration",envir_stacomi)) {
				bilanMigration<-get("bilanMigration",envir_stacomi)
			} else {      
				funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
			}
			################################################################
			#                 standard plot
			################################################################
			if (plot.type=="standard"){
				if (!silent) print("plot type standard")
				if (!silent) funout(gettext("Statistics about migration :\n",domain="R-stacomiR"))				
				taxon=bilanMigration@taxons@data[1,"tax_nom_latin"]
				stade=bilanMigration@stades@data[1,"std_libelle"]
				dc=as.numeric(bilanMigration@dc@dc_selectionne)[1]
				# preparation du jeu de donnees pour la fonction fungraph_civ
				#developpee pour la classe BilanMigration
				data<-bilanMigration@calcdata[[stringr::str_c("dc_",dc)]][["data"]]
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
								data.frame(No.pas=as.numeric(strftime(bilanMigration@time.sequence,format="%j"))-1,
										debut_pas=bilanMigration@time.sequence),
								data,
								by=c("No.pas","debut_pas"),
								all.x=TRUE
						)
						data_without_hole$CALCULE[is.na(data_without_hole$CALCULE)]<-0
						data_without_hole$MESURE[is.na(data_without_hole$MESURE)]<-0
						data_without_hole$EXPERT[is.na(data_without_hole$EXPERT)]<-0
						data_without_hole$PONCTUEL[is.na(data_without_hole$PONCTUEL)]<-0
						if (bilanMigration@calcdata[[stringr::str_c("dc_",dc)]][["contient_poids"]]&
								taxon=="Anguilla anguilla"&
								(stade=="civelle"|stade=="Anguille jaune")) {							
							#----------------------------------
							# bilan migration avec poids (civelles
							#-----------------------------------------
							
							fungraph_civelle(bilanMigration=bilanMigration,
									table=data_without_hole,
									time.sequence=bilanMigration@time.sequence,
									taxon=taxon,
									stade=stade,
									dc=dc,
									silent,
									...)
						}	else {
							
							#----------------------------------
							# bilan migration standard
							#-----------------------------------------
							#silent=TRUE
							fungraph(bilanMigration=bilanMigration,
									tableau=data_without_hole,
									time.sequence=bilanMigration@time.sequence,
									taxon,
									stade,
									dc,
									silent)
						}
					} # end nrow(data)>0	
				} # end is.null(data)
				
				################################################################
				#                 step plot
				################################################################
			} else if (plot.type=="step"){
				taxon= as.character(bilanMigration@taxons@data$tax_nom_latin)
				stade= as.character(bilanMigration@stades@data$std_libelle)
				dc=as.numeric(bilanMigration@dc@dc_selectionne)	
				if (bilanMigration@pasDeTemps@stepDuration==86400 & bilanMigration@pasDeTemps@stepDuration==86400) {
					grdata<-bilanMigration@calcdata[[stringr::str_c("dc_",dc)]][["data"]]
					grdata<-funtraitementdate(grdata,
							nom_coldt="debut_pas",
							annee=FALSE,
							mois=TRUE,
							quinzaine=TRUE,
							semaine=TRUE,
							jour_an=TRUE,
							jour_mois=FALSE,
							heure=FALSE)
					grdata$Cumsum=cumsum(grdata$Effectif_total)
					# pour sauvegarder sous excel
					annee=unique(strftime(as.POSIXlt(bilanMigration@time.sequence),"%Y"))[1]
					dis_commentaire=  as.character(bilanMigration@dc@data$dis_commentaires[bilanMigration@dc@data$dc%in%bilanMigration@dc@dc_selectionne]) 
					update_geom_defaults("line", aes(size = 2))
					
					p<-ggplot(grdata)+
							geom_line(aes(x=debut_pas,y=Cumsum,colour=mois))+
							ylab(gettext("Cumulative migration",domain="R-stacomiR"))+
							ggtitle(gettextf("Cumulative count %s, %s, %s, %s",dis_commentaire,taxon,stade,annee)) + 
							theme(plot.title = element_text(size=10,colour="navy"))+
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
									))
					print(p)	
				} else {
					funout(gettext("Warning, this function applies for annual summaries\n",domain="R-stacomiR"))
				}
			} else {
				stop("unrecognised plot.type argument, plot.type should either be standard or step")
			}
		})



#' handler for  hBilanMigrationgraph
#' 
#' Standard BilanMigration graph over time
#' @param h handler
#' @param ... additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
hbilanMigrationgraph = function(h,...) {
	if (exists("bilanMigration",envir_stacomi)) {
		bilanMigration<-get("bilanMigration",envir_stacomi)
	} else {      
		funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
	}
	plot(bilanMigration,plot.type="standard")
	
}

#' handler for hBilanMigrationgraph2
#' 
#' Step plot over time
#' @param h handler
#' @param ... additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
hbilanMigrationgraph2 = function(h,...) {
	if (exists("bilanMigration",envir_stacomi)) {
		bilanMigration<-get("bilanMigration",envir_stacomi)
	} else {      
		funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
	}
	plot(bilanMigration,plot.type="step")
}

#' handler for summary function, internal use
#' calls functions funstat and funtable to build summary tables in html and
#' csv files
#' @param h Handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
hTableBilanMigration=function(h,...) {
	if (exists("bilanMigration",envir_stacomi)) 
	{
		bilanMigration<-get("bilanMigration",envir_stacomi)
	} 
	else 
	{      
		funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
	}
	summary(bilanMigration)
	
}

#' summary for bilanMigration 
#' calls functions funstat and funtable to create migration overviews
#' and generate csv and html output in the user data directory
#' @param object An object of class \code{\link{BilanMigration-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @param ... Additional parameters (not used there)
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("summary",signature=signature(object="BilanMigration"),definition=function(object,silent=FALSE,...){
			bilanMigrationMult<-as(object,"BilanMigrationMult")
			summary(bilanMigrationMult,silent=silent)			
		})





#' handler hBilanMigrationwrite
#' Allows the saving of daily and monthly counts in the database
#' @note these entries are necessary to run the Interannual Migration class. 
#' then no entry will be written to the database

#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
hbilanMigrationwrite = function(h,...) {
	if (exists("bilanMigration",envir_stacomi)) {
		bilanMigration<-get("bilanMigration",envir_stacomi)
	} else {      
		funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
	}
	# ecriture du bilan journalier, ecrit aussi le bilan mensuel
	database_expected<-get("database_expected",envir=envir_stacomi)
	if (database_expected) {
		write_database(bilanMigration,silent=TRUE)
	}	else {
		funout(gettext("no bilan written to database : database_expected argument=FALSE",domain="R-stacomiR"))
	}
	
}
#' Command line method to write the daily and monthly counts to the 
#' t_bilanmigrationjournalier_bjo table
#' 
#' Daily values are needed to compare migrations from year to year, by the class \link{BilanMigrationInterAnnuelle-class}. They are added by
#' by this function.  
#' @param bilanMigration an object of class \code{\linkS4class{BilanMigration}}
#' @param silent : TRUE to avoid messages
#' @param dbname : the name of the database, defaults to "bd_contmig_nat"
#' @param check_for_bjo : do you want to check if data are already present in the bjo table, and delete them,
#' this param was added otherwise connect method when called from BilanMigrationInterAnnuelle runs in loops
#' @note the user is asked whether or not he wants to overwrite data, if no
#' data are present in the database, the import is done anyway. The name of the database
#' is not passed in odbc link, here defaults to "bd_contmig_nat"
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#' stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE) 
#' data("bM_Arzal")
#' bM_Arzal<-calcule(bM_Arzal)
#' write_database(bilanMigration=bM_Arzal,silent=FALSE)
#' }
#' @export
setMethod("write_database",signature=signature("BilanMigration"),definition=function(object,silent=TRUE,dbname="bd_contmig_nat",check_for_bjo=TRUE){
			# dbname="bd_contmig_nat";host="localhost";silent=FALSE;port=5432
			# object=bM
			#host : the host for sqldf, defaults to "localhost"
			 #port : the port, defaults to 5432
			host=get("sqldf.options",envir=envir_stacomi)["sqldf.host"]
			port=get("sqldf.options",envir=envir_stacomi)["sqldf.port"]		
			bilanMigration<-object
			if (class(bilanMigration)!="BilanMigration") stop("the bilanMigration should be of class BilanMigration")
			if (class(silent)!="logical") stop("the silent argument should be a logical")
			dc=as.numeric(bilanMigration@dc@dc_selectionne)[1]
			data=bilanMigration@calcdata[[stringr::str_c("dc_",dc)]][["data"]]
			data=data[data$Effectif_total!=0,]
			jour_dans_lannee_non_nuls=data$debut_pas	
			col_a_retirer=match(c("No.pas","type_de_quantite","debut_pas","fin_pas"),colnames(data))
			col_a_retirer=col_a_retirer[!is.na(col_a_retirer)] # as in the case of glass eel and weight
			# the columns are not the same
			data=data[,-col_a_retirer]
			# below again the taux_d_echappement not there if glass eel and weights
			if (is.null(data$taux_d_echappement)) data$taux_d_echappement<-NA
			data$taux_d_echappement[data$taux_d_echappement==-1]<-NA 
			if (!is.null(data$coe_valeur_coefficient)){
			data$coe_valeur_coefficient[data$"coe_valeur_coefficient"==1]<-NA 
		    }else {data$coe_valeur_coefficient<-NA}
			cannotbenull=match(c("taux_d_echappement","coe_valeur_coefficient"),colnames(data))
			data[,-cannotbenull][data[,-cannotbenull]==0]<-NA
			annee<-as.numeric(unique(strftime(as.POSIXlt(bilanMigration@time.sequence),"%Y"))[1])
			if ("Poids_total"%in%colnames(data)){
				aat_bilanmigrationjournalier_bjo=cbind(
						bilanMigration@dc@dc_selectionne,
						bilanMigration@taxons@data$tax_code,
						bilanMigration@stades@data$std_code,
						annee, # une valeur
						rep(jour_dans_lannee_non_nuls,ncol(data[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total","Effectif_total.p","Effectif_total.e","poids_depuis_effectifs","Poids_total","taux_d_echappement","coe_valeur_coefficient")])),
						utils::stack(data[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total","Effectif_total.p","Effectif_total.e","poids_depuis_effectifs","Poids_total","taux_d_echappement","coe_valeur_coefficient")]),  
						Sys.time(),
						substr(toupper(get("sch",envir=envir_stacomi)),1,nchar(toupper(get("sch",envir=envir_stacomi)))-1)
				)	
			} else{
				aat_bilanmigrationjournalier_bjo=cbind(
						bilanMigration@dc@dc_selectionne,
						bilanMigration@taxons@data$tax_code,
						bilanMigration@stades@data$std_code,
						annee, # une valeur
						rep(jour_dans_lannee_non_nuls,ncol(data[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total","taux_d_echappement","coe_valeur_coefficient")])),
						utils::stack(data[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total","taux_d_echappement","coe_valeur_coefficient")]),  
						Sys.time(),
						substr(toupper(get("sch",envir=envir_stacomi)),1,nchar(toupper(get("sch",envir=envir_stacomi)))-1)
				)	
			}
			aat_bilanmigrationjournalier_bjo= stacomirtools::killfactor(aat_bilanmigrationjournalier_bjo[!is.na(aat_bilanmigrationjournalier_bjo$values),])
			colnames(aat_bilanmigrationjournalier_bjo)<-c("bjo_dis_identifiant","bjo_tax_code","bjo_std_code","bjo_annee","bjo_jour","bjo_valeur","bjo_labelquantite","bjo_horodateexport","bjo_org_code")
			
			#####
			# Ci dessous conversion de la classe vers migration Interannuelle pour utiliser
			# les methodes de cette classe
			bil=as(bilanMigration,"BilanMigrationInterAnnuelle")
			# the argument check_for_bjo ensures that we don't re-run the connect method
			# in loop when the write_database is called from within the bilanMigrationInterAnnuelle connect method
			# check = FALSE tells the method not to check for missing data (we don't want that check when the
			# write database is called from the bilanMigration class
			if (check_for_bjo) bil=connect(bil,silent=silent,check=FALSE)
			
			hconfirm=function(h,...){			
				# suppression des donnees actuellement presentes dans la base
				# bilanjournalier et bilanmensuel
				if (check_for_bjo) supprime(bil)			
				baseODBC<-get("baseODBC",envir=envir_stacomi)
				sql<-stringr::str_c("INSERT INTO ",get("sch",envir=envir_stacomi),"t_bilanmigrationjournalier_bjo (",			
						"bjo_dis_identifiant,bjo_tax_code,bjo_std_code,bjo_annee,bjo_jour,bjo_valeur,bjo_labelquantite,bjo_horodateexport,bjo_org_code)",
						" SELECT * FROM  aat_bilanmigrationjournalier_bjo;")
				invisible(utils::capture.output(
								sqldf::sqldf(x=sql,
										drv="PostgreSQL",
										user=baseODBC["uid"],
										dbname=dbname,				
										password=baseODBC["pwd"],
										host=host,
										port=port)
						))		
				
				
				if (!silent){
					funout(gettextf("Writing daily summary in the database %s \n",annee))
				}
# si l'utilisateur accepte de remplacer les valeurs				
#progres<-get("progres",envir=envir_stacomi)
#gtkWidgetDestroy(progres)
# ecriture egalement du bilan mensuel
				taxon= as.character(bilanMigration@taxons@data$tax_nom_latin)
				stade= as.character(bilanMigration@stades@data$std_libelle)
				DC=as.numeric(bilanMigration@dc@dc_selectionne)	
				tableau<-bilanMigration@calcdata[[stringr::str_c("dc_",DC)]][["data"]]
				resum=funstat(tableau=tableau,time.sequence=tableau$debut_pas,taxon,stade,DC,silent=silent )
				fn_EcritBilanMensuel(bilanMigration,resum,silent=silent)
			}#end function hconfirm
			
			if (nrow(bil@data)>0)
			{ 
				if (!silent){
					choice<-gWidgets::gconfirm(gettextf("A summary has already been written in the database the :%s Overwrite ?",unique(bil@data$bjo_horodateexport)),
							handler=hconfirm)
				} else {
					hconfirm(h=NULL)
				}
				
			}
			else  # sinon on ecrit les resultats quoiqu'il arrive
			{
				
				baseODBC<-get("baseODBC",envir=envir_stacomi)
				sql<-stringr::str_c("INSERT INTO ",get("sch",envir=envir_stacomi),"t_bilanmigrationjournalier_bjo (",			
						"bjo_dis_identifiant,bjo_tax_code,bjo_std_code,bjo_annee,bjo_jour,bjo_valeur,bjo_labelquantite,bjo_horodateexport,bjo_org_code)",
						" SELECT * FROM  aat_bilanmigrationjournalier_bjo;")
				invisible(utils::capture.output(
								sqldf::sqldf(x=sql,
										drv="PostgreSQL",
										user=baseODBC["uid"],
										dbname=dbname,				
										password=baseODBC["pwd"],
										host=host,
										port=port)
						))		
#	
				
				if (!silent) funout(gettext("Writing daily summary in the database",domain="R-stacomiR"))
				taxon= as.character(bilanMigration@taxons@data$tax_nom_latin)
				stade= as.character(bilanMigration@stades@data$std_libelle)
				DC=as.numeric(bilanMigration@dc@dc_selectionne)	
				tableau<-bilanMigration@calcdata[[stringr::str_c("dc_",DC)]][["data"]]
				resum=funstat(tableau=tableau,time.sequence=tableau$debut_pas,taxon,stade,DC,silent=silent)
				fn_EcritBilanMensuel(bilanMigration,resum,silent=silent)
			} # end else
		})