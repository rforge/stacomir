#' Class "Bilan_carlot"
#' 
#' The Bilan_carlot class is used to load and display sample characteristics, which can be either
#' continuous or discrete variable, for instance, it can be used to analyse size or sex structure during
#' a given period. 
#' 
#' @note This class is displayed by interface_bilan_carlot, in the database, the class calls the content
#' of the view vue_lot_ope_car
#' @slot data A data frame
#' @slot dc An object of class \link{RefDC-class}: the control devices
#' @slot taxons An object of class \link{RefTaxon-class}: the speciess
#' @slot stades An object of class \link{RefStades-class} : the stages of the fish
#' @slot par An object of class \link{Refpar-class}: the parameters used
#' @slot horodatedebut An object of class \link{RefHorodate-class}
#' @slot horodatefin An object of class \link{RefHorodate-class}
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Bilan_carlot", ...)}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @family Bilan Objects
#' @keywords classes
#' @example inst/examples/bilancarlot_example.R
#' @export 
setClass(Class="Bilan_carlot",
		representation= representation(
				data="ANY",
				dc="RefDC",
				taxons="RefTaxon",
				stades="RefStades",
				par="Refpar",
				horodatedebut="RefHorodate",
				horodatefin="RefHorodate"
		),
		prototype=prototype(data=data.frame(),
				dc=new("RefDC"),
				taxons=new("RefTaxon"),
				stades=new("RefStades"),
				par=new("Refpar"),
				horodatedebut=new("RefHorodate"),
				horodatefin=new("RefHorodate")				
		))

#' connect method for Bilan_carlot
#' 
#' @param object An object of class \link{Bilan_carlot-class}
#' @param silent Boolean if TRUE messages are not displayed
#' @return An object of class \link{Bilan_carlot-class} 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("connect",signature=signature("Bilan_carlot"),definition=function(object,silent=FALSE) {
			requete<-new("RequeteODBCwheredate")
			requete@baseODBC=get("baseODBC",envir=envir_stacomi)
			requete@select= paste("SELECT * FROM ",get("sch",envir=envir_stacomi),"vue_lot_ope_car",sep="")
			requete@colonnedebut="ope_date_debut"
			requete@colonnefin="ope_date_fin"
			requete@datedebut<-object@horodatedebut@horodate
			requete@datefin<-object@horodatefin@horodate
			requete@order_by="ORDER BY ope_date_debut"
			requete@and=paste(" AND ope_dic_identifiant in ",vector_to_listsql(object@dc@dc_selectionne),
					" AND lot_tax_code in ", vector_to_listsql(object@taxons@data$tax_code),
					" AND lot_std_code in ", vector_to_listsql(object@stades@data$std_code),
					" AND car_par_code in ", vector_to_listsql(object@par@par_selectionne), sep="")
			requete<-stacomirtools::connect(requete) 
			object@data<-requete@query
			if (!silent) funout(gettext("Sample characteristics have been loaded from the database\n",domain="R-stacomiR"))
			return(object)
		})


#' charge method for Bilan_carlot class
#' 
#' this method verifies that boxes have been clicked in the user interface and gets the objects pasted in 
#' envir_stacomi
#' @param object An object of class \link{Bilan_carlot-class} 
#' @param h a handler
#' @return An object of class \link{Bilan_carlot-class} with slots filled with user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
#' @return An object of the class
setMethod("charge",signature=signature("Bilan_carlot"),definition=function(object,h) {
			if (exists("refDC",envir_stacomi)) {
				object@dc<-get("refDC",envir_stacomi)
			} else {
				funout(gettext("You need to choose a counting device, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			} 
			if (exists("refTaxon",envir_stacomi)) {
				object@taxons<-get("refTaxon",envir_stacomi)
			} else {
				funout(gettext("You need to choose a taxa, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("refStades",envir_stacomi)) {
				object@stades<-get("refStades",envir_stacomi)
			} else {
				funout(gettext("You need to choose a stage, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("refpar",envir_stacomi)) {
				object@par<-get("refpar",envir_stacomi)
			} else {
				funout(gettext("You need to choose a parameter, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}		
			# rem pas tres satisfaisant car ce nom est choisi dans l'interface
			if (exists("bilan_carlot_date_debut",envir_stacomi)) {
				object@horodatedebut@horodate<-get("bilan_carlot_date_debut",envir_stacomi)
			} else {
				funout(gettext("You need to choose the starting date\n",domain="R-stacomiR"),arret=TRUE)
			}
			# rem id
			if (exists("bilan_carlot_date_fin",envir_stacomi)) {
				object@horodatefin@horodate<-get("bilan_carlot_date_fin",envir_stacomi)
			} else {
				funout(gettext("You need to choose the ending date\n",domain="R-stacomiR"),arret=TRUE)
			}       
			assign("bilan_carlot",object,envir_stacomi)
			return(object)
		})


#' command line interface for Bilan_carlot class
#' @param object An object of class \link{Bilan_carlot-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,RefDC-method}
#' @param taxons Either a species name in latin or the SANDRE code for species (ie 2038=Anguilla anguilla),
#' these should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,RefTaxon-method}
#' @param stades A stage code matching the ref.tr_stadedeveloppement_std table in the stacomi database, see \link{choice_c,RefStades-method}
#' @param par A parameter matching th ref.tg_parametre_par table in the stacomi database, see \link{choice_c,Refpar-method}
#' @param horodatedebut The starting date as a character, formats like \code{\%Y-\%m-\%d} or \code{\%d-\%m-\%Y} can be used as input
#' @param horodatefin The finishing date of the Bilan, for this class this will be used to calculate the number of daily steps.
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return An object of class \link{BilanMigration-class}
#' The choice_c method fills in the data slot for classes \link{RefDC-class}, \link{RefTaxon-class}, \link{RefStades-class}, \link{Refpar-class} and two slots of \link{RefHorodate-class} and then 
#' uses the choice_c methods of these object to select the data.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("choice_c",signature=signature("Bilan_carlot"),definition=function(object,
				dc,
				taxons,
				stades,
				par,
				horodatedebut,
				horodatefin,
				silent=FALSE){
			# code for debug using example
			#bilan_carlot<-b_carlot;dc=c(5,6);taxons="Anguilla anguilla";stades=c("CIV","AGJ");par=c(1785,1786,1787,"C001");horodatedebut="2010-01-01";horodatefin="2015-12-31"
			bilan_carlot<-object
			bilan_carlot@dc=charge(bilan_carlot@dc)
			# loads and verifies the dc
			# this will set dc_selectionne slot
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
			return(bilan_carlot)
		})
#' Calcule method for Bilan_carlot
#' 
#' @param object An object of class \code{\link{Bilan_carlot-class}} 
#' @param silent Boolean, if TRUE, information messages are not displayed, only warnings and errors
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("calcule",signature=signature("Bilan_carlot"),definition=function(object,silent=FALSE) {
			#bilan_carlot<-b_carlot
			bilan_carlot<-object
			if(nrow(bilan_carlot@data)==0) {
				funout(gettext("No information for these samples during the selected period\n",domain="R-stacomiR"), arret=TRUE)
			}   
			vue_ope_lot=bilan_carlot@data # on recupere le data.frame
			nom_variable=bilan_carlot@par@data$par_nom[bilan_carlot@par@data$par_code%in%bilan_carlot@par@par_selectionne]
			#stopifnot(length(nom_variable)==1)
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
#			vue_ope_lot=stacomirtools::chnames(vue_ope_lot,
#					c("ope_identifiant","lot_identifiant","ope_dic_identifiant","lot_pere",             
#							"ope_date_debut","ope_date_fin","lot_effectif","lot_quantite","lot_tax_code","lot_std_code","tax_nom_latin","std_libelle","dev_code","dev_libelle","par_nom","car_par_code","car_methode_obtention","car_val_identifiant",    "car_valeur_quantitatif","val_libelle", "annee","mois","quinzaine","semaine","jour_365"),
#					c("ope","lot","dic","lot_pere",             
#							"date","date_fin","effectif","quantite","lot_tax_code","lot_std_code","tax","std","dev_code","dev","par","car_par_code","meth","val","val_quant","val_libelle", "annee","mois","quinzaine","semaine","jour"))
			#vue_ope_lot=vue_ope_lot[,c("ope","lot","dic","lot_pere","date","effectif","quantite","tax","std","dev","par","meth","val","val_quant","val_libelle", "annee","mois","quinzaine","semaine","jour")]
			bilan_carlot@data<-vue_ope_lot
			assign("bilan_carlot",bilan_carlot,envir_stacomi)#assign("bilan_carlot",vue_ope_lot,envir_stacomi)
			if (!silent) funout(gettext("To obtain the table, type : bilan_carlot=get('bilan_carlot',envir_stacomi)\n",domain="R-stacomiR"))
			return(bilan_carlot)
		})


#' Plots of various type for Bilancarlot
#' @param x An object of class Bilan_carlot
#' @param plot.type One of "1","violin plot". Defaut to \code{1} , can also be \code{2} boxplot or 
#' \code{3} points. 
#' @param silent Stops displaying the messages.
#' @param ... Additional arguments, see \code{plot}, \code{plot.default} and \code{par}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases plot.Bilan_carlot plot.bilan_carlot plot.b_carlot
#' @export
setMethod("plot", signature(x = "Bilan_carlot", y = "missing"), definition=function(x, plot.type="1", silent=FALSE){ 
			#bilan_carlot<-b_carlot;require(ggplot2);plot.type="1"
			#browser()
			bilan_carlot<-x
			plot.type<-as.character(plot.type)# to pass also characters
			if (!plot.type%in%c("1","2","3")) stop('plot.type must be 1,2,3')
			if (exists("bilan_carlot",envir_stacomi)) {
				bilan_carlot<-get("bilan_carlot",envir_stacomi)
			} else {      
				if (!silent) funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
			}
			name_param<-bilan_carlot
			if (plot.type==1){		
				g<-ggplot(bilan_carlot@data,aes(x=car_valeur_quantitatif))
				g<-g+stat_density(aes(ymax = ..density..,  ymin = -..density..),
								fill = "grey50", colour = "grey10",
								geom = "ribbon", position = "identity") +
						facet_grid(. ~ annee) +
						coord_flip()
				print(g) 
				assign("g",g,envir_stacomi)
				if (!silent) funout(gettext("To obtain the graphical object, type :  g<-get(\"g\",envir_stacomi)\n",domain="R-stacomiR"))				
			} else if (plot.type==2){
				g<-ggplot(bilan_carlot@data)
				g<-g+geom_boxplot(aes(x=mois,y=car_valeur_quantitatif,fill=std_libelle))+
						facet_grid(annee ~ .)				
				print(g) 
				assign("g",g,envir_stacomi)
				if (!silent) funout(gettext("To obtain the graphical object, type :  g<-get(\"g\",envir_stacomi)\n",domain="R-stacomiR"))
				
			}else if (plot.type==3){
				g<-ggplot(bilan_carlot@data)
				g<-g+geom_point(aes(x=ope_date_debut,y=car_valeur_quantitatif))
				print(g) 
				assign("g",g,envir_stacomi)
				if (!silent) funout(gettext("To obtain the graphical object, type :  g<-get(\"g\",envir_stacomi)\n",domain="R-stacomiR"))
			}
			return(invisible(NULL))
		})
		
#' summary for Bilan_carlot 
#' @param object An object of class \code{\link{Bilan_carlot-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("summary",signature=signature(object="Bilan_carlot"),definition=function(object,silent=FALSE,...){
			Hmisc::describe(object@data)		
		})

#' Method to print the command line of the object
#' @param x An object of class Bilan_carlot
#' @param ... Additional parameters passed to print
#' @return NULL
#' @author cedric.briand
#' @export
setMethod("print",signature=signature("Bilan_carlot"),definition=function(x,...){ 
			sortie1<-"bilan_carlot=new('Bilan_carlot')"
			sortie2<-stringr::str_c("bilan_carlot=choice_c(bilan_carlot,",
					"dc=c(",stringr::str_c(x@dc@dc_selectionne,collapse=","),"),",
					"taxons=c(",stringr::str_c(shQuote(x@taxons@data$tax_nom_latin),collapse=","),"),",
					"stades=c(",stringr::str_c(shQuote(x@stades@data$std_code),collapse=","),"),",	
					"par=c(",stringr::str_c(shQuote(x@par@par_selectionne),collapse=","),"),",	
					"horodatedebut=",shQuote(strftime(x@horodatedebut@horodate,format="%d/%m/%Y %H-%M-%S")),
					",horodatefin=",shQuote(strftime(x@horodatefin@horodate,format="%d/%m/%Y %H-%M-%S")),")")
			# removing backslashes
			funout(sortie1)
			funout(stringr::str_c(sortie2,...))
			return(invisible(NULL))
		})


#' fundensityBilan_carlot uses ggplot2 to draw density plots
#' 
#' assigns an object g in envir_stacomi for eventual modification of the plot
#' @param h A handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
fundensityBilan_carlot = function(h,...) {
	bilan_carlot<-get("bilan_carlot",envir=envir_stacomi)
	bilan_carlot<-charge(bilan_carlot)
	bilan_carlot<-connect(bilan_carlot)
	bilan_carlot<-calcule(bilan_carlot)
	plot(bilan_carlot,plot.type="1")
}

#' Boxplots for ggplot2
#' 
#' assigns an object g in envir_stacomi for eventual modification of the plot
#' @param h A handler passed by the graphical interface
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
funboxplotBilan_carlot = function(h,...) {
	bilan_carlot<-get("bilan_carlot",envir=envir_stacomi)
	bilan_carlot<-charge(bilan_carlot)
	bilan_carlot<-connect(bilan_carlot)
	bilan_carlot<-calcule(bilan_carlot)	
	plot(bilan_carlot,plot.type="2")
}


#' Point graph from ggplot
#' 
#' assigns an object g in envir_stacomi for eventual modification of the plot
#' @param h handler passed by the graphical interface
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
funpointBilan_carlot = function(h,...) {
	bilan_carlot<-get("bilan_carlot",envir=envir_stacomi)
	bilan_carlot<-charge(bilan_carlot)
	bilan_carlot<-connect(bilan_carlot)
	bilan_carlot<-calcule(bilan_carlot)
	plot(bilan_carlot,plot.type="3")
}  

#' table function
#' 
#' funtableBilan_carlot shows a table of results in gdf
#' @param h hanlder passed by the graphical interface
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
funtableBilan_carlot = function(h,...) {
	bilan_carlot<-get("bilan_carlot",envir=envir_stacomi)
	bilan_carlot=charge(bilan_carlot)
	bilan_carlot<-connect(bilan_carlot)
	vue_ope_lot=bilan_carlot@requete@query # on recupere le data.frame
	assign("bilan_carlot",bilan_carlot,envir_stacomi)#assign("bilan_carlot",vue_ope_lot,envir_stacomi)
	funout(gettext("To obtain the table, type : bilan_lot=get('bilan_lot',envir_stacomi)\n",domain="R-stacomiR"))
	vue_ope_lot[is.na(vue_ope_lot)]<-""
	vue_ope_lot$ope_date_debut=as.character(vue_ope_lot$ope_date_debut)
	vue_ope_lot$ope_date_fin=as.character(vue_ope_lot$ope_date_fin)   
	gdf(vue_ope_lot, container=TRUE)
}
