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
#' @include Bilan_carlot.r
#' @note The main difference between this class and \link{Bilan_carlot-class} is that this class allows to
#' select (or not) the samples, and that it handles quantitative and qualitative parameters separately.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanMigrationCar", ...)}.  they are loaded by the interface
#' using interface_BilanMigrationCar function.
#' @slot calcdata A "list" of calculated data, filled in by the calcule method
#' @slot data A \code{data.frame} inherited from \link{Bilan_carlot-class}
#' @slot dc an object of class \link{RefDC-class} inherited from \link{Bilan_carlot-class}
#' @slot taxons An object of class \link{RefTaxon-class} inherited from \link{Bilan_carlot-class}
#' @slot stades An object of class \link{RefStades-class} inherited from \link{Bilan_carlot-class}
#' @slot horodatedebut An object of class \link{RefHorodate-class} inherited from \link{Bilan_carlot-class}
#' @slot horodatefin An object of class \link{RefHorodate-class} inherited from \link{Bilan_carlot-class}
#' @slot par An object of class \link{Refpar-class} inherited from \link{Bilan_carlot-class}
#' @slot echantillon An object of class \link{RefChoix-class}, vector of choice
#' @slot parquan An object of class \link{Refparquan-class}, quantitative parameter 
#' @slot parqual An object of class \link{Refparqual-class}, quanlitative parameter
#' @family Bilan Objects
#' @aliases BilanMigrationCar bilanMigrationCar
#' @note program : default two parameter choice, checking box "none" will allow the program to ignore the parameter
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @concept Bilan Object 
#' @keywords classes
#' @export
setClass(Class="BilanMigrationCar",
		representation=representation(
				echantillon="RefChoix",
				calcdata="list",
				parqual="Refparqual",
				parquan="Refparquan"),
		prototype=list(
				data=list(),
				echantillon=new("RefChoix","listechoice"=c(gettext(c("with","without"),domain="stacomiR")),
						selectedvalue=gettext("with",domain="stacomiR")),
				calcdata<-list(),
				parqual=new("Refparqual"),
				parquan=new("Refparquan")),
		contains="Bilan_carlot")


setValidity("BilanMigrationCar",function(object)
		{
			retValue=""
			rep4<-length(object@taxons)==1
			if (!rep4) retValue=gettext("This bilan should be for just one taxa")
			rep5<-length(object@parqual)==1|length(object@parquan)==1 #au moins un qualitatif ou un quantitatif
			if (!rep5) retValue=gettext("length(object@parqual)==1|length(object@parquan)==1 not TRUE")  
			return(ifelse(rep4&rep5,TRUE,retValue))
		} )


#' command line interface for BilanMigrationCar class
#' @param object An object of class \link{BilanMigrationCar-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,RefDC-method}
#' @param taxons '2220=Salmo salar', can be a vector with several values
#' these should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,RefTaxon-method}
#' @param stades The stages selected, can be a vector with several values
#' @param parquan Quantitative parameter
#' @param parqual Qualitative parameter
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
				parquan=NULL,
				parqual=NULL,
				horodatedebut,
				horodatefin,
				echantillon=gettext("with",domain="R-stacomiR"),
				silent=FALSE){
			# code for debug using example
			#horodatedebut="2012-01-01";horodatefin="2013-12-31";dc=c(107,108,101);taxons=2220;	stades=c('5','11','BEC','BER','IND');parquan=c('1786','1785','C001','A124');parqual='COHO';silent=FALSE
			bmC<-object
			bmC@dc=charge(bmC@dc)
			bmC@dc<-choice_c(object=bmC@dc,dc)
			bmC@taxons<-charge_avec_filtre(object=bmC@taxons,bmC@dc@dc_selectionne)			
			bmC@taxons<-choice_c(bmC@taxons,taxons)
			bmC@stades<-charge_avec_filtre(object=bmC@stades,bmC@dc@dc_selectionne,bmC@taxons@data$tax_code)	
			bmC@stades<-choice_c(bmC@stades,stades,silent=silent)
			bmC@parquan<-charge_avec_filtre(object=bmC@parquan,dc_selectionne=bmC@dc@dc_selectionne,
					taxon_selectionne=bmC@taxons@data$tax_code,
					stade_selectionne=bmC@stades@data$std_code)	
			if (!is.null(parquan))
				bmC@parquan<-choice_c(bmC@parquan,parquan,silent=silent)
			# the method choice_c is written in refpar, and each time 
			assign("refparquan",bmC@parquan,envir_stacomi)
			bmC@parqual<-charge_avec_filtre(object=bmC@parqual,bmC@dc@dc_selectionne,bmC@taxons@data$tax_code,bmC@stades@data$std_code)	
			if (!is.null(parqual)){
				bmC@parqual<-choice_c(bmC@parqual,parqual,silent=silent)
				bmC@parqual<-charge_complement(bmC@parqual)
			}		
			assign("refparqual",bmC@parqual,envir_stacomi)
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
			bmC@echantillon<-charge(bmC@echantillon,vecteur=c(gettext("with",domain="R-stacomiR"),gettext("without",domain="R-stacomiR")),
					label="essai",
					selected=as.integer(1))
			bmC@echantillon<-choice_c(bmC@echantillon,selectedvalue=echantillon)
			validObject(bmC)	
			return(bmC)
		})

#' charge method for BilanMigrationCar
#' 
#' Used by the graphical interface to collect and test objects in the environment envir_stacomi, 
#' fills also the data slot by the connect method
#' @param object An object of class \link{BilanMigrationCar-class}
#' @param silent Default FALSE, if TRUE the program should not display messages
#' @return \link{BilanMigrationCar-class} with slots filled by user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("charge",signature=signature("BilanMigrationCar"),definition=function(object,silent=FALSE){ 
			bmC<-object 
			if (exists("bmC_date_debut",envir_stacomi)) {
				bmC@horodatedebut@horodate<-get("bmC_date_debut",envir_stacomi)
			} else {
				funout(gettext("You need to choose the starting date\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("bmC_date_fin",envir_stacomi)) {
				bmC@horodatefin@horodate<-get("bmC_date_fin",envir_stacomi)
			} else {
				funout(gettext("You need to choose the ending date\n",domain="R-stacomiR"),arret=TRUE)
			}  
			
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
					
			if (exists("refchoice",envir_stacomi)){
				bmC@echantillon<-get("refchoice",envir_stacomi)
			} else 
			{
				bmC@echantillon@listechoice<-"avec"
				bmC@echantillon@selected<-as.integer(1)
			}
			
			if (!(exists("refparquan",envir_stacomi)|exists("refparqual",envir_stacomi))){
				funout(gettext("You need to choose at least one parameter qualitative or quantitative\n",domain="R-stacomiR"),arret=TRUE)	
			}
			
			if (exists("refparquan",envir_stacomi)){
				bmC@parquan<-get("refparquan",envir_stacomi)
			} 
			if (exists("refparqual",envir_stacomi)){
				bmC@parqual<-get("refparqual",envir_stacomi)
			} 
			
			stopifnot(validObject(bmC, test=TRUE))
			return(bmC)
		})

#' connect method for BilanMigrationCar
#' 
#' 
#' uses the BilanMigrationMult method
#' @param object An object of class \link{BilanMigrationCar-class}
#' @param silent Boolean default FALSE, if TRUE information messages not displayed
#' @return BilanMigrationCar with slot @data filled from the database
#' @export
setMethod("connect",signature=signature("BilanMigrationCar"),definition=function(object,silent=FALSE){
			bmC<-object
			if (bmC@echantillon@selectedvalue==bmC@echantillon@listechoice[1]) {
				echantillons=" AND lot_pere IS NULL"      
			} else {
				echantillons=""      
			} 
			# data can be selected but not in the database or the inverse
			parquan<-intersect(bmC@parquan@par_selectionne,bmC@parquan@data$par_code)
			parqual<-intersect(bmC@parqual@par_selectionne,bmC@parqual@data$par_code)
			if (length(parquan)==0 & length(parqual)==0) {
				stop("You need to choose at least one quantitative or qualitative attribute")
			} else {
				if (length(parqual)!=0) {
					#caracteristique qualitative 
					req=new("RequeteODBC")
					req@baseODBC<-get("baseODBC", envir=envir_stacomi)					
					#this query will get characteristics from lot_pere when null
					req@sql=paste("SELECT ",
							" ope_date_debut,", 
							" ope_date_fin,",  
							" lot_methode_obtention,",
							" lot_identifiant ,",
							" lot_effectif,", 
							" car_val_identifiant,", 
							" ope_dic_identifiant,", 
							" lot_tax_code,", 
							" lot_std_code,",
							" car_par_code",
							" FROM ",get("sch",envir=envir_stacomi),"vue_ope_lot_ech_parqual", 
							" WHERE ope_dic_identifiant in ",vector_to_listsql(bmC@dc@dc_selectionne),
							echantillons,
							" AND lot_tax_code in ",vector_to_listsql(bmC@taxons@data$tax_code),
							" AND lot_std_code in ",vector_to_listsql(bmC@stades@data$std_code),
							" AND car_par_code in ",vector_to_listsql(bmC@parqual@data$par_code),
							" AND (ope_date_debut, ope_date_fin) OVERLAPS (TIMESTAMP '" ,
							bmC@horodatedebut@horodate ,
							"', TIMESTAMP '" , bmC@horodatefin@horodate  , "')" 
							,sep="")
					bmC@data[["parqual"]]<-connect(req)@query
				}# end if (parqual)
				if (length(parquan)!=0) {
					# Caracteristique quantitative
					req=new("RequeteODBC")
					req@baseODBC<-get("baseODBC", envir=envir_stacomi)					
					# we round the date to be consistent with daily values from the 
					req@sql=paste("SELECT ",
							" ope_date_debut,", 
							" ope_date_fin,",  
							" lot_methode_obtention,",
							" lot_identifiant ,",
							" lot_effectif,", 	
							" car_valeur_quantitatif,",
							" ope_dic_identifiant,", 
							" lot_tax_code,", 
							" lot_std_code,",
							" car_par_code",
							" FROM ",get("sch",envir=envir_stacomi),"vue_ope_lot_ech_parquan", 
							" WHERE ope_dic_identifiant in ",vector_to_listsql(bmC@dc@dc_selectionne),
							echantillons,
							" AND lot_tax_code in ",vector_to_listsql(bmC@taxons@data$tax_code),
							" AND lot_std_code in ",vector_to_listsql(bmC@stades@data$std_code),
							" AND car_par_code in ",vector_to_listsql(bmC@parquan@data$par_code),
							" AND (ope_date_debut, ope_date_fin) OVERLAPS (TIMESTAMP '" ,
							bmC@horodatedebut@horodate ,
							"', TIMESTAMP '" , bmC@horodatefin@horodate  , "')" 
							,sep="")
					
					bmC@data[["parquan"]]<-connect(req)@query				
				}# end if (parquan)
			}# end else		
			return(bmC)
		})


#' handler for bilanmigrationpar
#' @param h handler
#' @param ... Additional parameters
hbmCcalc=function(h,...){
	bmC<-get("bmC",envir=envir_stacomi)
	bmC<-charge(bmC)
	bmC<-connect(bmC)
	bmC<-calcule(bmC)
	# calcule will assign in envir_stacomi
}			
#' Turns a quantitative parameter into qualitative
#' 
#' @param object An object of class \link{Refparquan-class}
#' @param par The code of a quantitative parameter
#' @param silent Default FALSE, if TRUE the program should not display messages
#' @param ... Additional parms to the cut method \link[base]{cut}   
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("setasqualitative",signature=signature("BilanMigrationCar"),definition=function(object,par,silent=FALSE,...) {
			bmC<-object
			# par <-'A124'
			#========= initial checks ================
			if (class(par)!="character") stop("par should be a character")
			if (nrow(bmC@data[["parquan"]])==0)  funout(gettext("No data for quantitative parameter, perhaps you forgot to run the calcule method"))
			if (!par%in%bmC@parquan@par_selectionne) funout(gettextf("The parameter %s is not in the selected parameters",par),arret=TRUE)
			if (!par%in%bmC@parquan@data$par_code) funout(gettextf("No data for this parameter, nothing to do",par),arret=TRUE)
			#=============================================
			tab<-bmC@data[["parquan"]]
			lignes_du_par<-tab$car_par_code==par
			tab<-tab[lignes_du_par,]
			tab$car_valeur_quantitatif<-cut(tab$car_valeur_quantitatif,...)
			#tab$car_valeur_quantitatif<-cut(tab$car_valeur_quantitatif,breaks=c(0,1.5,2.5,10),label=c("1","2","3"))
			tab<-chnames(tab,"car_valeur_quantitatif","car_val_identifiant")
			bmC@data[["parquan"]]<-bmC@data[["parquan"]][!lignes_du_par,]
			bmC@data[["parqual"]]<-rbind(bmC@data[["parqual"]],tab)
			# Adding the par to parqual
			bmC@parqual@par_selectionne<-c(bmC@parqual@par_selectionne,par)
			# removing from parquan
			bmC@parquan@par_selectionne<-bmC@parquan@par_selectionne[-match(par,bmC@parquan@par_selectionne)]
			# resetting the right values for valqual
			bmC@parqual@valqual<-rbind(bmC@parqual@valqual,
					data.frame(val_identifiant=levels(tab$car_val_identifiant),
							val_qual_code=par,
							val_rang=1:length(levels(tab$car_val_identifiant)),
							val_libelle=NA))
			
			
			if (!silent) funout(gettextf("%s lines have been converted from quantitative to qualitative parameters",nrow(tab)))
			return(bmC)
		})

#' calcule methode
#' 
#' @param object An object of class \code{\link{BilanMigrationCar-class}} 
#' @param silent Boolean default FALSE, if TRUE information messages not displayed

setMethod("calcule",signature=signature("BilanMigrationCar"),definition=function(object,silent=FALSE){ 
			bmC<-object
			qual<-bmC@data[["parqual"]]
			quan<-bmC@data[["parquan"]]
			if (is.null(qual)&is.null(quan)) stop("cannot perform calcule method, no data in either qualitative or quantitative parameters")
			if (!is.null(qual)) qual<-chnames(qual,"car_par_code","car_par_code_qual")
			if (!is.null(quan)) quan<-chnames(quan,"car_par_code","car_par_code_quan")
			if (is.null(qual)) {
				quaa<-quan
				quaa$car_par_code_qual=NA
			} else	if (is.null(quan)) {
				quaa<-qual
				quaa$car_par_code_quan=NA
			} else {
			quaa<-merge(qual,quan,by=c("ope_dic_identifiant","lot_identifiant","ope_date_debut","ope_date_fin","lot_methode_obtention","lot_effectif","lot_tax_code","lot_std_code"),all.x=TRUE,all.y=TRUE)
			}
			quaa=funtraitementdate(data=quaa,nom_coldt="ope_date_debut") 
			quaa<-quaa[order(quaa$ope_dic_identifiant,quaa$lot_tax_code,quaa$lot_std_code,quaa$ope_date_debut),]
			bmC@calcdata<-quaa
			if(!silent) funout(gettext("The calculated data are in slot calcdata"))
			assign("bmC",bmC,envir_stacomi)	
			return(bmC)
		})
#' handler for plot
#' 
#' @param h handler
#' @param ... Additional parameters
hbmCplotquan = function(h,...) {
	if (exists("bmC",envir_stacomi)) {
		bmC<-get("bmC",envir_stacomi)
		plot(bmC,plot.type="quan",silent=FALSE)
	} else {      
		funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
	}
}
#' handler for plot
#' 
#' @param h handler
#' @param ... Additional parameters
hbmCplotqual=function(h,...){
	if (exists("bmC",envir_stacomi)) {
		bmC<-get("bmC",envir_stacomi)
		plot(bmC,plot.type="qual",silent=FALSE)
	} else {      
		funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
	}
}
#' handler for plot
#' 
#' @param h handler
#' @param ... Additional parameters
hbmCplotcrossed=function(h,...){
	if (exists("bmC",envir_stacomi)) {
		bmC<-get("bmC",envir_stacomi)
		plot(bmC,plot.type="crossed",silent=FALSE)
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
#' @param plot.type One of "qual", "quant" "crossed" default to qual
#' @param color_parm A named vector for the colors of either parameters (if plot.type=quant) or levels for
#' parameters (if plot.type=qual).
#' @param silent Boolean default FALSE, if TRUE information messages not displayed
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("plot",signature=signature(x="BilanMigrationCar",y="missing"),definition=function(x,color_parm=NULL,plot.type="qual",silent=FALSE,...){ 
			bmC<-x
			if (nrow(bmC@calcdata)==0) stop("no data in calcdata, have you forgotten to run calculations ?")
			# transformation du tableau de donnees
			# color_parm<-c("age 1"="red","age 2"="blue","age 3"="green")
			# color_parm<-c("C001"="red")
			if (plot.type=="qual") {	
				parlevels<-bmC@parqual@valqual$val_identifiant				
				cs<-colortable(color=color_parm,vec=parlevels,palette="Dark2")
				cs<-stacomirtools::chnames(cs,"name","car_val_identifiant")
				calcdata<-bmC@calcdata
				calcdata<-merge(calcdata,cs)
				g<-ggplot(calcdata)+
						geom_bar(aes(x=mois,y=lot_effectif,fill=color),stat = "identity")+
						xlab(gettext("Month"))+
						ylab(gettext("Number"))+
						scale_fill_identity(name=gettext("Classes"),
								labels=cs[,"car_val_identifiant"],
								breaks=cs[,"color"],
								guide = "legend")+
						theme_bw()
				
				assign("g",g,envir_stacomi)
				if (!silent) funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi) \n",domain="R-stacomiR"))
				print(g)
			} #end plot.type = "qual"
			if (plot.type=="quant") { 
				calcdata<-bmC@calcdata
				the_parms<-unique(calcdata$car_par_code_quan)
				cs<-colortable(color=color_parm,vec=the_parms,palette="Dark2")
				cs<-stacomirtools::chnames(cs,"name","car_par_code_quan")
				calcdata<-merge(calcdata,cs)
				g<-ggplot(calcdata)+
						geom_point(aes(x=ope_date_debut,y=car_valeur_quantitatif,col=color),stat='identity')+
						xlab(gettext("Month"))+
						ylab(gettext("Number"))+
						scale_colour_identity(name=gettext("Param"),
								labels=cs[,"car_par_code_quan"],
								breaks=cs[,"color"],
								guide = "legend")+
						theme_bw()
				assign("g",g,envir_stacomi)
				if (!silent) funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi) \n",domain="R-stacomiR"))
				print(g)
			} #end plot.type="quant"
			if (plot.type=="crossed") { 
				parlevels<-bmC@parqual@valqual$val_identifiant				
				cs<-colortable(color=color_parm,vec=parlevels,palette="Dark2")
				cs<-stacomirtools::chnames(cs,"name","car_val_identifiant")
				calcdata<-bmC@calcdata
				calcdata<-merge(calcdata,cs)
				
				g<-ggplot(calcdata)+
						geom_point(aes(x=ope_date_debut,y=car_valeur_quantitatif,col=color),stat='identity')+
						xlab(gettext("Month"))+
						ylab(gettext("Number"))+
						scale_colour_identity(name=gettext("Param"),
								labels=cs[,"car_val_identifiant"],
								breaks=cs[,"color"],
								guide = "legend")+
						theme_bw()
				assign("g",g,envir_stacomi)
				if (!silent) funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi) \n",domain="R-stacomiR"))
				print(g)
			} #end plot.type="xyplot"
		})


#' summary for BilanMigrationCar 
#' @param object An object of class \code{\link{BilanMigrationCar-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("summary",signature=signature(object="BilanMigrationCar"),definition=function(object,silent=FALSE,...){
			bmC<-object
			bm<-bmC@calcdata
			if (nrow(bm)==0) stop("No data in slot calcdata, did you forget to run the calcule method ?")
			if (length(unique(bm$annee))==1){
				table=round(tapply(bm$lot_effectif,list(bm$mois,bm$car_val_identifiant),sum),1)
				table<-rbind(table,
						colSums(table,na.rm=TRUE))
				rownames(table)[nrow(table)]<-gettext("Sum")
				if (!silent) print(table)
				table<-as.data.frame(table)
			} else 	{
				table=round(tapply(bm$lot_effectif,list(bm$annee,bm$mois,bm$car_val_identifiant),sum),1)
				
				if (!silent) print(ftable(table))
			}
			return(table)
		})


#' xtable funciton for \link{BilanMigrationCar-class}
#' create an xtable objet to be later used by the print.xtable method.
#' @param x, an object of class "BilanMigrationCar"
#' @param caption, see xtable
#' @param label, see xtable
#' @param align, see xtable, overidden if NULL
#' @param digits, see xtable
#' @param display see xtable
#' @param auto see xtable
#' @param ... Additional parameters
#' @export
setMethod("xtable",signature=signature("BilanMigrationCar"),definition=function(x,
				caption=NULL,
				label=NULL,
				align=NULL,...){
			bmC<-x
			dat=bmC@data
			dc=stringr::str_c(bmC@dc@dc_selectionne,collapse=" ")
			tax=stringr::str_c(bmC@taxons@data$tax_code,collapse=" ")
			std=stringr::str_c(bmC@stades@data$std_code,collapse=" ")
		
			dat<-summary(bmC,silent=TRUE)
			if (class(dat)=="data.frame"){	
				xt<-xtable::xtable(dat,...)	
				if (is.null(align)) {
					align<-c("l",rep("r",ncol(dat)))
					align(xt)<-align
				}
				if (is.null(display)) {
					display=c("s",rep("f",ncol(dat)))
					display(xt)<-display
				}
				if (is.null(caption)) {
					caption=gettextf("Summary for dc %s, taxa %s, stage %s.",dc,tax,std)
					caption(xt)<-caption
				}		
				return(xt)
			} else {
				# this comes from MIfuns not avalaible on CRAN
				ftable2data.frame<-function (x, ...) 
				{
					y <- format(x, quote = FALSE)
					z <- data.frame(y[-1, ], stringsAsFactors = FALSE)
					names(z) <- y[1, ]
					z
				}
				xt<-xtable::xtable(ftable2data.frame(ftable(dat)),...)
				if (is.null(align)) {
					align<-c("l",rep("r",ncol(dat)))
					align(xt)<-align
				}
				if (is.null(display)) {
					display=c("s",rep("f",ncol(dat)))
					display(xt)<-display
				}
				if (is.null(caption)) {
					caption=gettextf("Summary for dc %s, taxa %s, stage %s.",dc,tax,std)
					caption(xt)<-caption
				}		
				return(xt)
			}
		})

