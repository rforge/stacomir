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
#' @note The main difference between this class and \link{Bilan_carlot} is that this class allows to
#' select (or not) the samples, and that it handles quantitative and qualitative parameters separately.
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
				calcdata="list",
				parqual="Refparqual",
				parquan="Refparquan"),
		prototype=list(
				data=list(),
				echantillon=new("RefChoix"),
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
				parquan,
				parqual,
				horodatedebut,
				horodatefin,
				echantillon=TRUE,
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
			bmC@parquan<-choice_c(bmC@parquan,parquan,silent=silent)
			# the method choice_c is written in refpar, and each time 
			assign("refparquan",bmC@parquan,envir_stacomi)
			bmC@parqual<-charge_avec_filtre(object=bmC@parqual,bmC@dc@dc_selectionne,bmC@taxons@data$tax_code,bmC@stades@data$std_code)	
			bmC@parqual<-choice_c(bmC@parqual,parqual,silent=silent)
			bmC@parqual<-charge_complement(bmC@parqual)
			# the method choice_c is written in refpar, and each time 
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
			bmC@echantillon<-charge(bmC@echantillon,vecteur=c(TRUE,FALSE),label="essai",selected=as.integer(1))
			bmC@echantillon<-choice_c(bmC@echantillon,selectedvalue=echantillon)
			validObject(bmC)	
			return(bmC)
		})

#' charge method for BilanMigrationCar
#' 
#' Used by the graphical interface to collect and test objects in the environment envir_stacomi, 
#' fills also the data slot by the connect method
#' @param object An object of class \link{BilanMigrationMult-class}
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @return \link{BilanMigrationCar-class} with slots filled by user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
		setMethod("charge",signature=signature("BilanMigrationCar"),definition=function(object,silent=FALSE){ 
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


setMethod("connect",signature=signature("BilanMigrationCar"),definition=function(object,silent=FALSE){
			bmC<-object
			if (!bmC@echantillon@selectedvalue) {
				echantillons=" AND lot_pere IS NULL"      
			} else {
				echantillons=""      
			} 
			
			
			if (nrow(bmC@parquan@data)==0 & nrow(bmC@parqual@data)==0) {
				stop("You need to choose at least one quantitative or qualitative attribute")
			} else {
				if (nrow(bmC@parqual@data)!=0) {
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
				if (nrow(bmC@parquan@data)!=0) {
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
	calcule(h$action)
}			
#' Turns a quantitative parameter into qualitative
#' @param object An object of class \link{Refparquan-class}
#' @param par The code of a quantitative parameter
#' @param ... Additional parms to the cut method \link[base]{cut}   
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("setasqualitative",signature=signature("BilanMigrationCar"),definition=function(object,par,silent=FALSE,...) {
			bmC<-object
			# par <-'A124'
		
			if (class(par)!="character") stop("par should be a character")
			if (nrow(bmC@data[["parquan"]])==0)  funout(gettext("No data for quantitative parameter, perhaps you forgot to run the calcule method"))
			if (!par%in%bmC@parquan@par_selectionne) funout(gettextf("The parameter %s is not in the selected parameters",par),arret=TRUE)
			if (!par%in%bmC@parquan@data$par_code) funout(gettextf("No data for this parameter, nothing to do",par),arret=TRUE)
			tab<-bmC@data[["parquan"]]
			lignes_du_par<-tab$car_par_code==par
			tab<-tab[lignes_du_par,]
			tab$car_valeur_quantitatif<-as.character(cut(tab$car_valeur_quantitatif,...))
			#tab$car_valeur_quantitatif<-as.character(cut(tab$car_valeur_quantitatif,breaks=c(0,1.5,2.5,10),label=c("1","2","3")))
			tab<-chnames(tab,"car_valeur_quantitatif","car_val_identifiant")
			bmC@data[["parquan"]]<-bmC@data[["parquan"]][!lignes_du_par,]
			bmC@data[["parqual"]]<-rbind(bmC@data[["parqual"]],tab)
			if (!silent) funout(gettextf("%s lines have been converted from quantitative to qualitative parameters",nrow(tab)))
			return(bmC)
		})

#' calcule methode
#' 
#' 
#'@param object An object of class \code{\link{BilanMigrationCar-class}} 
setMethod("calcule",signature=signature("BilanMigrationCar"),definition=function(object,silent=FALSE){ 
			bmC<-object
			qual<-bmC@data[["parqual"]]
			quan<-bmC@data[["parquan"]]
			qual<-chnames(qual,"car_par_code","car_par_code_qual")
			quan<-chnames(quan,"car_par_code","car_par_code_quan")
			quaa<-merge(qual,quan,by=c("ope_dic_identifiant","lot_identifiant","ope_date_debut","ope_date_fin","lot_methode_obtention","lot_effectif","lot_tax_code","lot_std_code"),all.x=TRUE,all.y=TRUE)
			quaa=funtraitementdate(data=quaa,nom_coldt="ope_date_debut") 
			quaa<-quaa[order(quaa$ope_dic_identifiant,quaa$lot_tax_code,quaa$lot_std_code,quaa$ope_date_debut),]
			bmC@calcdata<-quaa
			if(!silent) funout(gettext("The calculated data are in slot calcdata"))
 			assign("bmC",bmC,envir_stacomi)	
			return(bmC)
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
#' @param plot.type One of "qual", "quant" "crossed"
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("plot",signature=signature(x="BilanMigrationCar",y="missing"),definition=function(x,plot.type="barplot",...){ 
			bmC<-object
			# transformation du tableau de donnees
			bm
			if (plot.type=="qual") {				
				g<-ggplot(bmC@calcdata)
				g<-g+geom_bar(aes(x=mois,y=lot_effectif,fill=car_val_identifiant),stat = "identity")
				g<-g+xlab()
				assign("g",g,envir_stacomi)
				funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi) \n",domain="R-stacomiR"))
				print(g)
			} #end plot.type = "qual"
			if (plot.type=="quant") { 				
				g<-ggplot(bmC@calcdata)
				g<-g+geom_point(aes(x=ope_date_debut,y=car_valeur_quantitatif,col=car_par_code_quan),stat='identity')
				assign("g",g,envir_stacomi)
				funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi) \n",domain="R-stacomiR"))
				print(g)
			} #end plot.type="quant"
			if (plot.type=="crossed") { 				
				g<-ggplot(bmC@calcdata)
				g<-g+geom_point(aes(x=ope_date_debut,y=car_valeur_quantitatif,col=car_val_identifiant),stat='identity')
				assign("g",g,envir_stacomi)
				funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi) \n",domain="R-stacomiR"))
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
