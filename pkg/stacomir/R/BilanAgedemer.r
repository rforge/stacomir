# todo interface modifier, refaire les commentaires, tester avec testthat les methodes writedatabase et supprime


#' Class "BilanAgedemer"
#' 
#' the BilanAgedemer class is used to dispatch adult salmons to age class according to
#' their size and to basin dependent limits set by the user. Once checked with graphs and summary
#' statistics, the results are to be written to the database.
#' @include create_generic.r
#' @include ReftextBox.r
#' @include RefDC.r
#' @include RefTaxon.r
#' @include RefStades.r
#' @include RefHorodate.r
#' @include Refpar.r
#' @note This class is displayed by interface_bilan_agedemer
#' @slot data A data frame with data generated from the database
#' @slot calcdata A list of dc with processed data. This lists consists of two elements
#' \itemize{
#' \item (1) data A dataset with age set to be used by the plot and summary methods
#' \item (2) tj_caracteristitiquelot_car A dataset to import into the database
#' }
#' @slot dc Object of class \link{RefDC-class}: the control devices
#' @slot taxons Object of class \link{RefTaxon-class}: the species
#' @slot stades Object of class \link{RefStades-class} : the stages of the fish
#' @slot par Object of class \link{Refpar-class}: the parameters used
#' @slot horodatedebut An object of class \code{RefHorodate-class}
#' @slot horodatefin An object of class \code{RefHorodate-class}
#' @slot limit1hm The size limit, in mm between 1 sea winter fishes and 2 sea winter fishes
#' @slot limit2hm The size limit, in mm between 2 sea winter fishes and 3 sea winter fishes
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanAgedemer", ...)}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @family Bilan Objects
#' @keywords classes
#' @example inst/examples/bilanAgedemer_example.R
#' @aliases BilanAgedemer bilA bilanagedemer bilanAgedeMer BilanAgeDeMer bilan_adm
#' @export 
setClass(Class="BilanAgedemer",
		representation= representation(
				data="data.frame",
				calcdata="list",
				dc="RefDC",
				taxons="RefTaxon",
				stades="RefStades",
				par="Refpar",
				horodatedebut="RefHorodate",
				horodatefin="RefHorodate",
				limit1hm="RefTextBox",
				limit2hm="RefTextBox"
		),
		prototype=prototype(data=data.frame(),
				calcdata=list(),
				dc=new("RefDC"),
				taxons=new("RefTaxon"),
				stades=new("RefStades"),
				par=new("Refpar"),
				horodatedebut=new("RefHorodate"),
				horodatefin=new("RefHorodate"),
				limit1hm=new("RefTextBox"),
				limit2hm=new("RefTextBox")
		))
setValidity("BilanAgedemer",function(object)
		{
			rep1=object@taxons@data$tax_code[1]=='2220'
			label1<-'BilanAgedemer should only be for salmon (tax_code=2220)'
			rep2=all(object@stades@data$std_code%in%c('5','11','BEC','BER','IND'))
			label2<-'Only stages 5,11,BEC,BER,IND should be used in BilanAgedemer'
			return(ifelse(rep1 & rep2 , TRUE ,c(label1,label2)[!c(rep1, rep2)]))
		}   
)
#' connect method for BilanAgedemer
#' 
#' @param object An object of class \link{BilanAgedemer-class}
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @return An object of class \link{BilanAgedemer-class} 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("connect",signature=signature("BilanAgedemer"),definition=function(object,silent=FALSE) {
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
			if (!silent) funout(gettext("Data loaded",domain="R-stacomiR"))
			return(object)
		})


#' charge method for BilanAgedemer class
#' 
#' this method verifies that boxes have been clicked in the user interface and gets the objects pasted in 
#' envir_stacomi
#' @param object An object of class \link{BilanAgedemer-class} 
#' @param h a handler
#' @return An object of class \link{BilanAgedemer-class} with slots filled with user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("charge",signature=signature("BilanAgedemer"),definition=function(object,h) {
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
			if (exists("bilan_adm_date_debut",envir_stacomi)) {
				object@horodatedebut@horodate<-get("bilan_adm_date_debut",envir_stacomi)
			} else {
				funout(gettext("You need to choose the starting date\n",domain="R-stacomiR"),arret=TRUE)
			}
			# rem id
			if (exists("bilan_adm_date_fin",envir_stacomi)) {
				object@horodatefin@horodate<-get("bilan_adm_date_fin",envir_stacomi)
			} else {
				funout(gettext("You need to choose the ending date\n",domain="R-stacomiR"),arret=TRUE)
			}   
			if (exists("limit1hm",envir_stacomi)) {
				object@limit1hm<-get("limit1hm",envir_stacomi)
			} else {
				funout(gettext("you need to choose a value for limit1hm",domain="R-stacomiR"),arret=TRUE)
			} 
			if (exists("limit2hm",envir_stacomi)) {
				object@limit2hm<-get("limit2hm",envir_stacomi)
			} else {
				funout(gettext("you need to choose a value for limit2hm",domain="R-stacomiR"),arret=TRUE)
			} 
			
			return(object)
			validObject(object)
			assign("bilan_adm",object,envir_stacomi)
		})


#' command line interface for BilanAgedemer class
#' @param object An object of class \link{BilanAgedemer-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,RefDC-method}
#' @param taxons '2220=Salmo salar',
#' these should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,RefTaxon-method}
#' @param stades '5','11','BEC','BER','IND'
#' @param par Parameters chosen for the Bilan are mesured body size (1786), mesured fork length (1785),video size (C001) and number of year at sea (A124)
#' @param horodatedebut The starting date as a character, formats like \code{\%Y-\%m-\%d} or \code{\%d-\%m-\%Y} can be used as input
#' @param horodatefin The finishing date of the Bilan, for this class this will be used to calculate the number of daily steps.
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @return An object of class \link{BilanAgedemer-class}
#' The choice_c method fills in the data slot for classes \link{RefDC-class}, \link{RefTaxon-class}, \link{RefStades-class}, \link{Refpar-class} and two slots of \link{RefHorodate-class} and then 
#' uses the choice_c methods of these object to select the data.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("choice_c",signature=signature("BilanAgedemer"),definition=function(object,
				dc,
				taxons=2220,
				stades=c('5','11','BEC','BER','IND'),
				par=c('1786','1785','C001','A124'),
				horodatedebut,
				horodatefin,
				limit1hm,
				limit2hm,
				silent=FALSE){
			# code for debug using example
			#horodatedebut="2012-01-01";horodatefin="2013-12-31";dc=c(107,108,101);
			#taxons=2220;	stades=c('5','11','BEC','BER','IND');par=c('1786','1785','C001');silent=FALSE
			if (!(is.numeric(limit1hm)|is.integer(limit1hm))) funout(gettext("limit1hm should be numeric or integer",domain="R-stacomiR"),arret=TRUE)
			if (!(is.numeric(limit2hm)|is.integer(limit2hm))) funout(gettext("limit2hm should be numeric or integer",domain="R-stacomiR"),arret=TRUE)
			
			bilan_adm<-object
			bilan_adm@dc=charge(bilan_adm@dc)
			# loads and verifies the dc
			# this will set dc_selectionne slot
			bilan_adm@dc<-choice_c(object=bilan_adm@dc,dc)
			# only taxa present in the bilanMigration are used
			bilan_adm@taxons<-charge_avec_filtre(object=bilan_adm@taxons,bilan_adm@dc@dc_selectionne)			
			bilan_adm@taxons<-choice_c(bilan_adm@taxons,taxons)
			bilan_adm@stades<-charge_avec_filtre(object=bilan_adm@stades,bilan_adm@dc@dc_selectionne,bilan_adm@taxons@data$tax_code)	
			bilan_adm@stades<-choice_c(bilan_adm@stades,stades,silent=silent)
			bilan_adm@par<-charge_avec_filtre(object=bilan_adm@par,bilan_adm@dc@dc_selectionne,bilan_adm@taxons@data$tax_code,bilan_adm@stades@data$std_code)	
			bilan_adm@par<-choice_c(bilan_adm@par,par,silent=silent)
			bilan_adm@horodatedebut<-choice_c(object=bilan_adm@horodatedebut,
					nomassign="bilan_adm_date_debut",
					funoutlabel=gettext("Beginning date has been chosen\n",domain="R-stacomiR"),
					horodate=horodatedebut, 
					silent=silent)
			bilan_adm@horodatefin<-choice_c(bilan_adm@horodatefin,
					nomassign="bilan_adm_date_fin",
					funoutlabel=gettext("Ending date has been chosen\n",domain="R-stacomiR"),
					horodate=horodatefin,
					silent=silent)
			bilan_adm@limit1hm<-choice_c(bilan_adm@limit1hm,as.character(limit1hm),"limit1hm")
			bilan_adm@limit2hm<-choice_c(bilan_adm@limit2hm,as.character(limit2hm),"limit2hm")
			validObject(bilan_adm)
			return(bilan_adm)
		})

#' Calcule method for BilanAgedemer, this method will split the data according to cut
#' 
#' @param object An object of class \code{\link{BilanAgedemer-class}} 
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @return An object of class \link{BilanAgedemer-class}
#' @export
setMethod("calcule",signature=signature("BilanAgedemer"),definition=function(object,silent) {
			#bilan_adm<-b_carlot
			bilan_adm<-object
			if(nrow(bilan_adm@data)==0) {
				funout(gettext("you are in deep shit",domain="R-stacomiR"), arret=TRUE)
			}   
			adm=bilan_adm@data # we get the data.frame
			# the age already present in the database don't interest us there
			adm=adm[adm$car_par_code!='A124',]
			if (is.na(as.numeric(bilan_adm@limit1hm@label))) stop("internal error")
			# if no value, a dummy value of 2m
			if (is.na(as.numeric(bilan_adm@limit2hm@label))) bilan_adm@limit2hm@label<-2000
			lescoupes<-c(0,as.numeric(bilan_adm@limit1hm@label),as.numeric(bilan_adm@limit2hm@label),2001)
			adm$age<-cut(x=adm$car_valeur_quantitatif,breaks=lescoupes,labels=FALSE)
			bilan_adm@calcdata[["data"]]<-adm
			
			
			assign("bilan_adm",bilan_adm,envir_stacomi)
			return(bilan_adm)
		})


#' Plots of various type for BilanAgedemer
#' 
#' @param x An object of class \link{BilanAgedemer-class}
#' @param plot.type Default "1"
#'  \itemize{
#' 		\item{plot.type="1"}{density plot by sea age}
#' 		\item{plot.type="2"}{Density plot by sea age and dc}
#' }
#' @param silent Default FALSE, if TRUE the program should no display messages.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases plot.BilanAgedemer plot.bilan_adm
#' @export
setMethod("plot", signature(x = "BilanAgedemer", y = "missing"), definition=function(x, plot.type="1", silent=FALSE){ 
			#require(ggplot2);plot.type="1"
			#browser()
			bilan_adm<-x
			plot.type<-as.character(plot.type)# to pass also characters
			if (!plot.type%in%c("1","2")) stop('plot.type must be 1,2')
			if (nrow(bilan_adm@calcdata[["data"]])==0) {   
				if (!silent) funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
			} 
			dat<-bilan_adm@calcdata[["data"]]
			# cols are using viridis::inferno(6,alpha=0.9)
			les_coupes=as.numeric(c(bilan_adm@limit1hm@label,bilan_adm@limit2hm@label))
			
			
			#################################################
			# plot.type =1 density plot
			#################################################
			
			if (plot.type=="1"){		
				
				p<-ggplot(dat)+geom_histogram(aes(x=car_valeur_quantitatif,fill=factor(age)),binwidth=10,alpha=0.8)+
						geom_vline(xintercept=les_coupes,lty=2,lwd=1)+
						annotate("text",x=les_coupes,y=0,label=les_coupes,vjust=1,hjust=-0.2)+
						theme_minimal()+
						scale_fill_manual("Age",values=c("1"="#379ec6","2"="#173957","3"="#b09953"))+
						xlab("Size in mm")+
						ylab("Effectif")
				print(p)
				assign("p",p,envir=envir_stacomi)
				funout(gettext("The graphical object is written is env_stacomi, type p<-get('p',envir=envir_stacomi)",domain="R-stacomiR"))
				
			}
			######################################
			# Migration according to stage, month and year
			######################################
			# todo see of anotation is possible
			if (plot.type=="2"){					
				p<-ggplot(dat)+geom_histogram(aes(x=car_valeur_quantitatif,fill=factor(age)),binwidth=10,alpha=0.8)+
						geom_vline(xintercept=les_coupes,lty=2,lwd=1)+
						theme_minimal()+
						scale_fill_manual("Age",values=c("1"="#379ec6","2"="#173957","3"="#b09953"))+
						xlab("Size in mm")+
						ylab("Effectif")+
						facet_grid(ope_dic_identifiant~.)
				print(p)
				assign("p",p,envir=envir_stacomi)
				funout(gettext("The graphical object is written is env_stacomi, type p<-get('p',envir=envir_stacomi)",domain="R-stacomiR"))
			}
			
		})

#' summary for BilanAgedemer 
#' @param object An object of class \link{BilanAgedemer-class}
#' @param silent Default FALSE, if TRUE the program should no display messages.
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("summary",signature=signature(object="BilanAgedemer"),definition=function(object,silent=FALSE,...){
			bilan_adm<-object
			dat<-bilan_adm@calcdata[["data"]]	
			if (nrow(dat)==0) {   
				if (!silent) funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
			} 
			ndc=unique(dat$ope_dic_identifiant)
			result<-list()
			for (i in 1:length(ndc)){
				datdc<-	dat[dat$ope_dic_identifiant==ndc[i],]
				dc_code<-bilan_adm@dc@data$dc_code[bilan_adm@dc@data$dc==ndc[i]]
				ouvrage<-
						gsub("[\r\n]", "", bilan_adm@dc@data[bilan_adm@dc@data$dc==bilan_adm@dc@dc_selectionne[i],"ouv_libelle"])
				dc<-as.character(unique(datdc$ope_dic_identifiant))
				result[[dc]]<-list()
				result[[dc]][["ouvrage"]]<-ouvrage
				print(noquote(stringr::str_c("Age Statistics for dam : ",ouvrage," CD=",dc_code)))
				print(noquote("========================"))
				print(table(datdc$age))		
				result[[dc]][["age"]]<-table(datdc$age)
				
			}
			if (length(ndc)>1){
				print(noquote(stringr::str_c("Age Statistics total")))
				print(noquote("========================"))
				print(table(dat$age))		
				
			}
			return(result)		
		})

#' Command line method to write the caracteristic "sea age" (car_par_code='A124')
#' into the tj_caracteristiquelot_car table in the user's scheme
#' 
#' The sea age caracteristic is calculated from the mesured or calculated size of salmon and with a size/age rule
#' defined by the user  
#' @param object an object of class \link{BilanAgedemer-class}}
#' @param silent : Default FALSE, if TRUE the program should no display messages.
#' @param dbname : the name of the database, defaults to "bd_contmig_nat"
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#' stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE) 
#' data("bilan_adm")
#' bilan_adm<-calcule(bilan_adm)
#' write_database(bilanAgedemer=bilan_adm,silent=FALSE)
#' }
#' @export
setMethod("write_database",signature=signature("BilanAgedemer"),definition=function(object,silent=TRUE,dbname="bd_contmig_nat"){
			# dbname="bd_contmig_nat"
			bilan_adm<-object
			host=get("sqldf.options",envir=envir_stacomi)["sqldf.host"]
			port=get("sqldf.options",envir=envir_stacomi)["sqldf.port"]		
			calcdata<-bilan_adm@calcdata[["data"]]	
			data_in_base<-bilan_adm@data
			if (nrow(calcdata)==0) {   
				if (!silent) funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
			} 
			if (class(bilan_adm)!="BilanAgedemer") stop("the bilan_adm should be of class BilanAgedemer")
			if (class(silent)!="logical") stop("the silent argument should be a logical")
			data_in_base<-data_in_base[data_in_base$car_par_code=='A124',]
			if (nrow(data_in_base)>0){
				supprime(bilan_adm,silent=silent)						
			} 
			#--------------
			# creating the table to import
			#--------------
			code_parametre_age='A124'	
			code_methode_obtention="CALCULE"
			comment=gettextf("Age calculated from the size of fish compared to reference value %s for the limit between 1 sea winter and 2 sea winter fish, and %s for the limit between 2 sea winter fish and 3 sea winter fish",bilan_adm@limit1hm@label,bilan_adm@limit2hm@label)
			bam=data.frame(
					bilan_adm@calcdata$data$lot_identifiant, 
					code_parametre_age,
					code_methode_obtention,
					as.integer(NA),
					bilan_adm@calcdata$data$age,
					as.integer(NA),
					comment,
					substr(toupper(get("sch",envir=envir_stacomi)),1,nchar(toupper(get("sch",envir=envir_stacomi)))-1)
			)
			#--------------
			# writing the table in the database
			#--------------
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			sql<-stringr::str_c("INSERT INTO ",get("sch",envir=envir_stacomi),
					"tj_caracteristiquelot_car 	SELECT * FROM  bam;")
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
				funout(gettextf("Writing  %s age values in the database \n",nrow(bam)))
			}
		})		

#' Method to print the command line of the object
#' @param x An object of class BilanAgedemer
#' @param ... Additional parameters passed to print
#' @return NULL
#' @author cedric.briand
#' @export
setMethod("print",signature=signature("BilanAgedemer"),definition=function(x,...){ 
			sortie1<-"bilan_adm=new('BilanAgedemer')"
			sortie2<-stringr::str_c("bilan_adm=choice_c(bilan_adm,",
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


#' funplotBilanAgedemer 
#' 
#' assigns an object g in envir_stacomi for eventual modification of the plot
#' @param h A handler, with action 1,2,3 or 4 
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
funplotBilanAgedemer = function(h,...) {
	bilan_adm<-get(x="bilan_adm",envir=envir_stacomi)
	bilan_adm<-charge(bilan_adm)
	bilan_adm<-connect(bilan_adm)
	bilan_adm<-calcule(bilan_adm)
	#plot.type is determined by button in h$action
	the_plot<-plot(bilan_adm,plot.type=h$action)
	print(the_plot)
}


#' table function
#' 
#' funtableBilanAgedemer shows a table of results in gdf
#' @param h hanlder passed by the graphical interface
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
funtableBilanAgedemer = function(h,...) {
	bilan_adm=charge(bilan_adm)
	bilan_adm<-connect(bilan_adm)
	bilan_adm<-calcule(bilan_adm)
	bilan_adm<-print(bilan_adm)
}



#' supprime method for BilanMigrationInterannuelle class
#' @param object An object of class \link{BilanAgedemer-class}
#' @return nothing
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("supprime",signature=signature("BilanAgedemer"),
		definition=function(object)
		{ 
			bilan_adm<-object
			data_in_base<-bilan_adm@data
			data_in_base<-data_in_base[data_in_base$car_par_code=='A124',]
			if (nrow(data_in_base)==0) funout(gettext("No data to remove"),arret=TRUE)
		
			requete=new("RequeteODBCwhere")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@select=stringr::str_c("DELETE from ",get("sch",envir=envir_stacomi),"tj_caracteristiquelot_car ")
			requete@where=paste("WHERE car_lot_identifiant IN ",
					vector_to_listsql(data_in_base$lot_identifiant),
					" AND car_par_code='A124';",
					sep="")
			invisible(utils::capture.output(requete<-stacomirtools::connect(requete)))			
			return(invisible(NULL))
		}

)