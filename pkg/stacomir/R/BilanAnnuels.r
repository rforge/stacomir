#' Class "BilanAnnuels"
#' @include RefDC.r
#' @include RefTaxon.r
#' @include RefStades.r
#' @include RefAnnee.r
#' @slot dc Object of class \code{\link{RefDC-class}}, the counting device, multiple values allowed
#' @slot data Object of class \code{"data.frame"} data for bilan lot
#' @slot taxons An object of class \code{\link{RefTaxon-class}}, multiple values allowed
#' @slot stades An object of class \code{\link{RefStades-class}}, multiple values allowed
#' @slot anneedebut Object of class \code{\link{RefAnnee-class}}. RefAnnee allows to choose year of beginning
#' @slot anneefin Object of class \code{\link{RefAnnee-class}}
#' RefAnnee allows to choose last year of the Bilan
#' 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @family Bilan Objects
#' @keywords classes
#' @example inst/examples/bilanAnnuels_example.R
#' @export
setClass(Class="BilanAnnuels",representation=
				representation(
						dc="RefDC",
						taxons="RefTaxon",
						stades="RefStades",
						data="data.frame",
						anneedebut="RefAnnee",
						anneefin="RefAnnee"
				),
		prototype=prototype(dc=new("RefDC"),
				taxons=new("RefTaxon"),
				stades=new("RefStades"),
				data=data.frame(),
				anneedebut=new("RefAnnee"),
				anneefin=new("RefAnnee")
		)
)

#' charge method for BilanAnnuels class
#' @param object An object of class \link{BilanAnnuels-class}
#' @param silent Stops messages from being displayed if silent=TRUE, default FALSE 
setMethod("charge",signature=signature("BilanAnnuels"),
		definition=function(object,silent=FALSE){
			bilA<-object
			if (exists("refDC",envir_stacomi)) {
				bilA@dc<-get("refDC",envir_stacomi)
			} else {
				funout(gettext("You need to choose a counting device, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("refTaxon",envir_stacomi)) {
				bilA@taxons<-get("refTaxon",envir_stacomi)
			} else {      
				funout(gettext("You need to choose a taxa, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("refStades",envir_stacomi)){
				bilA@stades<-get("refStades",envir_stacomi)
			} else 
			{
				funout(gettext("You need to choose a stage, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("anneedebut",envir_stacomi)) {
				bilA@anneedebut<-get("anneedebut",envir_stacomi)
			} else {
				funout(gettext("You need to choose the starting year\n",domain="R-stacomiR"),arret=TRUE)
			}  	
			if (exists("anneefin",envir_stacomi)) {
				bilA@anneefin<-get("anneefin",envir_stacomi)
			} else {
				funout(gettext("You need to choose the ending year\n",domain="R-stacomiR"),arret=TRUE)
			}
			assign("bilanAnnuels",bilA,envir_stacomi)
			funout(gettext("The object BilanAnnuels is stored in the stacomi environment, type bilA <-get('bilanAnnuels',envir_stacomi)",domain="R-stacomiR"))
			return(bilA)
			
			
		})


#' connect method for BilanAnnuels class
#' this method performs the sum over the year attention this function does
#' not count subsamples.
#' @param object An object of class \link{BilanAnnuels-class}
#' @param silent Stops messages from being displayed if silent=TRUE, default FALSE
#' @return An instantianted object with values filled with user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @return A dataframe with column effectif, comprising the sum of bilanMigration counts
#' @export
setMethod("connect",signature=signature("BilanAnnuels"),
		definition=function(object,silent=FALSE)
		{ 
			bilA<-object
			req=new("RequeteODBC")
			req@baseODBC<-get("baseODBC", envir=envir_stacomi)
			##############################			
			##############################"  
			anneedebut=	bilA@anneedebut@annee_selectionnee
			anneefin=bilA@anneefin@annee_selectionnee
			dc = vector_to_listsql(bilA@dc@dc_selectionne)
			tax=vector_to_listsql(bilA@taxons@data$tax_code)
			std=vector_to_listsql(bilA@stades@data$std_code)
			req@sql = paste(" select sum(lot_effectif) as effectif, annee, ope_dic_identifiant,lot_tax_code, lot_std_code  from 
							(select *, extract(year  from ope_date_debut) as annee FROM ",get("sch",envir=envir_stacomi),"t_operation_ope ",
					" join ",get("sch",envir=envir_stacomi),"t_lot_lot on lot_ope_identifiant=ope_identifiant where ope_dic_identifiant in",dc,
					" and extract(year from ope_date_debut)>=", anneedebut,
					" and extract(year from ope_date_fin)<=", anneefin,	
					" and ope_dic_identifiant in ", dc,
					" and lot_tax_code in ", tax,
					" and lot_std_code in ",std,
					" and lot_lot_identifiant is null) as tmp",
					" group by annee, ope_dic_identifiant, lot_tax_code, lot_std_code ",
					" order by ope_dic_identifiant, annee, lot_tax_code, lot_std_code; ",sep="" )
			req@sql<-stringr::str_replace_all(req@sql,"[\r\n\t]" , "")
			req<-stacomirtools::connect(req)
			bilA@data=req@query			
			return(bilA)
		})

#' command line interface for \link{BilanAnnuels-class}
#' @param object An object of class \link{BilanAnnuels-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,RefDC-method}
#' @param taxons Either a species name in latin or the SANDRE code for species (ie 2038=Anguilla anguilla),
#' it should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,RefTaxon-method}
#' @param stades A stage code matching the ref.tr_stadedeveloppement_std table in the stacomi database, see \link{choice_c,RefStades-method}
#' @param anneedebut The starting the first year, passed as charcter or integer
#' @param anneefin the finishing year
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return An object of class \link{BilanAnnuels-class}
#' The choice_c method fills in the data slot for classes \link{RefDC-class}, \link{RefTaxon-class}, \link{RefStades-class} and two slots of \link{RefAnnee-class}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("choice_c",signature=signature("BilanAnnuels"),definition=function(object,
				dc,
				taxons,
				stades,			
				anneedebut,
				anneefin,
				silent=FALSE){
			# code for debug using example
			#dc=c(5,6);taxons="Anguilla anguilla";stades=c("AGJ","AGG","CIV");anneedebut="1996";anneefin="2016"
			bilA<-object
			bilA@dc=charge(bilA@dc)
			# loads and verifies the dc
			# this will set dc_selectionne slot
			bilA@dc<-choice_c(object=bilA@dc,dc)
			# only taxa present in the bilanMigration are used
			bilA@taxons<-charge_avec_filtre(object=bilA@taxons,bilA@dc@dc_selectionne)			
			bilA@taxons<-choice_c(bilA@taxons,taxons)
			bilA@stades<-charge_avec_filtre(object=bilA@stades,bilA@dc@dc_selectionne,bilA@taxons@data$tax_code)	
			bilA@stades<-choice_c(bilA@stades,stades)
			
			bilA@anneedebut<-charge(object=bilA@anneedebut,
					objectBilan="BilanAnnuels")
			bilA@anneedebut<-choice_c(object=bilA@anneedebut,
					nomassign="anneeDebut",
					annee=anneedebut, 
					silent=silent)
			bilA@anneefin@data<-bilA@anneedebut@data
			bilA@anneefin<-choice_c(object=bilA@anneefin,
					nomassign="anneeFin",
					annee=anneefin, 
					silent=silent)
			assign("bilanAnnuels",bilA,envir=envir_stacomi)
			return(bilA)
		})

#' xtable funciton for \link{BilanAnnuels-class}
#' create an xtable objet but also assigns an add.to.column argument in envir_stacomi,
#' for later use by the print.xtable method.
#' @param x, an object of class "BilanAnnuels"
#' @param caption, see xtable
#' @param label, see xtable
#' @param align, see xtable, overidden if NULL
#' @param digits default 0
#' @param display see xtable
#' @param auto see xtable
#' @param dc_name A string indicating the names of the DC, in the order of  x@dc@dc_selectionne
#' if not provided DC codes are used.
#' @param tax_name A string indicating the names of the taxa, if not provided latin names are used
#' @param std_name A string indicating the stages names, if not provided then std_libelle are used
#' @export
setMethod("xtable",signature=signature("BilanAnnuels"),definition=function(x,
				caption=NULL,
				label=NULL,
				align=NULL,
				digits=0,
				display=NULL,
				auto=FALSE,
				dc_name=NULL,
				tax_name=NULL,
				std_name=NULL
		){
			bilA<-x
			dat=bilA@data
			tax=bilA@taxons@data$tax_code
			std=bilA@stades@data$std_code
			dc=bilA@dc@dc_selectionne
			# giving names by default if NULL else checking that arguments dc_name, tax_name, std_name 
			#have the right length			
			if (is.null(dc_name)) dc_name=bilA@dc@data[bilA@dc@data$dc==dc,"dc_code"] else
			if (length(dc)!=length(dc_name)) stop (stringr::str_c("dc_name argument should have length ",length(dc)))
			if (is.null(tax_name)) tax_name=bilA@taxons@data$tax_nom_latin else 
			if (length(tax)!=length(tax_name)) stop (stringr::str_c("tax_name argument should have length ",length(tax)))
			if (is.null(std_name)) std_name=bilA@stades@data$std_libelle else 
			if (length(std)!=length(std_name)) stop (stringr::str_c("std_name argument should have length ",length(std)))
			
			
			dat<-dat[,c("annee","effectif","ope_dic_identifiant","lot_tax_code","lot_std_code")]
			dat<-reshape2::dcast(dat, annee ~ ope_dic_identifiant+lot_tax_code+lot_std_code, value.var="effectif")
			coln<-colnames(dat)[2:length(colnames(dat))]
			# names header for DC
			# this function creates title as "multicolumn" arguments, repeated over columns if necessary
			# it will be passed later as add.to.row print.xtable command
			fn_title<-function(les_valeurs,valeur_uk,name_uk,total=TRUE){
				which_arg<-match(les_valeurs,valeur_uk)
				if (length(les_valeurs)==1) {
					repetes<-FALSE
				} else {
					repetes<-c(les_valeurs[2:length(les_valeurs)]==les_valeurs[1:(length(les_valeurs)-1)],FALSE) # FALSE, at the end we want the values agregated anyway
				}
				rr=1
				les_valeurs_final<-vector()
				for (i in 1:length(les_valeurs)){
					# if the same argument is repeated over current value and the next
					if (repetes[i]) {
						rr<-rr+1
					} else {
						# sortie de la boucle
						les_valeurs_final<-c(les_valeurs_final,stringr::str_c("\\multicolumn{",rr,"}{c}{",xtable::sanitize(name_uk[which_arg[i]]),"}"))
						rr=1
					}				
				}
				if (total) {
					les_valeurs_final<-stringr::str_c(" & ",stringr::str_c(les_valeurs_final,collapse=" & ")," & Total\\\\\n")
				} else {
					les_valeurs_final<-stringr::str_c(" & ",stringr::str_c(les_valeurs_final,collapse=" & ")," & \\\\\n")
					}
				return(les_valeurs_final)
			}
			les_dc<-unlist(lapply(stringr::str_split(coln,"_"),function(X)X[1]))
			les_dc<-fn_title(les_valeurs=les_dc,valeur_uk=dc,name_uk=dc_name,total=FALSE)
			
			#header for tax
			les_tax<-unlist(lapply(stringr::str_split(coln,"_"),function(X)X[2]))
			les_tax<-fn_title(les_valeurs=les_tax,valeur_uk=tax,name_uk=tax_name,total=FALSE)
			# name header for std
			les_std<-unlist(lapply(stringr::str_split(coln,"_"),function(X)X[3]))
			les_std<-fn_title(les_valeurs=les_std,valeur_uk=std,name_uk=std_name,total=TRUE)
			# remove annee (it is now only rownames)
			rownames(dat)<-dat$annee
			dat<-dat[,-1,FALSE]
			# calculating sum
			if (ncol(dat)>1) dat$sum<-rowSums(dat[,1:ncol(dat)],na.rm=TRUE)
			
			
			if (is.null(align)) align<-c("l",rep("r",ncol(dat)))
			if (is.null(display)) display=c("s",rep("f",ncol(dat)))
			xt<-xtable::xtable(dat,caption=caption,label=label,align=align,digits=0,
					display=display, # integer,small scientific if it saves place, string..
					auto=auto)			
			addtorow <- list()
			addtorow$pos <- list()
			addtorow$pos[[1]] <- 0
			addtorow$pos[[2]] <- 0			
			addtorow$pos[[3]] <- 0
			addtorow$pos[[4]] <- 0			
			addtorow$pos[[5]] <- 0
			addtorow$command <- c(les_dc,"\\hline\n", les_tax ,"\\hline\n",les_std)
			assign("addtorow",addtorow,envir_stacomi)
			return(xt)
		})


#' barplot method for object \link{BilanAnnuels-class}		
#' @param height An object of class BilanAnnuels
#' @param legend.text See barplot help 
#' @param ... additional arguments passed to barplot
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases barplot.BilanAnnuels barplot.bilA
#' @seealso \link{BilanAnnuels-class} for examples
#' @export
setMethod("barplot",signature(height = "BilanAnnuels"),definition=function(height,legend.text=NULL,...){ 
			#bilanMigrationInterAnnuelle<-bmi
			bilA<-height
			# require(ggplot2)
			if(nrow(bilA@data)>0){
				
				dat=bilA@data  
				lesdic<-unique(dat$ope_dic_identifiant)
				lestax<-unique(dat$lot_tax_code)
				lesstd<-unique(dat$lot_std_code)
				
				# create a matrix of each dc, taxon, stage
				if (length(lestax)==1&length(lesstd) & length(lesdic)==1){
					
					dat0<-reshape2::dcast(dat, lot_tax_code ~ annee, value.var="effectif")						
					mat<-as.matrix(dat0[,2:ncol(dat0)])
					mat[is.na(mat)]<-0
					barplot(mat,...)
					
				}else if (length(lestax)==1 & length(lesstd)==1){
					
					dat0<-reshape2::dcast(dat, ope_dic_identifiant ~ annee, value.var="effectif")
					mat<-as.matrix(dat0[,2:ncol(dat0)])
					mat[is.na(mat)]<-0
					if (is.null(legend.text)) {
						legend.text=dat0$ope_dic_identifiant
						barplot(mat,legend.text=legend.text,...)
					} else {
						barplot(mat,...)
					}
					
				} else if (length(lestax)==1 & length(lesdic)==1){
					
					dat0<-reshape2::dcast(dat, lot_std_code ~ annee, value.var="effectif")
					mat<-as.matrix(dat0[,2:ncol(dat0)])
					mat[is.na(mat)]<-0
					if (is.null(legend.text)) {
						legend.text=dat0$lot_std_code
						barplot(mat,legend.text=legend.text,...)
					} else {
						barplot(mat,...)
					}
					
				} else if (length(lesdic)==1 & length(lesstd)==1){
					
					dat0<-reshape2::dcast(dat, lot_tax_code ~ annee, value.var="effectif")
					mat<-as.matrix(dat0[,2:ncol(dat0)])
					mat[is.na(mat)]<-0
					if (is.null(legend.text)) {
						legend.text<-dat0$lot_tax_code
						barplot(mat,legend.text=legend.text,...)	
					} else {
						barplot(mat,...)
					}
					
				} else if (length(lestax)==1){
					
					dat0<-reshape2::dcast(dat, ope_dic_identifiant+lot_std_code ~ annee, value.var="effectif")
					mat<-as.matrix(dat0[,3:ncol(dat0)])
					mat[is.na(mat)]<-0
					if (is.null(legend.text)) {
						legend.text<-stringr::str_c(dat0$ope_dic_identifiant,"_",dat0$lot_std_code)
						barplot(mat,legend.text=legend.text,...)	
					} else {
						barplot(mat,...)
					}
					
				} else if (length(lesstd)==1){
					
					dat0<-reshape2::dcast(dat, ope_dic_identifiant+lot_tax_code ~ annee, value.var="effectif")
					mat<-as.matrix(dat0[,3:ncol(dat0)])
					mat[is.na(mat)]<-0
					if (is.null(legend.text)){
						legend.text<-stringr::str_c(dat0$ope_dic_identifiant,"_",dat0$lot_tax_code)
						barplot(mat,legend.text=legend.text,...)	
					} else {
						barplot(mat,...)
					}
				} else if (length(lesdic)==1){
					
					dat0<-reshape2::dcast(dat, lot_std_code+lot_tax_code ~ annee, value.var="effectif")
					mat<-as.matrix(dat0[,3:ncol(dat0)])
					mat[is.na(mat)]<-0
					if (is.null(legend.text)) {
						legend.text<-stringr::str_c(dat0$lot_tax_code,"_",dat0$lot_std_code)
						barplot(mat,legend.text=legend.text,...)	
					} else {
						barplot(mat,...)
					}
					
				} else {
					
					dat0<-reshape2::dcast(dat, ope_dic_identifiant+lot_tax_code+lot_std_code~annee, value.var="effectif")
					mat<-as.matrix(dat0[,4:ncol(dat0)])
					mat[is.na(mat)]<-0
					if (is.null(legend.text)) {
						legend.text<-stringr::str_c(dat0$ope_dic_identifiant,"_",
								dat0$lot_tax_code,"_",dat0$lot_std_code)
						barplot(mat,legend.text=legend.text,...)		
					} else {
						barplot(mat,...)
					}
				}
			}    else     {
				funout(gettext("No data",domain="R-stacomiR"))
			}				
		})



#' Plot method for BilanAnnuels
#' 
#' @param x An object of class \link{BilanAnnuels-class}
#' @param plot.type Default point
#' @param silent Stops displaying the messages.
#' \itemize{
#' 		\item{plot.type="point": ggplot+geom_point}' 		
#' }
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases plot.BilanAnnuels plot.bilA
#' @seealso \link{BilanMigrationInterAnnuelle-class} for examples
#' @export
setMethod("plot",signature(x = "BilanAnnuels", y = "missing"),definition=function(x, 
				plot.type="point",
				silent=FALSE){ 
			bilA<-x
			dat<-bilA@data
			lesdic<-unique(dat$ope_dic_identifiant)
			lestax<-unique(dat$lot_tax_code)
			lesstd<-unique(dat$lot_std_code)
			
			if(nrow(bilA@data)>0){
				if (plot.type=="point"){
					
					colnames(dat)<-c("effectif","annee","dc","taxon","stade")
					dat$dc<-as.factor(dat$dc)
					dat$taxon<-as.factor(dat$taxon)
					if (length(lestax)==1 & length(lesstd) & length(lesdic)==1){
						
						g<-ggplot(dat,aes(x=annee,y=effectif))+geom_point()+
								geom_line()+
								theme_bw() 
						print(g)
						assign("g",g,envir_stacomi)
						if (!silent) funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi)\n",domain="R-stacomiR"))
						
					} else if (length(lestax)==1 & length(lesstd)==1){
						
						g<-ggplot(dat,aes(x=annee,y=effectif))+geom_point(aes(col=dc))+
								geom_line(aes(col=dc))+
								theme_bw() 
						print(g)
						assign("g",g,envir_stacomi)
						if (!silent) funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi)\n",domain="R-stacomiR"))
						
					} else if (length(lestax)==1 & length(lesdic)==1){
						
						g<-ggplot(dat,aes(x=annee,y=effectif))+geom_point(aes(col=stade))+
								geom_line(aes(col=stade))+
								theme_bw() 
						print(g)
						assign("g",g,envir_stacomi)
						if (!silent) funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi)\n",domain="R-stacomiR"))
						
					} else if (length(lesdic)==1 & length(lesstd)==1){
						
						g<-ggplot(dat,aes(x=annee,y=effectif))+geom_point(aes(col=taxon))+
								geom_line(aes(col=taxon))+
								theme_bw() 
						print(g)
						assign("g",g,envir_stacomi)
						if (!silent) funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi)\n",domain="R-stacomiR"))
						
						
					} else if (length(lestax)==1){
						
						g<-ggplot(dat,aes(x=annee,y=effectif))+geom_point(aes(col=dc,shape=stade))+
								geom_line(aes(col=dc,shape=stade))+
								theme_bw() 
						print(g)
						assign("g",g,envir_stacomi)
						if (!silent) funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi)\n",domain="R-stacomiR"))
						
					} else if (length(lesstd)==1){
						
						g<-ggplot(dat,aes(x=annee,y=effectif))+geom_point(aes(col=dc,shape=taxon))+
								geom_line(aes(col=dc,shape=taxon))+
								theme_bw() 
						print(g)
						assign("g",g,envir_stacomi)
						if (!silent) funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi)\n",domain="R-stacomiR"))
						
					} else if (length(lesdic)==1){
						
						g<-ggplot(dat,aes(x=annee,y=effectif))+geom_point(aes(col=taxon,shape=stade))+
								geom_line(aes(col=taxon,shape=stade))+
								theme_bw() 
						print(g)
						assign("g",g,envir_stacomi)
						if (!silent) funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi)\n",domain="R-stacomiR"))							
						
					} else {
						if (length(lesdic)<3){
							g<-ggplot(dat,aes(x=annee,y=effectif))+geom_point(aes(col=taxon,shape=stade))+
									geom_line(aes(col=taxon,shape=stade))+
									facet_wrap(~dc)+
									theme_bw() 
							print(g)
							assign("g",g,envir_stacomi)
						} else {
							g<-ggplot(dat,aes(x=annee,y=effectif))+geom_point(aes(col=stade))+
									geom_line(aes(col=stade))+
									facet_grid(dc~stade)+
									theme_bw() 
							print(g)	
							
							assign("g",g,envir_stacomi)
							if (!silent) funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get(\"g\",envir_stacomi)\n",domain="R-stacomiR"))	
						}
					}
				}
				
			}    else     {
				funout(gettext("No data",domain="R-stacomiR"))
			}	
		})


#' Barplot handler
#' @param h handler
#' @param ... additional parameters
hbarplotBilanAnnuels = function(h,...)
{
	bilA <- get("bilanAnnuels",envir=envir_stacomi)
	bilA <- charge(bilA)
	bilA <- connect(bilA)
	barplot(bilA)			
}

#' plot handler
#' @param h handler
#' @param ... additional parameters
hplotBilanAnnuels = function(h,...)
{
	bilA <- get("bilanAnnuels",envir=envir_stacomi)
	bilA <- charge(bilA)
	bilA <- connect(bilA)
	plot(bilA)			
}


#' xtable handler
#' @param h handler
#' @param ... additional parameters
hxtableBilanAnnuels = function(h,...)
{
	bilA <- get("bilanAnnuels",envir=envir_stacomi)
	bilA <- charge(bilA)
	bilA <- connect(bilA)
	print(xtable(bilA))			
}