#' Migration report for one DC, one species and one stage
#' 
#' This class performs a migration summary. A migration monitoring operation can correspond to a single
#' horodate (in the case of some video monitoring operation) or comprise a period which does not necessarily
#' span a full day. The daily migration is calculated by splitting the operation between days, and the migration is either
#' grouped or split according to the lenth of the different time spans. 
#' @include ref_taxa.R
#' @include ref_stage.R
#' @include ref_timestep_daily.R
#' @include report_df.R
#' @include report_dc.R
#' @include report_ope.R
#' @slot dc Object of class \link{ref_dc-class}: the control device 
#' @slot taxa Object of class \link{ref_taxa-class}: the species
#' @slot stage Object of class \link{ref_stage-class} : the stage of the fish
#' @slot timestep Object of class \link{ref_timestep_daily-class} : the time step 
#' constrained to daily value and 365 days
#' @slot data Object of class \code{data.frame} with data filled in from the connect method
#' @slot calcdata A "list" of calculated daily data, one per dc, filled in by the calcule method
#' @slot coef_conversion A data.frame of daily weight to number conversion coefficients, filled in by the connect 
#' method if any weight are found in the data slot.
#' @slot time.sequence Object of class \code{POSIXct} : a time sequence of days generated by the calcule method
#' @note In practise, the report_mig class uses methods (calcule, connect...) from the more elaborate \link{report_mig_mult-class}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @family report Objects
#' @keywords classes
#' @aliases report_mig
#' @example inst/examples/report_mig-example.R
#' @export 
setClass(Class="report_mig",
	representation=
		representation(dc="ref_dc",
			taxa="ref_taxa",
			stage="ref_stage",
			timestep="ref_timestep_daily",
			data="data.frame",
			calcdata="list",
			coef_conversion="data.frame",
			time.sequence="POSIXct"),
	prototype=prototype(dc=new("ref_dc"),
		taxa=new("ref_taxa"),
		stage=new("ref_stage"),
		timestep=new("ref_timestep_daily"),
		data=data.frame(),
		calcdata=list(),
		coef_conversion=data.frame(),
		time.sequence=as.POSIXct(Sys.time()) 
	))
# report_mig= new("report_mig")

setValidity("report_mig",function(object)
	{
	  rep1=length(object@dc)==1
	  rep2=length(object@taxa)==1
	  rep3=length(object@stage)==1
	  rep3=length(object@timestep)==1
	  rep4=(object@timestep@nb_step==365|object@timestep@nb_step==366) # constraint 365 to 366 days
	  rep5=as.numeric(strftime(object@timestep@dateDebut,'%d'))==1 # contrainte : depart = 1er janvier
	  rep6=as.numeric(strftime(object@timestep@dateDebut,'%m'))==1
	  return(ifelse(rep1 & rep2 & rep3 & rep4 & rep5 & rep6 , TRUE ,c(1:6)[!c(rep1, rep2, rep3, rep4, rep5, rep6)]))
	}   
)

#' handler for calculations report_mig
#' 
#'  internal use
#' @param h handler
#' @param ... additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
h_report_migcalc=function(h,...){
  if (exists("report_mig",envir_stacomi)) {
	report_mig<-get("report_mig",envir_stacomi)
  } else {      
	funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
  }
  report_mig<-charge(report_mig)
  report_mig<-connect(report_mig)
  report_mig<-calcule(report_mig)
}

#' connect method for report_mig
#' 
#' 
#' uses the report_mig_mult method
#' @param object An object of class \link{report_mig-class}
#' @param silent Boolean default FALSE, if TRUE information messages not displayed
#' @return report_mig with slot @data filled from the database
#' @aliases connect.report_mig
#' @export
setMethod("connect",signature=signature("report_mig"),definition=function(object,silent=FALSE){ 
	  report_mig<-object
	  report_mig_mult<-as(report_mig,"report_mig_mult")
	  report_mig_mult<-connect(report_mig_mult,silent=silent)
	  report_mig@data<-report_mig_mult@data
	  report_mig@coef_conversion<-report_mig_mult@coef_conversion
	  return(report_mig)
	})
#' command line interface for report_mig class
#' @param object An object of class \link{report_mig-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,ref_dc-method}
#' @param taxa Either a species name in latin or the SANDRE code for species (ie 2038=Anguilla anguilla),
#' these should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,ref_taxa-method}
#' @param stage A stage code matching the ref.tr_stadedeveloppement_std table in the stacomi database see \link{choice_c,ref_stage-method}
#' @param datedebut The starting date as a character, formats like \code{\%Y-\%m-\%d} or \code{\%d-\%m-\%Y} can be used as input
#' @param datefin The finishing date of the report, for this class this will be used to calculate the number of daily steps.
#' @return An object of class \link{report_mig-class}
#' The choice_c method fills in the data slot for ref_dc, ref_taxa, ref_stage, and refref_timestep_daily and then 
#' uses the choice_c methods of these object to select the data.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases choice_c.report_mig
#' @export
setMethod("choice_c",signature=signature("report_mig"),definition=function(object,dc,taxa,stage,datedebut,datefin){
	  # code for debug using r_mig example
	  #report_mig<-r_mig;dc=5;taxa="Liza ramada";stage="IND";datedebut="2015-01-01";datefin="2015-12-31"
	  report_mig<-object
	  report_mig@dc=charge(report_mig@dc)
	  # loads and verifies the dc
	  # this will set dc_selectionne slot
	  report_mig@dc<-choice_c(object=report_mig@dc,dc)
	  # only taxa present in the report_mig are used
	  report_mig@taxa<-charge_with_filter(object=report_mig@taxa,report_mig@dc@dc_selectionne)			
	  report_mig@taxa<-choice_c(report_mig@taxa,taxa)
	  report_mig@stage<-charge_with_filter(object=report_mig@stage,report_mig@dc@dc_selectionne,report_mig@taxa@data$tax_code)	
	  report_mig@stage<-choice_c(report_mig@stage,stage)
	  report_mig@timestep<-choice_c(report_mig@timestep,datedebut,datefin)
	  return(report_mig)
	})

#' Loads additional data on migration control operations, df (fishway) dc (counting device).
#' 
#' this method creates additional classes in envir_stacomi for later use in plot (operations, 
#' DF operation, DC operation). So unlike in most report classes where the charge method is only
#' used by the graphical interface, it is necessary to run charge for report_mig.
#' @param object An object of class \code{\link{report_mig-class}}
#' @param silent Should the program be returning messages
#' @return An object of class \link{report_mig-class} with slots filled by user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases charge.report_mig
#' @export
setMethod("charge",signature=signature("report_mig"),definition=function(object,silent=FALSE){ 
	  report_mig<-object
	  #report_mig<-r_mig
	  #pour l'instant ne lancer que si les fenetre sont fermees
	  # funout(gettext("launching updateplot \n",domain="R-stacomiR"))
	  if (exists("ref_dc",envir_stacomi)) {
		report_mig@dc<-get("ref_dc",envir_stacomi)
		dc<-report_mig@dc@dc_selectionne
		df<-report_mig@dc@data$df[report_mig@dc@data$dc%in%dc]
	  } else {
		funout(gettext("You need to choose a counting device, clic on validate\n",domain="R-stacomiR"),arret=TRUE)	
	  }
	  if (exists("ref_taxa",envir_stacomi)) {
		report_mig@taxa<-get("ref_taxa",envir_stacomi)
	  } else {      
		funout(gettext("You need to choose a taxa, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
	  }
	  if (exists("ref_stage",envir_stacomi)){
		report_mig@stage<-get("ref_stage",envir_stacomi)
	  } else 
	  {
		funout(gettext("You need to choose a stage, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
	  }
	  if (exists("timestep",envir_stacomi)){
		report_mig@timestep<-get("timestep",envir_stacomi)
	  } else {
		funout(gettext("Attention, no time step selected, compunting with default value\n",domain="R-stacomiR"),arret=FALSE)
		warning(gettext("Attention, no time step selected, compunting with default value\n",domain="R-stacomiR"))
	  }
	  
	  #################################
	  # loading data for other classes associated with report_mig_mult
	  #################################
	  report_df=new("report_df")		
	  report_dc=new("report_dc")
	  report_ope=new("report_ope")
	  assign("report_dc_date_debut",get("timestep",envir_stacomi)@"dateDebut",envir_stacomi)
	  assign("report_dc_date_fin",as.POSIXlt(end_date(get("timestep",envir_stacomi))),envir_stacomi)
	  assign("report_df_date_debut",get("timestep",envir_stacomi)@"dateDebut",envir_stacomi)
	  assign("report_df_date_fin",as.POSIXlt(end_date(get("timestep",envir_stacomi))),envir_stacomi)
	  assign("report_ope_date_debut",get("timestep",envir_stacomi)@"dateDebut",envir_stacomi)
	  assign("report_ope_date_fin",as.POSIXlt(end_date(get("timestep",envir_stacomi))),envir_stacomi)
	  
	  report_ope<-charge(report_ope) 
	  # charge will search for ref_dc (possible multiple choice), report_ope_date_debut
	  # and report_ope_date_fin in envir_stacomi
	  # charge will search for ref_dc (possible multiple choice), report_dc_date_debut
	  # and report_dc_date_fin in envir_stacomi
	  report_dc<-charge(report_dc)
	  ref_df=new("ref_df")
	  ref_df<-charge(ref_df)
	  ref_df<-choice_c(ref_df,df)
	  
	  assign("ref_df",ref_df,envir=envir_stacomi)
	  
	  # charge will search for ref_df (possible multiple choice), report_df_date_debut
	  # and report_df_date_fin in envir_stacomi
	  report_df<-charge(report_df)
	  # the object are assigned to the envir_stacomi for later use by the connect method
	  assign("report_df",report_df,envir=envir_stacomi)
	  assign("report_dc",report_dc,envir=envir_stacomi)
	  assign("report_ope",report_ope,envir=envir_stacomi)					
	  return(report_mig)
	})


#' Transforms migration per period to daily migrations, and performs the conversion from weights to number is data
#' are stored as weights (glass eel).
#' 
#'  The calculation must be launched once data are filled by the connect method. Currently the negative argument
#' has no effect.
#' @param object An object of class \code{\link{report_mig-class}}
#' @param negative a boolean indicating if a separate sum must be done for positive and negative values, if true, positive and negative counts return 
#' different rows
#' @param silent Boolean, if TRUE, information messages are not displayed, only warnings and errors
#' @note The class report_mig does not handle escapement rates nor 
#' 'devenir' i.e. the destination of the fishes.
#' @return report_mig with calcdata slot filled. It is a list with one element per counting device containing
#' \describe{
#' \item{method}{In the case of instantaneous periods (video counting) the sum of daily values is done by the \link{fun_report_mig_mult} method and the value indicated in method is "sum".
#'  If any migration monitoring period is longer than a day, then the migration is split using the \link{fun_report_mig_mult_overlaps} function and the value indicated in the 
#' method is "overlaps" as the latter method uses the overlap package to split migration period.}
#' \item{data}{the calculated data.}
#' \item{contient_poids}{A boolean which indicates, in the case of glass eel, that the function \link{fun_weight_conversion} has been run to convert the weights to numbers using the weight
#' to number coefficients in the database (see link{report_ge_weight}).}
#' \item{negative}{A parameter indicating if negative migration (downstream in the case of upstream migration devices) have been converted to positive numbers,
#' not developed yet}}
#' @aliases calcule.report_mig
#' @export
setMethod("calcule",signature=signature("report_mig"),definition=function(object,negative=FALSE,silent=FALSE){ 
	  #report_mig<-r_mig
	  #report_mig<-bM_Arzal_civ
	  #negative=FALSE;silent=FALSE
	  if (!silent){
		funout(gettext("Starting migration summary ... be patient\n",domain="R-stacomiR"))
	  }
	  report_mig<-object
	  
	  if (nrow(report_mig@data)>0){
#				report_mig@data$time.sequence=difftime(report_mig@data$ope_date_fin,
#						report_mig@data$ope_date_debut,
#						units="days")
		debut=report_mig@timestep@dateDebut
		fin=end_date(report_mig@timestep)
		time.sequence<-seq.POSIXt(from=debut,to=fin,
			by=as.numeric(report_mig@timestep@step_duration))
		report_mig@time.sequence<-time.sequence
		lestableaux<-list()			
		datasub<-report_mig@data	
		dic<-unique(report_mig@data$ope_dic_identifiant)
		stopifnot(length(dic)==1)
		datasub$duree=difftime(datasub$ope_date_fin,datasub$ope_date_debut,units="days")
		if (any(datasub$duree>(report_mig@timestep@step_duration/86400))){				
		  #----------------------
		  # reports with overlaps
		  #----------------------
		  data<-fun_report_mig_mult_overlaps(time.sequence = time.sequence, datasub = datasub,negative=negative)
		  # to remain compatible with report_mig
		  data$taux_d_echappement=-1					
		  lestableaux[[stringr::str_c("dc_",dic)]][["data"]]<-data
		  lestableaux[[stringr::str_c("dc_",dic)]][["method"]]<-"overlaps"
		  contient_poids<-"poids"%in%datasub$type_de_quantite
		  lestableaux[[stringr::str_c("dc_",dic)]][["contient_poids"]]<-contient_poids
		  lestableaux[[stringr::str_c("dc_",dic)]][["negative"]]<-negative
		  if (contient_poids){
			coe<-report_mig@coef_conversion[,c("coe_date_debut","coe_valeur_coefficient")]
			data$coe_date_debut<-as.Date(data$debut_pas)
			data<-merge(data,coe,by="coe_date_debut")
			data<-data[,-1] # removing coe_date_debut
			data <-fun_weight_conversion(tableau=data,time.sequence=report_mig@time.sequence,silent)
		  }
		  
		  lestableaux[[stringr::str_c("dc_",dic)]][["data"]]<-data
		  
		} else {
		  #----------------------
		  #report simple
		  #----------------------
		  data<-fun_report_mig_mult(time.sequence = time.sequence,datasub=datasub,negative=negative)
		  data$taux_d_echappement=-1					
		  contient_poids<-"poids"%in%datasub$type_de_quantite
		  if (contient_poids){
			coe<-report_mig@coef_conversion[,c("coe_date_debut","coe_valeur_coefficient")]
			data$coe_date_debut<-as.Date(data$debut_pas)
			data<-merge(data,coe,by="coe_date_debut")
			data<-data[,-1] # removing coe_date_debut
			data <-fun_weight_conversion(tableau=data,time.sequence=report_mig@time.sequence,silent)
		  } else {
			data$coe_valeur_coefficient=NA
		  }
		  lestableaux[[stringr::str_c("dc_",dic)]][["data"]]<-data
		  lestableaux[[stringr::str_c("dc_",dic)]][["method"]]<-"sum"
		  lestableaux[[stringr::str_c("dc_",dic)]][["contient_poids"]]<-contient_poids
		  lestableaux[[stringr::str_c("dc_",dic)]][["negative"]]<-negative
		}
		# TODO developper une methode pour sumneg 
		report_mig@calcdata<-lestableaux
		assign("report_mig",report_mig,envir_stacomi)
		if (!silent){
		  funout(gettext("Summary object is stocked into envir_stacomi environment : write report_mig=get('report_mig',envir_stacomi) \n",domain="R-stacomiR"))
		  funout(gettext("To access calculated data, type report_mig@calcdata\n",domain="R-stacomiR"))
		}
		
		
		
	  } else {
		# no fish...
		funout(gettext("There are no values for the taxa, stage and selected period\n",domain="R-stacomiR"))
	  }
	  return(report_mig)
	})



#' handler to print the command line
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
houtreport_mig=function(h=null,...) {
  if (exists("ref_stage",envir_stacomi)) 	{
	report_mig<-get("report_mig",envir_stacomi)
	print(report_mig)
  } 
  else 
  {      
	funout(gettext("Please select DC, taxa, and stages for a complete command\n",domain="R-stacomiR"),arret=TRUE)
  }
}

#' Method to print the command line of the object
#' @param x An object of class report_mig
#' @param ... Additional parameters passed to print
#' @return NULL
#' @author cedric.briand
#' @aliases print.report_mig
#' @export
setMethod("print",signature=signature("report_mig"),definition=function(x,...){ 
	  sortie1<-"report_mig=new('report_mig');"
	  sortie2<-stringr::str_c("report_mig=choice_c(report_mig,",
		  "dc=c(",stringr::str_c(x@dc@dc_selectionne,collapse=","),"),",
		  "taxa=c(",stringr::str_c(shQuote(x@taxa@data$tax_nom_latin),collapse=","),"),",
		  "stage=c(",stringr::str_c(shQuote(x@stage@data$std_code),collapse=","),"),",			
		  "datedebut=",shQuote(strftime(x@timestep@dateDebut,format="%d/%m/%Y")),
		  ",datefin=",shQuote(strftime(end_date(x@timestep),format="%d/%m/%Y")),")")
	  # removing backslashes
	  funout(stringr::str_c(sortie1,sortie2),...)
	  return(invisible(NULL))
	})




#' Plots of various type for report_mig.
#' 
#' \itemize{
#' 		\item{plot.type="standard"}{calls \code{\link{fungraph}} and \code{\link{fungraph_glasseel}} functions to plot as many "report_mig"
#' 			as needed, the function will test for the existence of data for one dc, one taxa, and one stage}
#' 		\item{plot.type="step"}{creates Cumulated graphs for report_mig_mult.  Data are summed per day for different dc taxa and stages}
#' 		\item{plot.type="multiple"}{Method to overlay graphs for report_mig_mult (multiple dc/taxa/stage in the same plot)}
#' }
#' @param x An object of class report_mig
#' @param y From the formals but missing
#' @param plot.type One of "standard","step". Defaut to \code{standard} the standard report_mig with dc and operation displayed, can also be \code{step} or 
#' \code{multiple} 
#' @param silent Stops displaying the messages.
#' @param color Default NULL, argument passed for the plot.type="standard" method. A vector of color in the following order : (1) working, (2) stopped, (3:7) 1...5 types of operation,
#' (8:11) numbers, weight, NULL, NULL (if glass eel), (8:11)  measured, calculated, expert, direct observation for other taxa. If null will be set to brewer.pal(12,"Paired")[c(8,10,4,6,1,2,3,5,7)]
#' @param color_ope Default NULL, argument passed for the plot.type="standard" method. A vector of color for the operations. Default to brewer.pal(4,"Paired")
#' @param ... Additional arguments passed to matplot or plot if plot.type="standard", see ... in \link{fungraph_glasseel} and \link{fungraph}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases plot.report_mig
#' @export
setMethod("plot",signature(x = "report_mig", y = "ANY"),definition=function(x, y,plot.type="standard",color=NULL, color_ope=NULL,silent=FALSE,...){ 
	  #report_mig<-r_mig
	  report_mig<-x
	  ################################################################
	  #                 standard plot
	  ################################################################
	  if (plot.type=="standard"){
		if (!silent) print("plot type standard")
		if (!silent) funout(gettext("Statistics about migration :\n",domain="R-stacomiR"))				
		taxa=report_mig@taxa@data[1,"tax_nom_latin"]
		stage=report_mig@stage@data[1,"std_libelle"]
		dc=as.numeric(report_mig@dc@dc_selectionne)[1]
		data<-report_mig@calcdata[[stringr::str_c("dc_",dc)]][["data"]]
		if (!is.null(data)){
		  if	(nrow(data)>0){						
			if (!silent) {
			  funout(paste("dc=",dc,
					  "taxa"=taxa,
					  "stage"=stage,"\n"))
			  funout("---------------------\n")
			}
			if (any(duplicated(data$No.pas))) stop("duplicated values in No.pas")
			data_without_hole<-merge(
				data.frame(No.pas=as.numeric(strftime(report_mig@time.sequence,format="%j"))-1,
					debut_pas=report_mig@time.sequence),
				data,
				by=c("No.pas","debut_pas"),
				all.x=TRUE
			)
			data_without_hole$CALCULE[is.na(data_without_hole$CALCULE)]<-0
			data_without_hole$MESURE[is.na(data_without_hole$MESURE)]<-0
			data_without_hole$EXPERT[is.na(data_without_hole$EXPERT)]<-0
			data_without_hole$PONCTUEL[is.na(data_without_hole$PONCTUEL)]<-0
			if (report_mig@calcdata[[stringr::str_c("dc_",dc)]][["contient_poids"]]&
				taxa == "Anguilla anguilla"&
				(stage == "civelle"|stage=="Anguille jaune")) {							
			  #----------------------------------
			  # report migration with weights (glass eel)
			  #-----------------------------------------
			  
			  fungraph_glasseel(report_mig=report_mig,
				  table=data_without_hole,
				  time.sequence=report_mig@time.sequence,
				  taxa=taxa,
				  stage=stage,
				  dc=dc,
				  silent,
				  color=color,
				  color_ope=color_ope,									
				  ...)
			}	else {
			  
			  #----------------------------------
			  # report migration standard
			  #-----------------------------------------
			  #silent=TRUE
			  fungraph(report_mig=report_mig,
				  tableau=data_without_hole,
				  time.sequence=report_mig@time.sequence,
				  taxa,
				  stage,
				  dc,
				  color=color,
				  color_ope=color_ope,
				  silent,
				  ...)
			}
		  } # end nrow(data)>0	
		} # end is.null(data)
		
		################################################################
		#                 step plot
		################################################################
		#FIXME problem with negative numbers
	  } else if (plot.type=="step"){
		taxa  <-  as.character(report_mig@taxa@data$tax_nom_latin)
		stage  <-  as.character(report_mig@stage@data$std_libelle)
		dc  <-  as.numeric(report_mig@dc@dc_selectionne)	
		if (report_mig@timestep@step_duration==86400 & report_mig@timestep@step_duration==86400) {
		  grdata<-report_mig@calcdata[[stringr::str_c("dc_",dc)]][["data"]]
		  grdata<-fun_date_extraction(grdata,
			  nom_coldt="debut_pas",
			  annee=FALSE,
			  mois=TRUE,
			  quinzaine=TRUE,
			  semaine=TRUE,
			  jour_an=TRUE,
			  jour_mois=FALSE,
			  heure=FALSE)
		  grdata$Cumsum <- cumsum(grdata$Effectif_total)
		  # pour sauvegarder sous excel
		  annee <- unique(strftime(as.POSIXlt(report_mig@time.sequence),"%Y"))[1]
		  dis_commentaire <-  as.character(report_mig@dc@data$dis_commentaires[report_mig@dc@data$dc%in%report_mig@dc@dc_selectionne]) 
		  update_geom_defaults("line", aes(size = 2))
		  
		  p<-ggplot(grdata)+
			  geom_line(aes(x=debut_pas,y=Cumsum,colour=mois)) +
			  ylab(gettext("Cumulative migration",domain="R-stacomiR")) +
			  ggtitle(gettextf("Cumulative count %s, %s, %s, %s",dis_commentaire,taxa,stage,annee)) + 
			  theme(plot.title = element_text(size=10,colour="navy")) +
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



#' handler for  h_report_miggraph
#' 
#' Standard report_mig graph over time
#' @param h handler
#' @param ... additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
h_report_miggraph = function(h,...) {
  if (exists("report_mig",envir_stacomi)) {
	report_mig<-get("report_mig",envir_stacomi)
  } else {      
	funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
  }
  plot(report_mig,plot.type="standard")
  
}

#' handler for h_report_miggraph2
#' 
#' Step plot over time
#' @param h handler
#' @param ... additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
h_report_miggraph2 = function(h,...) {
  if (exists("report_mig",envir_stacomi)) {
	report_mig<-get("report_mig",envir_stacomi)
  } else {      
	funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
  }
  plot(report_mig,plot.type="step")
}

#' handler for summary function, internal use
#' calls functions funstat and funtable to build summary tables in html and
#' csv files
#' @param h Handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
hTablereport_mig=function(h,...) {
  if (exists("report_mig",envir_stacomi)) 
  {
	report_mig<-get("report_mig",envir_stacomi)
  } 
  else 
  {      
	funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
  }
  summary(report_mig)
  
}

#' summary for report_mig 
#' calls functions funstat and funtable to create migration overviews
#' and generate csv and html output in the user data directory
#' @param object An object of class \code{\link{report_mig-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @param ... Additional parameters (not used there)
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases summary.report_mig
#' @export
setMethod("summary",signature=signature(object="report_mig"),definition=function(object,silent=FALSE,...){
	  report_mig_mult<-as(object,"report_mig_mult")
	  summary(report_mig_mult,silent=silent)			
	})





#' handler h_report_migwrite
#' Allows the saving of daily and monthly counts in the database
#' @note these entries are necessary to run the Interannual Migration class. 
#' then no entry will be written to the database

#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
h_report_migwrite = function(h,...) {
  if (exists("report_mig",envir_stacomi)) {
	report_mig<-get("report_mig",envir_stacomi)
  } else {      
	funout(gettext("You need to launch computation first, clic on calc\n",domain="R-stacomiR"),arret=TRUE)
  }
  # ecriture du report journalier, ecrit aussi le report mensuel
  database_expected<-get("database_expected",envir=envir_stacomi)
  if (database_expected) {
	write_database(report_mig,silent=FALSE)
  }	else {
	funout(gettext("no report written to database : database_expected argument=FALSE",domain="R-stacomiR"))
  }
  
}
#' Command line method to write the daily and monthly counts to the 
#' t_bilanmigrationjournalier_bjo table
#' 
#' Daily values are needed to compare migrations from year to year, by the class \link{report_mig_interannual-class}. They are added by
#' by this function.  
#' @param object an object of class \code{\linkS4class{report_mig}}
#' @param silent : TRUE to avoid messages
#' @param check_for_bjo : do you want to check if data are already present in the bjo table, and delete them,
#' this param was added otherwise connect method when called from report_mig_interannual runs in loops
#' @note the user is asked whether or not he wants to overwrite data, if no
#' data are present in the database, the import is done anyway. The name of the database
#' is retrieved from the odbc link
#' 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#' stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE) 
#' data("r_mig")
#' r_mig<-calcule(r_mig)
#' write_database(report_mig=r_mig,silent=FALSE)
#' }
#' @aliases write_database.report_mig
#' @export
setMethod("write_database",signature=signature("report_mig"),definition=function(object,silent=TRUE,check_for_bjo=TRUE){
	  # dbname="bd_contmig_nat";host="localhost";silent=FALSE;port=5432
	  # object=bM
	  #host : the host for sqldf, defaults to "localhost"
	  #port : the port, defaults to 5432
	  host <- get("sqldf.options",envir=envir_stacomi)["sqldf.RPostgreSQL.host"]
	  port <- get("sqldf.options",envir=envir_stacomi)["sqldf.RPostgreSQL.port"]		
	  # getting the database name
	  dbname <- get("sqldf.options",envir=envir_stacomi)["sqldf.RPostgreSQL.dbname"]				
	  report_mig<-object
	  if (class(report_mig)!="report_mig") stop("the report_mig should be of class report_mig")
	  if (class(silent)!="logical") stop("the silent argument should be a logical")
	  dc=as.numeric(report_mig@dc@dc_selectionne)[1]
	  data=report_mig@calcdata[[stringr::str_c("dc_",dc)]][["data"]]
		# keep one line if there is one species in one day with as much up as down...
		if (nrow(data)>1) 	  data=data[data$Effectif_total!=0,]
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
		if (nrow(data)>1) data[,-cannotbenull][data[,-cannotbenull]==0]<-NA
	  annee<-as.numeric(unique(strftime(as.POSIXlt(report_mig@time.sequence),"%Y"))[1])
	  if ("Poids_total"%in%colnames(data)){
		aat_reportmigrationjournalier_bjo=cbind(
			report_mig@dc@dc_selectionne,
			report_mig@taxa@data$tax_code,
			report_mig@stage@data$std_code,
			annee, # une valeur
			rep(jour_dans_lannee_non_nuls,ncol(data[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total","Effectif_total.p","Effectif_total.e","poids_depuis_effectifs","Poids_total","taux_d_echappement","coe_valeur_coefficient")])),
			utils::stack(data[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total","Effectif_total.p","Effectif_total.e","poids_depuis_effectifs","Poids_total","taux_d_echappement","coe_valeur_coefficient")]),  
			Sys.time(),
			substr(toupper(rlang::env_get(envir_stacomi, "sch")),1,nchar(toupper(rlang::env_get(envir_stacomi, "sch")))-1)
		)	
	  } else{
		aat_reportmigrationjournalier_bjo=cbind(
			report_mig@dc@dc_selectionne,
			report_mig@taxa@data$tax_code,
			report_mig@stage@data$std_code,
			annee, # une valeur
			rep(jour_dans_lannee_non_nuls,ncol(data[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total","taux_d_echappement","coe_valeur_coefficient")])),
			utils::stack(data[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total","taux_d_echappement","coe_valeur_coefficient")]),  
			Sys.time(),
			substr(toupper(rlang::env_get(envir_stacomi, "sch")),1,nchar(toupper(rlang::env_get(envir_stacomi, "sch")))-1)
		)	
	  }
	  aat_reportmigrationjournalier_bjo= stacomirtools::killfactor(aat_reportmigrationjournalier_bjo[!is.na(aat_reportmigrationjournalier_bjo$values),])
	  colnames(aat_reportmigrationjournalier_bjo)<-c("bjo_dis_identifiant","bjo_tax_code","bjo_std_code","bjo_annee","bjo_jour","bjo_valeur","bjo_labelquantite","bjo_horodateexport","bjo_org_code")
	  
	  #####
	  # Ci dessous conversion de la classe vers migration Interannuelle pour utiliser
	  # les methodes de cette classe
	  bil=as(report_mig,"report_mig_interannual")
	  # the argument check_for_bjo ensures that we don't re-run the connect method
	  # in loop when the write_database is called from within the report_mig_interannual connect method
	  # check = FALSE tells the method not to check for missing data (we don't want that check when the
	  # write database is called from the report_mig class
	  if (check_for_bjo) bil=connect(bil,silent=silent,check=FALSE)
      
	  
	  hconfirm=function(h,...){			
		if (check_for_bjo) supprime(bil)			
		baseODBC<-get("baseODBC",envir=envir_stacomi)
		sql<-stringr::str_c("INSERT INTO ",rlang::env_get(envir_stacomi, "sch"),"t_bilanmigrationjournalier_bjo (",			
			"bjo_dis_identifiant,bjo_tax_code,bjo_std_code,bjo_annee,bjo_jour,bjo_valeur,bjo_labelquantite,bjo_horodateexport,bjo_org_code)",
			" SELECT * FROM  aat_reportmigrationjournalier_bjo;")
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
# ecriture egalement du report mensuel
		taxa= as.character(report_mig@taxa@data$tax_nom_latin)
		stage= as.character(report_mig@stage@data$std_libelle)
		DC=as.numeric(report_mig@dc@dc_selectionne)	
		tableau<-report_mig@calcdata[[stringr::str_c("dc_",DC)]][["data"]]
		resum=funstat(tableau=tableau,time.sequence=tableau$debut_pas,taxa,stage,DC,silent=silent )
		fun_write_monthly(report_mig,resum,silent=silent)
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
		sql<-stringr::str_c("INSERT INTO ",rlang::env_get(envir_stacomi, "sch"),"t_bilanmigrationjournalier_bjo (",			
			"bjo_dis_identifiant,bjo_tax_code,bjo_std_code,bjo_annee,bjo_jour,bjo_valeur,bjo_labelquantite,bjo_horodateexport,bjo_org_code)",
			" SELECT * FROM  aat_reportmigrationjournalier_bjo;")
		
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
		taxa= as.character(report_mig@taxa@data$tax_nom_latin)
		stage= as.character(report_mig@stage@data$std_libelle)
		DC=as.numeric(report_mig@dc@dc_selectionne)	
		tableau<-report_mig@calcdata[[stringr::str_c("dc_",DC)]][["data"]]
		resum=funstat(tableau=tableau,time.sequence=tableau$debut_pas,taxa,stage,DC,silent=silent)
		fun_write_monthly(report_mig,resum,silent=silent)
	  } # end else
	})