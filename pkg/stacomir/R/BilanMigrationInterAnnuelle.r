#' Class "BilanMigrationInterAnnuelle"
#' 
#' When daily bilan are written in the t_bilanjournalier_bjo table by the 
#' \link{BilanMigration-class} they can be used by this class to display
#' interannual comparisons of migration. Different charts are produced with different
#' period grouping. See \link{fn_EcritBilanJournalier} for details about the writing to the
#' t_bilanjournalier_bjo table.
#' 
#' @include RefAnnee.r
#' @slot dc Object of class \code{\link{RefDC-class}}, the counting device
#' @slot data Object of class \code{"data.frame"} data for bilan lot 
#' @slot taxons An object of class \code{\link{RefTaxon-class}}
#' @slot stades An object of class \code{\link{RefStades-class}}
#' @slot anneeDebut Object of class \code{\link{RefAnnee-class}}. refAnnee allows to choose year of beginning
#' @slot anneeFin Object of class \code{\link{RefAnnee-class}}
#' refAnnee allows to choose last year of the Bilan
#' 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @family Bilan Objects
#' @keywords classes
#' @example inst/examples/bilanMigrationInterAnnuelle_example.R
#' @export
setClass(Class="BilanMigrationInterAnnuelle",representation=
				representation(
						dc="RefDC",
						taxons="RefTaxon",
						stades="RefStades",
						data="data.frame",
						anneeDebut="RefAnnee",
						anneeFin="RefAnnee",
						calcdata="list"
				),
		prototype=prototype(dc=new("RefDC"),
				taxons=new("RefTaxon"),
				stades=new("RefStades"),
				data=data.frame(),
				anneeDebut=new("RefAnnee"),
				anneeFin=new("RefAnnee"),
				calcdata=list()				
		)
)
setValidity("BilanMigrationInterAnnuelle",function(object)
		{
			# if more than one taxa, the connect method will fail when trying to run the write_database for missing data
			# also plots have not been developped accordingly
			rep1=ifelse(length(object@taxons@data$tax_code)==1,TRUE,gettext("BilanMigrationInterannuelle can only take one taxa", domain="R-stacomiR"))
			# same for stage
			rep2=ifelse(length(object@stades@data$std_code)==1,TRUE,gettext("BilanMigrationInterannuelle can only take one stage", domain="R-stacomiR"))
			# multiple DC are allowed
			return(ifelse(rep1 & rep2 , TRUE ,c(1:2)[!c(rep1, rep2)]))
		}   
)


#' connect method for BilanMigrationInterannuelle class
#' 
#' This method will check if the data in the t_bilanjournalier_bjo table has no missing data,
#' if missing the program will load missing data. As a second step,
#' the program will check if the numbers in the table  t_bilanjournalier_bjo differ from those in the database,
#' and propose to re-run the bilanmigration (which has a write_database methode to write daily bilans) for those years.
#' @note We expect different results between daily bilans from the t_bilanjournalier_bjo table and the annual sums
#' from bilanAnnuels for glass eels as those may have been weighted and not only counted. The t_bilanjournalier_bjo table used by BilanMigrationInterAnnuelle
#' contains the sum of glass eel numbers converted from weights and those directly counted. The bilanAnnuels does not.
#' @param object An object of class \link{BilanMigrationInterAnnuelle-class}
#' @param silent Stops messages from being displayed if silent=TRUE, default FALSE
#' @param check Checks that data are corresponding between BilanAnnuels and BilanMigration
#' @return bilanMigrationInterAnnuelle an instantianted object with values filled with user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("connect",signature=signature("BilanMigrationInterAnnuelle"),
		definition=function(object,silent=FALSE,check=TRUE)
		{ 
			# object<-bmi 
			# object<-bmi_cha
			# object<-bmi_des
			# object<-bmi_vichy
			# require(dplyr); require(ggplot2)
			#---------------------------------------------------------------------------------------
			# this function will be run several times if missing data or mismatching data are found
			# later in the script (hence the encapsulation)
			#---------------------------------------------------------------------------------------
			fn_connect<-function(){
				les_annees = (object@anneeDebut@annee_selectionnee):(object@anneeFin@annee_selectionnee)
				tax = object@taxons@data$tax_code
				std = object@stades@data$std_code
				dic= object@dc@dc_selectionne
				requete=new("RequeteODBCwhere")
				requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
				requete@where=paste("WHERE bjo_annee IN ",vector_to_listsql(les_annees)," AND bjo_tax_code='",tax,"' AND bjo_std_code='",std,"' AND bjo_dis_identifiant in",vector_to_listsql(dic),sep="")
				requete@select=paste("SELECT * FROM ",get("sch",envir=envir_stacomi),"t_bilanmigrationjournalier_bjo",sep="")
				requete@order_by=" ORDER BY bjo_jour "
				requete<-stacomirtools::connect(requete)
				data<- stacomirtools::killfactor(requete@query)
			}
			object@data<-fn_connect()
			#browser()
			if (check){
				#----------------------------------------------------------------------
				# Loading a bilan Annuel to compare numbers
				#----------------------------------------------------------------------
				bilanAnnuel<-as(object,"BilanAnnuels")
				bilanAnnuel<-connect(bilanAnnuel)
				
				#----------------------------------------------------------------------
				# MAIN LOOP, there can be several dic
				#----------------------------------------------------------------------
				dic<-object@dc@dc_selectionne
				for (i in 1:length(dic)){
					#i=1
					############################################
					# function creating a table to compare actual counts with those stored in
					# in the t_bilanjournalier_bjo table
					###########################################
					#==========================================
					fn_check<-function(){
						data1<-bilanAnnuel@data[bilanAnnuel@data$ope_dic_identifiant==dic[i],c("effectif","annee")] 
						# data from bilanMigrationInterannuel
						data2<-object@data[object@data$bjo_dis_identifiant==dic[i],]
						data21<-dplyr::select(data2,bjo_annee,bjo_valeur,bjo_labelquantite)
						data22<-dplyr::group_by(data21,bjo_annee,bjo_labelquantite)
						data23<-dplyr::summarize(data22,total=sum(bjo_valeur))
						data24<-dplyr::filter(ungroup(data23),bjo_labelquantite=="Effectif_total")
						data24<-dplyr::select(data24,bjo_annee,total)
						data24<-dplyr::rename(data24,annee=bjo_annee,effectif_bjo=total)
						data124<-merge(data1,data24,all.x=TRUE,all.y=TRUE,by="annee")
						return(data124)
					}
					#==========================================
					# table with 3 columns : annee; effectif; effectif_bjo
					compared_numbers<-fn_check()
					# as we have changed the bilanAnnuel to split data between years
					# some unwanted data might step in outside the year range
					# we correct for that
					compared_numbers<- compared_numbers[
							compared_numbers$annee>=object@anneeDebut@annee_selectionnee&
									compared_numbers$annee<=object@anneeFin@annee_selectionnee,]

#-------------------------------------------------------------------------------------
# First test, if missing data, the program will propose to load the data by running bilanMigration
#-------------------------------------------------------------------------------------
# when data are missing, NA appear in the effectif_bjo column
if (any(is.na(compared_numbers$effectif_bjo))){
	index_missing_years<-which(is.na(compared_numbers$effectif_bjo))
	missing_years<-compared_numbers$annee[index_missing_years]
	if (! silent & length(dic)>1) funout(gettextf("DC with missing values : %s ",dic[i],domain="R-StacomiR"))
	if (! silent) funout(gettextf("Years with no value : %s ",stringr::str_c(missing_years,collapse="; "),domain="R-StacomiR"))
	if (! silent) funout(gettextf("Some years are missing in the t_bilanjournalier_bjo table, loading them now !",domain="R-StacomiR"))
	
	
	for (y in 1:length(missing_years)){
		Y<-missing_years[y]
		bM=new("BilanMigration")
		funout(gettextf("Running Bilanmigration for year %s",Y,domain="R-StacomiR"))
		bM=choice_c(bM,
				dc=dic[i],
				taxons=object@taxons@data$tax_nom_latin,
				stades=object@stades@data$std_code,
				datedebut=stringr::str_c(Y,"-01-01"),
				datefin=stringr::str_c(Y,"-12-31"))
		bM<-charge(bM,silent=silent)
		bM<-connect(bM,silent=silent)
		bM<-calcule(bM,silent=silent)
		if (nrow(bM@data)>0 ){
			# below the argument check_for_bjo is necessary
			# as the write database method from bilanMigration 
			# uses the connect method from BilanMigrationInterAnnuelle and the
			# program runs in endless loops...
			write_database(bM,silent=silent,check_for_bjo=FALSE)
		}
	} # end for loop to write new bilans
	# reloading everything
	object@data<-fn_connect()			
	compared_numbers<-fn_check()		
} # end if any...

#-------------------------------------------------------------------------------------
# Second test, for existing bilan with different numbers, again the data will be witten again
# if the previous test failed, and user confirmed that there was a problem
# the object@data and  compared_numbers are reloaded (see above)
# this test will only be run if the stage is not glass eel, for glass eels it does not make sense
# as some of the "effectif_total" in the bjo table correspond to weights not counts.
#-------------------------------------------------------------------------------------

if (object@taxons@data$tax_code==2038 & object@stades@data$std_code=="CIV"){
	if (! silent) funout(gettext("For glass eel it is not possible to check that data are up to date",domain="R-StacomiR"))
	
} else if (!all(compared_numbers$effectif==compared_numbers$effectif_bjo)){
	index_different_years<-which(round(compared_numbers$effectif)!=round(compared_numbers$effectif_bjo))
	differing_years<-compared_numbers$annee[index_different_years]
	if (! silent) funout(gettextf("Years with values differing between t_bilanjournalier_bjo and bilanAnnuels : %s ",stringr::str_c(differing_years,collapse="; "),domain="R-StacomiR"))
	#==================================
	reload_years_with_error=function(h,...){	
		bM=new("BilanMigration")
		for (Y in differing_years){
			# Y=differing_years[1]
			funout(gettextf("Running Bilanmigration to correct data for year %s",Y))
			bM=choice_c(bM,
					dc=dic[i],
					taxons=object@taxons@data$tax_nom_latin,
					stades=object@stades@data$std_code,
					datedebut=stringr::str_c(Y,"-01-01"),
					datefin=stringr::str_c(Y,"-12-31"))
			bM<-charge(bM,silent=silent)
			bM<-connect(bM,silent=silent)
			bM<-calcule(bM,silent=silent)
			if (nrow(bM@data)>0 ){
				# check for bjo will ensure that previous bilan are deleted
				write_database(bM,silent=silent,check_for_bjo=TRUE)
			}
		} # end for loop to write new bilans
		# the data are loaded again
		object@data<-fn_connect()
		# I need to assign the result one step up (in the environment of the connect function)
		assign("object",object,envir=parent.frame(n=1))
		
	} # end h confirm function
	#==================================
	
	if (!silent){
		choice2<-gWidgets::gconfirm(gettextf("Some data differ between t_bilanjournalier_bjo table, this means that they have been changed after the last bilanmigration was run,  
								do you want to load them again for calculation ?",domain="R-StacomiR"),
				handler=reload_years_with_error)
	} else {
		reload_years_with_error(h=NULL)
	}
} # secondary check
} # end for
} # end check
#-------------------------------------------------------------------------------------
# Final check for data
# index of data already present in the database
#-------------------------------------------------------------------------------------
les_annees=object@anneeDebut@annee_selectionnee:object@anneeFin@annee_selectionnee
index=unique(object@data$bjo_annee) %in% les_annees
# s'il manque des donnees pour certaines annees selectionnnees" 
if (!silent){
	if (length(les_annees[!index])>0) 
	{
		funout(paste(gettext("Attention, there is no migration summary for this year\n",domain="R-stacomiR"),
						paste(les_annees[!index],collapse=","),gettext(", this taxon and this stage (BilanMigrationInterAnnuelle.r)\n",domain="R-stacomiR")))
	} # end if    
	
	# si toutes les annees sont presentes
	if (length(les_annees[index])>0){
		funout(paste(gettext("Annual migrations query completed",domain="R-stacomiR"),
						paste(les_annees[index],collapse=","), "\n")) 
	}  
}
return(object)
}
)

#' supprime method for BilanMigrationInterannuelle class
#' @param object An object of class \link{BilanMigrationInterAnnuelle-class}
#' @return nothing
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export

setMethod("supprime",signature=signature("BilanMigrationInterAnnuelle"),
		definition=function(object)
		{ 
			# recuperation des annees taxons et stades concernes
			les_annees = (object@anneeDebut@annee_selectionnee):(object@anneeFin@annee_selectionnee)
			tax = object@taxons@data$tax_code
			std = object@stades@data$std_code
			dic= object@dc@dc_selectionne
			requete=new("RequeteODBCwhere")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@select=stringr::str_c("DELETE from ",get("sch",envir=envir_stacomi),"t_bilanmigrationjournalier_bjo ")
			requete@where=paste("WHERE bjo_annee IN (",paste(les_annees,collapse=","),") AND bjo_tax_code='",tax,"' AND bjo_std_code='",std,"' AND bjo_dis_identifiant=",dic,sep="")
			invisible(utils::capture.output(requete<-stacomirtools::connect(requete)))
			
			requete=new("RequeteODBCwhere")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@select=stringr::str_c("DELETE from ",get("sch",envir=envir_stacomi),"t_bilanmigrationmensuel_bme ")
			requete@where=paste("WHERE bme_annee IN (",paste(les_annees,collapse=","),") AND bme_tax_code='",tax,"' AND bme_std_code='",std,"' AND bme_dis_identifiant=",dic,sep="")
			invisible(utils::capture.output(requete<-stacomirtools::connect(requete)))
			
			return(invisible(NULL))
		}

)

#' loading method for BilanMigrationInterannuelle class
#' @param object An object of class \link{BilanMigrationInterAnnuelle-class}
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return An object of class  \link{BilanMigrationInterAnnuelle-class}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("charge",signature=signature("BilanMigrationInterAnnuelle"),
		definition=function(object,silent=FALSE)
		{ 
			bilanMigrationInterAnnuelle<-object
			if (exists("refDC",envir_stacomi)) {
				bilanMigrationInterAnnuelle@dc<-get("refDC",envir_stacomi)
			} else {
				funout(gettext("You need to choose a counting device, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("refTaxon",envir_stacomi)) {
				bilanMigrationInterAnnuelle@taxons<-get("refTaxon",envir_stacomi)
			} else {      
				funout(gettext("You need to choose a taxa, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("refStades",envir_stacomi)){
				bilanMigrationInterAnnuelle@stades<-get("refStades",envir_stacomi)
			} else 
			{
				funout(gettext("You need to choose a stage, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}
			if (exists("anneeDebut",envir_stacomi)) {
				bilanMigrationInterAnnuelle@anneeDebut<-get("anneeDebut",envir_stacomi)
			} else {
				funout(gettext("You need to choose the starting year\n",domain="R-stacomiR"),arret=TRUE)
			}  	
			if (exists("anneeFin",envir_stacomi)) {
				bilanMigrationInterAnnuelle@anneeFin<-get("anneeFin",envir_stacomi)
			} else {
				funout(gettext("You need to choose the ending year\n",domain="R-stacomiR"),arret=TRUE)
			}
			# this will test that only one taxa and one stage have been loaded (multiple dc are allowed)
			validObject(bilanMigrationInterAnnuelle)
			assign("bilanMigrationInterAnnuelle",bilanMigrationInterAnnuelle,envir_stacomi)
			if (!silent) funout(gettext("Writing bilanMigrationInterannuelle in the environment envir_stacomi : write bmi=get('bilanMigrationInterannuelle',envir_stacomi) ",domain="R-stacomiR"))
			
			return(bilanMigrationInterAnnuelle)
		}
)

#' command line interface for BilanMigrationInterAnnuelle class
#' @param object An object of class \link{BilanMigrationInterAnnuelle-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,RefDC-method}
#' @param taxons Either a species name in latin or the SANDRE code for species (ie 2038=Anguilla anguilla),
#' it should match the ref.tr_taxon_tax referential table in the stacomi database, see \link{choice_c,RefTaxon-method}
#' @param stades A stage code matching the ref.tr_stadedeveloppement_std table in the stacomi database, see \link{choice_c,RefStades-method}
#' @param anneedebut The starting the first year, passed as charcter or integer
#' @param anneefin the finishing year
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return An object of class \link{BilanMigrationInterAnnuelle-class}
#' The choice_c method fills in the data slot for classes \link{RefDC-class}, \link{RefTaxon-class}, \link{RefStades-class} and two slots of \link{RefAnnee-class}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("choice_c",signature=signature("BilanMigrationInterAnnuelle"),definition=function(object,
				dc,
				taxons,
				stades,			
				anneedebut,
				anneefin,
				silent=FALSE){
			# code for debug using example
			#bilanMigrationInterAnnuelle<-bmi;dc=c(16);taxons="Anguilla anguilla";stades=c("AGJ");anneedebut="1984";anneefin="2016"
			bilanMigrationInterAnnuelle<-object
			bilanMigrationInterAnnuelle@dc=charge(bilanMigrationInterAnnuelle@dc)
			# loads and verifies the dc
			# this will set dc_selectionne slot
			bilanMigrationInterAnnuelle@dc<-choice_c(object=bilanMigrationInterAnnuelle@dc,dc)
			# only taxa present in the bilanMigration are used
			bilanMigrationInterAnnuelle@taxons<-charge_avec_filtre(object=bilanMigrationInterAnnuelle@taxons,bilanMigrationInterAnnuelle@dc@dc_selectionne)			
			bilanMigrationInterAnnuelle@taxons<-choice_c(bilanMigrationInterAnnuelle@taxons,taxons)
			bilanMigrationInterAnnuelle@stades<-charge_avec_filtre(object=bilanMigrationInterAnnuelle@stades,bilanMigrationInterAnnuelle@dc@dc_selectionne,bilanMigrationInterAnnuelle@taxons@data$tax_code)	
			bilanMigrationInterAnnuelle@stades<-choice_c(bilanMigrationInterAnnuelle@stades,stades)
			# depending on objetBilan the method will load data and issue a warning if data are not present
			# this is the first step, the second verification will be done in method connect
			bilanMigrationInterAnnuelle@anneeDebut<-charge(object=bilanMigrationInterAnnuelle@anneeDebut,
					objectBilan="BilanMigrationInterAnnuelle")
			bilanMigrationInterAnnuelle@anneeDebut<-choice_c(object=bilanMigrationInterAnnuelle@anneeDebut,
					nomassign="anneeDebut",
					annee=anneedebut, 
					silent=silent)
			bilanMigrationInterAnnuelle@anneeFin@data<-bilanMigrationInterAnnuelle@anneeDebut@data
			bilanMigrationInterAnnuelle@anneeFin<-choice_c(object=bilanMigrationInterAnnuelle@anneeFin,
					nomassign="anneeFin",
					annee=anneefin, 
					silent=silent)
			assign("bilanMigrationInterAnnuelle",bilanMigrationInterAnnuelle,envir=envir_stacomi)
			return(bilanMigrationInterAnnuelle)
		})


#' calcule method for BilanMigrationInterannuelle
#' 
#' @param object An object of class \code{\link{BilanMigrationInterannuelle-class}}
#' @param silent Boolean, if TRUE, information messages are not displayed, only warnings and errors
#' @param timesplit One of "day","week","month","2 weeks", "jour","semaine","quinzaine","mois"
#' @note The class BilanMigration does not handle escapement rates nor 
#' 'devenir' i.e. the destination of the fishes.
#' @return BilanMigration with calcdata slot filled.
#' @export
setMethod("calcule",signature=signature("BilanMigrationInterAnnuelle"),definition=function(object,silent=FALSE,timesplit="mois"){ 
			bilanMigrationInterAnnuelle<-object	
			#bilanMigrationInterAnnuelle<-bmi_vichy;silent=FALSE;timesplit="mois"
			#require(dplyr)
			if (!timesplit%in%c("jour","day","month","mois","week","semaine","quinzaine","2 weeks")) stop (
						stringr::str_c("timesplit should be one of :","jour ","day ","month ","mois ","week ","semaine ","month ","mois ","quinzaine ","2 weeks "))
			# back to french labels for consistency with fundat code
			timesplit<-switch(timesplit,"day"="jour_365","jour"="jour_365","week"="semaine","month"="mois","2 weeks"="quinzaine",timesplit)
			# there should be just one station, this will be tested
			station<-bilanMigrationInterAnnuelle@dc@station
			taxon<-bilanMigrationInterAnnuelle@taxons@data$tax_code
			stade<-bilanMigrationInterAnnuelle@stades@data$std_code
			if(length(unique(bilanMigrationInterAnnuelle@dc@station))!=1) stop("You have more than one station in the Bilan, the dc from the Bilan should belong to the same station")
			if(nrow(bilanMigrationInterAnnuelle@data)==0) stop("No rows in bilanMigrationInterannuelle@data, nothing to run calculations on")
			
			datadic<-bilanMigrationInterAnnuelle@data[
					bilanMigrationInterAnnuelle@data$bjo_labelquantite=="Effectif_total",]
			datadic<-funtraitementdate(datadic, nom_coldt = "bjo_jour", jour_an = TRUE, quinzaine = TRUE)
			datadic<-killfactor(datadic)
			# here this code avoids the following problem :Error: (list) object cannot be coerced to type 'double'
			# data is subsetted for columns not containing bjo, and apply is run on each of the column
			datadic[,colnames(datadic)[!grepl("bjo_",colnames(datadic))]]<-apply(X=datadic[,colnames(datadic)[!grepl("bjo_",colnames(datadic))]],MARGIN=2,FUN=function(X) as.numeric(X))
			fnquant<-function(data, timesplit="jour_365",probs=c(0, .05, .5, .95, 1)){
				res<-Hmisc::wtd.quantile(x=data[,timesplit],
						weights=abs(data$bjo_valeur),
						probs=probs)
				return(res)
			}
			fnquant(datadic[datadic$bjo_annee==2012,],"mois")
			# for some reasons this code does not work : Error in x + weights : non-numeric argument to binary operator
#			dat<-dplyr::select_(datadic,"bjo_annee","bjo_dis_identifiant","bjo_tax_code","bjo_std_code","bjo_valeur",timesplit)%>%
#					dplyr::group_by_("bjo_annee","bjo_tax_code","bjo_std_code")			
#			dat2<-dat%>% do(res=fnquant(data=.,timesplit=timesplit,probs=c(0, .05, .5, .95, 1)))
#			dat3<-dat2%>%summarize(bjo_annee,bjo_tax_code,bjo_std_code,Q0=res[[1]],Q5=res[[2]],
#					Q50=res[[3]],Q95=res[[4]],Q100=res[[5]])	
			# this simple code will do :
			dat<-list()
			for (i in unique(datadic$bjo_annee)){
				dat[[i]]<-fnquant(data=datadic[datadic$bjo_annee==i,],timesplit=timesplit)
			}
			dat<-as.data.frame(matrix(unlist(dat),ncol=5,byrow=TRUE))
			colnames(dat)<-c("Q0","Q5","Q50","Q95","Q100")
			dat$d90<-dat$Q95-dat$Q5
			dat$year=unique(datadic$bjo_annee)
			dat$taxon=taxon
			dat$stade=stade
			dat$station=unique(station)	
			dat$timesplit=timesplit
			dat<-dat[,c("year","station","taxon","stade","Q0","Q5","Q50","Q95","Q100","d90","timesplit")]							
			bilanMigrationInterAnnuelle@calcdata<-dat				
			return(bilanMigrationInterAnnuelle)
		})	

#' statistics per time period
#' 
#' function called for bilamMigrationInterannelle objects renames columns
#' replaces nulls, and calculates reports with time period larger than day
#' 
#' @param dat a data frame
#' @param annee The year to exclude from the historical series (it will be plotted against the historical series)
#' @param timesplit "week" "2 week" "month" as provided to seq.POSIXT, default NULL
#' @return a data frame with mean, max, and min calculated for each timesplit
#' @export
#' @seealso \code{\linkS4class{Bilan_poids_moyen}}
fundat=function(dat,annee=NULL,timesplit=NULL)
{
	
	if(nrow(dat)>0)
	{
		# ci dessous les calculs s'appliquent bien aux jours
		# remplacement des valeurs manquantes par des zeros par creation d'une sequence journaliere
		dat<-dat[dat$bjo_labelquantite=="Effectif_total",]
		dat<-stacomirtools::chnames(dat,c("bjo_annee","bjo_jour","bjo_labelquantite","bjo_valeur"),    c("annee","jour","labelquantite","valeur"))
		dat=dat[,c("annee","jour","valeur")] 
		if (!is.null(annee)){
			dat<-dat[dat$annee!=annee,]
		}
		dat$jour=trunc.POSIXt(dat$jour, digits='days')
		dat$jour = as.Date(strptime(strftime(dat$jour,'2000-%m-%d'),'%Y-%m-%d')) 
		
		
		# ci dessous calcul des sommes par semaine mois... Comme trunk.POSIXt ou floor ne prend pas 
		# la valeur week on est oblige de faire avec seq.POSIXt et calculer avec une boucle !
		if (!is.null(timesplit)){
			seq_timesplit= seq.POSIXt(from=strptime("2000-01-01",format='%Y-%m-%d'),
					to=strptime("2000-12-31",format='%Y-%m-%d'),
					by=getvalue(new("Refperiode"),timesplit))
			seq_timesplit<-as.Date(trunc(seq_timesplit, digits='days'))
			# utilise la classe Refperiode pour avoir la correspondance entre le nom francais et la variable utilisee par seq.POSIXt
			#datc=data.frame(rep(seq_timesplit,length(unique(dat$annee))),sort(rep(unique(dat$annee),length(seq_timesplit))))  # dataframe pour cumuls par periodes
			#colnames(datc)<-c(timesplit,"annee")
			# calcul des sommes par annee et par periode
			dat[,timesplit]<-dat$jour # pour avoir le format sinon renvoit un numerique
			# ci dessous on remplace une double boucle par un truc plus rapide
			for (j in 1:(length(seq_timesplit)-1)){
				dat[dat$jour>=seq_timesplit[j]&dat$jour<seq_timesplit[j+1],timesplit]<-seq_timesplit[j]
			}
			dat[dat$jour>=seq_timesplit[length(seq_timesplit)],timesplit]<-seq_timesplit[length(seq_timesplit)]
			dat[,"interv"]<-paste(dat[,"annee"],dat[,timesplit]) # on veut les valeurs uniques par annee et timesplit
			res<-tapply(dat$valeur,dat[,"interv"],sum,na.rm=TRUE)
			datc<-data.frame("annee"=substr(names(res),1,4),timesplit=substr(names(res),5,15),"valeur"=as.numeric(res))
			colnames(datc)[2]<-timesplit
			dat<-datc 
			rm(datc)
		} else {
			# si nul on remplace par jour pour generer le script en dessous
			timesplit="jour"
			jour2000=as.Date(seq.POSIXt(from=strptime("2000-01-01",format='%Y-%m-%d'),
							to=strptime("2000-12-31",format='%Y-%m-%d'), by="day"))
			for (j in unique(dat$annee)){
				# les jours qui n'ont pas de bilan journalier pour ce jour sont rajoutes avec zero
				jour2000restant<-jour2000[!jour2000 %in% dat[dat$annee==j,"jour"]]
				dat0=data.frame("jour"=jour2000restant,"annee"=j, "valeur"=NA)
				dat=rbind(dat,dat0)
			} # end for
		}
		# calcul des valeurs min et max et moyenne en fonction de la coupure (jour, semaine,quinzaine, mois)
		
		maxdat<-suppressWarnings(tapply(dat$valeur,as.character(dat[,timesplit]),max,na.rm=TRUE))
		mindat<-suppressWarnings(tapply(dat$valeur,as.character(dat[,timesplit]),min,na.rm=TRUE))
		meandat<-suppressWarnings(tapply(dat$valeur,as.character(dat[,timesplit]),mean,na.rm=TRUE))
		datsummary<-data.frame("maxtab"=maxdat,"mintab"=mindat,"moyenne"=meandat)
		datsummary<-datsummary[!is.infinite(datsummary$maxtab),]# the minimum and max of empty set are -Inf and Inf respectively
		datsummary[,timesplit]<-names(maxdat)[!is.infinite(maxdat)]
		dat[,timesplit]<-as.character(dat[,timesplit])
		dat<-merge(dat,datsummary,by=timesplit)
		dat[,timesplit]<-as.POSIXct(strptime(dat[,timesplit],format='%Y-%m-%d')) # le format Posixct est necessaire pour les ggplot
		rm(maxdat,mindat,meandat)
		dat<-dat[order(dat$annee,dat[,timesplit]),]
		# renvoit la premiere occurence qui correspond, pour n'importe quel jour min, max et moyenne sont OK
		return(dat)
		
	} else   {  # arret avec erreur
		funout(gettext("Attention : you have to complete a migration summary for at least one of the selected year before launching a inter-annual summary",domain="R-stacomiR"),arret=TRUE)
	}    # end else
}	

#' Plot method for BilanMigrationInterannuelle
#' 
#' Several of these plots are scaled against the same year, ie the comparison is based on
#' year 2000, meaning that day 1 would correspond to the first date of 2000,  which is also a
#' saturday, the last day of the week.
#' @param x An object of class BilanMigrationInterannuelle
#' @param plot.type Default standard
#' @param timesplit Used for plot.type barchart or dotplot, Default mois (month) other possible values are semaine (week), quinzaine (2 weeks),
#' english values within parenthesis are also accepted.
#' @param silent Stops displaying the messages.
#' \itemize{
#' 		\item{plot.type="line": one line per daily bilanmigration}
#' 		\item{plot.type="standard": the current year is displayed against a ribbon of historical values"}
#' 		\item{plot.type="density": creates density plot to compare seasonality, data computed by 15 days period}
#' 		\item{plot.type="step" : creates step plots to compare seasonality, the year chosen in the interface is the
#' latest if silent=TRUE, or it can be selected in the droplist. It is highlighted against the other with a dotted line}
#' 		\item{plot.type="barchart": comparison of daily migration of one year against periodic migration for the other years available in the chronicle,
#' 									different periods can be chosen with argument timesplit}
#' 		\item{plot.type="pointrange": Pointrange graphs, different periods can be chosen with argument timesplit}
#'      \item{plot.type="seasonal": plot to display summary statistics about the migration period}
#' }
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases plot.BilanMigrationInterAnnuelle plot.bilanMigrationInterAnnuelle plot.bmi
#' @seealso \link{BilanMigrationInterAnnuelle-class} for examples
#' @export
setMethod("plot",signature(x = "BilanMigrationInterAnnuelle", y = "missing"),definition=function(x, plot.type="standard",timesplit="mois",silent=FALSE){ 
			#bilanMigrationInterAnnuelle<-bmi
			bilanMigrationInterAnnuelle<-x
			if (!timesplit%in%c("jour","day","month","mois","week","semaine","month","mois","quinzaine","2 weeks")) stop (
						stringr::str_c("timesplit should be one of :","jour ","day ","month ","mois ","week ","semaine ","month ","mois ","quinzaine ","2 weeks "))
			# back to french labels for consistency with fundat code
			timesplit<-switch(timesplit,"day"="jour","week"="semaine","month"="mois","2 weeks"="quinzaine",timesplit)
			# plot.type="line";require(ggplot2)
			if(nrow(bilanMigrationInterAnnuelle@data)>0){
				if (plot.type=="line"){
					# TODO traitement des poids
					dat=bilanMigrationInterAnnuelle@data        
					dat<-dat[dat$bjo_labelquantite=="Effectif_total",]
					dat<-stacomirtools::chnames(dat,c("bjo_annee","bjo_jour","bjo_labelquantite","bjo_valeur"),    c("annee","jour","labelquantite","valeur"))
					# we need to choose a date, every year brought back to 2000
					dat$jour = as.POSIXct(strptime(strftime(dat$jour,'2000-%m-%d %H:%M:%S'),format='%Y-%m-%d %H:%M:%S'),tz="GMT")
					dat$annee=as.factor(dat$annee)					
					dat=stacomirtools::killfactor(dat)					
					titre=paste(gettext("Migration for years "),
							paste(min(dat$annee),max(dat$annee), collapse=":"),
							", ",
							bilanMigrationInterAnnuelle@dc@data$dis_commentaires[bilanMigrationInterAnnuelle@dc@data$dc==bilanMigrationInterAnnuelle@dc@dc_selectionne])
					soustitre=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin, ", ", bilanMigrationInterAnnuelle@stades@data$std_libelle, sep="")
					g<-ggplot(dat,aes(x=jour,y=valeur))
					g<-g+geom_line(aes(color=annee))+ labs(title=paste(titre, "\n", soustitre))+
							scale_x_datetime(name="date")
					print(g)
					assign("g",g,envir=envir_stacomi)
					if (!silent) funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get('g',envir_stacomi)\n",domain="R-stacomiR"))
					#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				} else if (plot.type=="standard"){
					dat=bilanMigrationInterAnnuelle@data
					if (silent==FALSE){
						the_choice=as.numeric(select.list(choices=as.character(unique(dat$bjo_annee )[order(unique(dat$bjo_annee ))]),
										preselect=as.character(max(dat$bjo_annee )),
										"choice annee",multiple=FALSE))
					} else {
						the_choice=max(dat$bjo_annee)
					}
					# dataset for current year
					dat0=fundat(dat,annee=NULL,timesplit=NULL)
					dat=fundat(dat,annee=the_choice,timesplit=NULL)				
					dat=dat[dat$moyenne!=0,] # pour des raisons graphiques on ne garde pas les effectifs nuls generes par fundat
					newdat=dat[match(unique(as.character(dat$jour)),as.character(dat$jour)),]
					newdat=newdat[order(newdat$jour),] # pour avoir les range sur l'ensemble des valeurs dispo et pas seult l'annee en cours
					amplitude=paste(min(as.numeric(as.character(dat$annee))),"-",max(as.numeric(as.character(dat$annee))),sep="")        
					if (length(the_choice)>0) { 
						# le layout pour l'affichage des graphiques
						vplayout <- function(x, y) { grid::viewport(layout.pos.row = x, layout.pos.col = y)   }
						grid::grid.newpage()
						grid::pushViewport(grid::viewport(layout = grid::grid.layout(length(the_choice),1,just="center")))   
						amplitudechoice<-paste(the_choice,'/',amplitude)
						tmp <- dat0[as.numeric(as.character(dat0$annee))==the_choice,]
						tmp$annee=as.character(tmp$annee)
						g <- ggplot(newdat,aes(x=jour))
						g <- g+geom_ribbon(aes(ymin=mintab, ymax=maxtab,fill="amplitude"),color="grey20",alpha=0.5)
						g <- g+geom_bar(aes(y=valeur,fill=I("orange")),position="dodge",stat="identity",color="grey20",alpha=0.8,data=tmp)
						g<- g+ scale_fill_manual(name=eval(amplitudechoice), values=c("#35789C","orange"),
								labels = c("amplitude historique",the_choice))
						#g <- g+geom_point(aes(y=valeur,col=annee),data=tmp,pch=16,size=1)  
						# moyenne interannuelle
						
						g <- g+	geom_line(aes(y=moyenne,col=I("#002743")),data=newdat)
						g <- g+ geom_point(aes(y=moyenne,col=I("#002743")),size=1.2,data=newdat)		           
						g <- g+ scale_colour_manual(name=eval(amplitudechoice),values=c("#002743"),
										labels=c(stringr::str_c("Moyenne interannuelle\n",amplitude)))+
								guides(fill = guide_legend(reverse=TRUE))
						g <- g+labs(title=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,",",bilanMigrationInterAnnuelle@stades@data$std_libelle,unique(as.character(tmp$annee)),"/",amplitude))
						g <- g+scale_x_datetime(name="date",date_breaks="months",date_minor_breaks="weeks", date_labels="%d-%m")
						g<-g+theme_bw()+ theme(legend.key = element_blank())
						print(g, vp=vplayout(1,1)) 
						assign(paste("g",1,sep=""),g,envir_stacomi)
						if (!silent) funout(gettextf("Writing the graphical object into envir_stacomi environment : write g=get(gi\",envir_stacomi) with i=%s",paste(1:length(the_choice),collapse=",")))
						
						
					} # end if					
					#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				} else if (plot.type=="step"){
					dat=bilanMigrationInterAnnuelle@data
					dat=fundat(dat)
					#dat=dat[order(dat$annee,dat$jour),] 
					dat$valeur[is.na(dat$valeur)]<-0 # sinon si il ne reste qu'une ligne peut planter
					if (silent==FALSE){
						the_choice=select.list(choices=as.character(unique(dat$annee)),preselect=as.character(max(dat$annee)),"choice annee",multiple=FALSE)
					} else {
						the_choice=max(as.numeric(as.character(dat$annee)))
					}
					amplitude=paste(min(as.numeric(as.character(dat$annee))),"-",max(as.numeric(as.character(dat$annee))),sep="")      
					#################
					# Calcul des cumsum
					###################
					
					#dat$valeur[dat$valeur<0]<-0
					for (an in unique(dat$annee)){
						# an=as.character(unique(dat$annee)) ;an<-an[1]
						dat[dat$annee==an,"cumsum"]<-cumsum(dat[dat$annee==an,"valeur"])
						dat[dat$annee==an,"total_annuel"]<-max(dat[dat$annee==an,"cumsum"])          
					}
					dat$cumsum=dat$cumsum/dat$total_annuel
					dat$jour=as.Date(dat$jour)
					dat$annee=as.factor(dat$annee)
					# bug, enleve les annees avec seulement une ligne
					
					#################
					# Graphique
					###################
					
					g <- ggplot(dat,aes(x=jour,y=cumsum))
					tmp<-dat[as.numeric(as.character(dat$annee))==as.numeric(the_choice),]
					g <- g+geom_step(aes(col=annee,size=total_annuel))
					g <- g+geom_step(data=tmp,col="black",lty=2)
					g<-g+labs(title=gettextf("%s %s, Cumulated numbers %s",bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,bilanMigrationInterAnnuelle@stades@data$std_libelle,amplitude))
					g<-g+scale_y_continuous(name=gettext("Annual migration percentage",domain="R-stacomiR"))
					g<-g+scale_x_date(name=gettext("date",domain="R-stacomiR"),date_breaks="months", 
							date_minor_breaks="weeks", 
							date_labels="%b",
							limits=range(dat[dat$valeur>0&dat$cumsum!=1,"jour"]))# date 
					g<-g+scale_colour_hue(name=gettext("year",domain="R-stacomiR"),l=70, c=150)# annee
					print(g) 
					assign("g",g,envir_stacomi)
					if (!silent) funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get('g',envir_stacomi)\n",domain="R-stacomiR"))	
					#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				} else if (plot.type=="barchart"){	
					dat=bilanMigrationInterAnnuelle@data
					if (silent==FALSE){
						the_choice=select.list(choices=as.character(unique(dat$bjo_annee)),preselect=as.character(max(dat$bjo_annee)),"choice annee",multiple=FALSE)
					} else {
						the_choice=max(as.numeric(as.character(dat$bjo_annee)))
					}
					dat0=fundat(dat,timesplit=timesplit)
					dat=fundat(dat,annee=the_choice,timesplit=timesplit)
					prepare_dat<-function(dat){
						dat=dat[order(dat$annee,dat[,timesplit]),]
						dat$annee=as.factor(dat$annee)
						dat$keeptimesplit<-dat[,timesplit]
						if(timesplit=="mois") {
							dat[,timesplit]<-strftime(dat[,timesplit],format="%m")							
						} else if (timesplit=="quinzaine") {
							dat[,timesplit]<-strftime(dat[,timesplit],format="%m/%d")
						} else {
							dat[,timesplit]<-strftime(dat[,timesplit],format="%W")
						} 
						dat[,timesplit]<-as.factor(dat[,timesplit])
						# we only keep one per week
						newdat=dat[match(unique(dat[,timesplit]),dat[,timesplit]),]
						newdat=newdat[order(newdat[,"keeptimesplit"]),] # pour avoir les range sur l'ensemble des valeurs dispo et pas seult l'annee en cours
						# here change 12/2012 the geom_crossbar now needs a factor, label change according to timesplit
						newdat[,timesplit]<-as.factor(newdat[,timesplit])
						levels(newdat[,timesplit])<-newdat[,timesplit] # to have the factor in the right order from january to dec
						return(newdat)
					}
					amplitude=paste(min(as.numeric(as.character(dat$annee))),"-",max(as.numeric(as.character(dat$annee))),sep="") 
					
					newdat<-prepare_dat(dat)
					newdat0<-prepare_dat(dat0)
					if (length(the_choice)>0) { 
						# le layout pour l'affichage des graphiques
						vplayout <- function(x, y) { grid::viewport(layout.pos.row = x, layout.pos.col = y)   }
						grid::grid.newpage()
						grid::pushViewport(grid::viewport(layout = grid::grid.layout(length(the_choice),1,just="center")))   
						
						selection=as.numeric(as.character(dat0$annee))==as.numeric(the_choice) 
						tmp <- dat0[selection,]
						tmp[tmp$valeur>=tmp$moyenne,"comp"]<-">=moy"
						tmp[tmp$valeur<tmp$moyenne,"comp"]<-"<moy"
						options(warn = -1)
						tmp[tmp$valeur==tmp$maxtab,"comp"]<-"max"
						tmp[tmp$valeur==tmp$mintab,"comp"]<-"min"
						options(warn = 0)
						tmp[tmp$moyenne==0,"comp"]<-"0"
						
						tmp$annee=as.factor(as.numeric(as.character(tmp$annee)))
						if(timesplit=="mois") {
							tmp[,timesplit]<-strftime(tmp[,timesplit],format="%m")							
						} else if (timesplit=="quinzaine") {
							tmp[,timesplit]<-strftime(tmp[,timesplit],format="%m/%d")
						} else {
							tmp[,timesplit]<-strftime(tmp[,timesplit],format="%W")
						} 
						tmp[,timesplit]<-as.factor(tmp[,timesplit])
						tmp[!tmp[,timesplit]%in%newdat[,timesplit],"comp"]<-"?"
						newdat$comp<-NA
						
						g <- ggplot(tmp,aes_string(x=timesplit,y="valeur"))
						g <- g+geom_crossbar(data=newdat,aes_string(x=timesplit, 
										y="moyenne",
										ymin="mintab",ymax="maxtab"),fill="grey60",alpha=0.5,size=0.5,fatten=3,col="grey60")
						g <- g+geom_bar(stat="identity",aes_string(ymin="valeur",ymax="valeur",col="comp"),fill=NA,width=0.6)
						g <- g+geom_bar(stat="identity",aes_string(ymin="valeur",ymax="valeur",fill="comp"),alpha=0.5,width=0.6)
						#g <- g+scale_x_date(name=paste("mois"),breaks="month",minor_breaks=getvalue(new("Refperiode"),label=date_format("%b"),timesplit))
						#lim=as.POSIXct(c(Hmisc::trunc.POSIXt((min(tmp[tmp$com!="0",timesplit])),"month")-delai,
						#				Hmisc::ceil.POSIXt((max(tmp[tmp$com!="0",timesplit])),"month")+delai)) 
						# pb the limit truncs the value
						g <- g+ylab("effectif")
						cols <- c("max" = "#000080","min" = "#BF0000",">=moy" = "darkgreen", "<moy" = "darkorange","hist_mean"="black","hist_range"="grey","?"="darkviolet")
						fills <- c("max" = "blue","min" = "red",">=moy" = "green", "<moy" = "orange","hist_mean"="black","hist_range"="grey","?"="violet")
						
						g <- g+scale_colour_manual(name=the_choice,values=cols,limits=c("min","max","<moy",">=moy","hist_mean","hist_range","?"))
						g <- g+scale_fill_manual(name=the_choice,values=fills,limits=c("min","max","<moy",">=moy","hist_mean","hist_range","?"))
						
						g<-g+labs(title=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,",",
										bilanMigrationInterAnnuelle@stades@data$std_libelle,
										", bilan par",timesplit,unique(as.character(tmp$annee)),"/",amplitude))
						g<-g+ theme_minimal() 
						print(g, vp=vplayout(1,1)) 
						assign(paste("g",1,sep=""),g,envir_stacomi)
						if (!silent) funout(gettextf("\"Writing the graphical object into envir_stacomi environment : write g=get(gi\",envir_stacomi) with \" i=%s",paste(1:length(the_choice),collapse=",")))
						
					} # end if
					
					#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				} else if (plot.type=="pointrange"){	
					# below before several plots could be made, it's no longer the case
					# as I remove the chosen year from the observation (reference) set
					dat=bilanMigrationInterAnnuelle@data
					
					if (silent==FALSE){
						the_choice=select.list(choices=as.character(unique(dat$bjo_annee)),preselect=as.character(max(dat$bjo_annee)),"choice annee",multiple=FALSE)
					} else {
						the_choice=max(dat$bjo_annee)
					}
					dat0=fundat(dat,timesplit=timesplit)
					dat=fundat(dat,annee=the_choice,timesplit=timesplit)
					dat$annee=as.factor(dat$annee) 
					dat=dat[order(dat$annee,dat[,timesplit]),]
					dat$keeptimesplit<-dat[,timesplit]
					if(timesplit=="mois") {
						dat[,timesplit]<-strftime(dat[,timesplit],format="%m")
					} else if (timesplit=="quinzaine") {
						dat[,timesplit]<-strftime(dat[,timesplit],format="%m/%d")
					} else {
						dat[,timesplit]<-strftime(dat[,timesplit],format="%W")
					} 
					dat[,timesplit]<-as.factor(dat[,timesplit])
					
					# dat=dat[dat$moyenne!=0,] # pour des raisons graphiques on ne garde pas les effectifs nuls generes par fundat
					newdat=dat[match(unique(dat[,timesplit]),dat[,timesplit]),]
					newdat=newdat[order(newdat[,"keeptimesplit"]),] # il peut y avoir des annees pour le calcul de range qui s'ajoutent 
					# et viennent d'autres annees, il faut donc reordonner.
					
					
					amplitude=paste(min(as.numeric(as.character(dat$annee))),"-",max(as.numeric(as.character(dat$annee))),sep="") 
					
					
					if (length(the_choice)>0) { 
						# le layout pour l'affichage des graphiques
						vplayout <- function(x, y) { grid::viewport(layout.pos.row = x, layout.pos.col = y)   }
						grid::grid.newpage()
						grid::pushViewport(grid::viewport(layout = grid::grid.layout(length(the_choice),1,just="center")))   
						
						selection=as.numeric(as.character(dat0$annee))==as.numeric(the_choice) 
						tmp <- dat0[selection,]
						tmp[tmp$valeur>=tmp$moyenne,"comp"]<-">=moy"
						tmp[tmp$valeur<tmp$moyenne,"comp"]<-"<moy"
						options(warn = -1)
						tmp[tmp$valeur==tmp$maxtab,"comp"]<-"max"
						tmp[tmp$valeur==tmp$mintab,"comp"]<-"min"
						options(warn = 0)
						tmp[tmp$moyenne==0,"comp"]<-"0"
						tmp$annee=as.factor(as.numeric(as.character(tmp$annee)))
						if(timesplit=="mois") {
							tmp[,timesplit]<-strftime(tmp[,timesplit],format="%m")
						} else if (timesplit=="quinzaine") {
							tmp[,timesplit]<-strftime(tmp[,timesplit],format="%m/%d")
						} else {
							tmp[,timesplit]<-strftime(tmp[,timesplit],format="%W")
						} 
						tmp[,timesplit]<-as.factor(tmp[,timesplit])
						tmp[!tmp[,timesplit]%in%newdat[,timesplit],"comp"]<-"?"
						newdat$comp<-NA
						g <- ggplot(tmp,aes_string(x=timesplit,y="valeur"))
						g<-g+geom_dotplot(aes_string(x=timesplit, y="valeur"),data=dat,stackdir = "center",binaxis = "y",position = "dodge",dotsize = 0.5,fill="wheat") #position = "dodge",dotsize = 0.4,alpha=0.5,binwidth = 1.5
						g<-g+geom_pointrange(data=newdat,aes_string(x=timesplit, y="moyenne",ymin="mintab",ymax="maxtab"),alpha=1,size=0.8)
						g<-g+geom_bar(stat="identity",aes_string(y="valeur",fill="comp"),alpha=0.6)			
						g <- g+scale_y_continuous(name="effectif")
						cols <- c("max" = "blue","min" = "red",">=moy" = "darkgreen", "<moy" = "darkorange","0"="grey10","?"="darkviolet")
						g <- g+scale_fill_manual(name=the_choice,values=cols)
						g<-g+labs(title=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,",",bilanMigrationInterAnnuelle@stades@data$std_libelle,", bilan par",timesplit,unique(as.character(tmp$annee)),"/",amplitude))
						g<-g+ theme_minimal() 
						print(g, vp=vplayout(1,1)) 
						assign(paste("g",1,sep=""),g,envir_stacomi)
						if (!silent) funout(gettextf("\"Writing the graphical object into envir_stacomi environment : write g=get(gi\",envir_stacomi) with \" i=%s",paste(1:length(the_choice),collapse=",")))
						
					} # end if
					#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				} else if (plot.type=="density"){	
					if(nrow(bilanMigrationInterAnnuelle@data)>0)
					{
						timesplit="quinzaine"
						dat=bilanMigrationInterAnnuelle@data
						dat=fundat(dat,annee=NULL,timesplit)
						dat$annee=as.factor(dat$annee)    
						sum_per_year<-tapply(dat$valeur,dat$annee,sum)
						sum_per_year<-data.frame(annee=names(sum_per_year),sum_per_year=sum_per_year)
						dat<-merge(dat,sum_per_year,by="annee")
						dat$std_valeur<-dat$valeur/dat$sum_per_year
						all_15<-unique(dat[,timesplit])
						# below I'm adding 0 instead of nothing for 15 days without value
						for (i in 1:length(unique(dat$annee))){#i=5
							annee<-unique(dat$annee)[i]
							this_year_15<-unique(dat[dat$annee==annee,timesplit])
							missing<-all_15[!all_15%in%this_year_15]
							if (length(missing>=1)){
								missingdat<-data.frame("annee"=annee,
										"quinzaine"=missing,
										"valeur"=0,
										"maxtab"=0,
										"mintab"=0,
										"moyenne"=0,
										"sum_per_year"=0,
										"std_valeur"=0)
								dat<-rbind(dat,missingdat)
							}
						}
						dat=dat[order(dat$annee,dat[,timesplit]),]
						g <- ggplot(dat,aes_string(x=timesplit,y="std_valeur"))
						g<-g+geom_area(aes_string(y="std_valeur",fill="annee"),position="stack")
						g <- g+scale_x_datetime(name=paste("mois"),date_breaks="month",
								date_minor_breaks=getvalue(new("Refperiode"),timesplit),
								date_labels="%b",
								limits=as.POSIXct(c(Hmisc::trunc.POSIXt((min(dat[dat$valeur!=0,timesplit])),"month"),Hmisc::ceil.POSIXt((max(dat[dat$valeur!="0",timesplit])),"month")))) 
						g <- g+scale_y_continuous(name="Somme des pourcentages annuels de migration par quinzaine")
						cols <- grDevices::rainbow(length(levels(dat$annee)))
						g <- g+scale_fill_manual(name="annee",values=cols)
						g<-g+labs(title=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,",",bilanMigrationInterAnnuelle@stades@data$std_libelle,
										", saisonnalite de la migration")) 
						g<-g+ theme_minimal() 
						print(g)
						assign(paste("g",sep=""),g,envir_stacomi)
						if (!silent) funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get('g',envir_stacomi)\n",domain="R-stacomiR"))
						
					}    else     {
						if (!silent) funout(gettext("Warning : you have to complete a migration summary for at least one of the selected year before launching a inter-annual summary",domain="R-stacomiR"))
					}
					
				} else if (plot.type=="seasonal"){
					if (! silent) funout("Seasonal graph to show the phenology of migration")
					#bilanMigrationInterAnnuelle<-bmi_vichy;silent=FALSE;timesplit="mois";require(ggplot2)
					bilanMigrationInterAnnuelle<-calcule(bilanMigrationInterAnnuelle,timesplit=timesplit)
					#if (!silent& nrow(bilanMigrationInterAnnuelle@calcdata)==0) stop("You should run calculation before plotting seasonal data")
					dat3<-bilanMigrationInterAnnuelle@calcdata
					datadic<-bilanMigrationInterAnnuelle@data
					datadic<-funtraitementdate(datadic, nom_coldt = "bjo_jour", jour_an = TRUE, quinzaine = TRUE)
					datadic<-chnames(datadic,"jour_365","jour")
					datadic<-killfactor(datadic)
					#datadic[,timesplit]<-as.numeric(datadic[,timesplit])
					# to get nicer graphs we don't use a "numeric but transform our data into dates
					# this function takes a vector of column as argument (col), a timesplit argument
					# and a year. So far it does not handle quinzaine so will issue an error if quinzaine is selected
					dat3[,c("Q0","Q5","Q50","Q95","Q100","d90")]<-round(dat3[,c("Q0","Q5","Q50","Q95","Q100","d90")])
					fn_getbacktodate<-function(dat,col,timesplit_,year=2000){						
						for (i in 1:length(col)){
							dat[,col[i]]<-switch(timesplit_, "jour"={
										as.Date(paste(year,"-",dat[,col[i]],sep=""),"%Y-%j")
									},"semaine"={
										as.Date(paste(year,"-",dat[,col[i]],"-",6,sep=""),"%Y-%U-%w")
									},"mois"={
										as.Date(paste(year,"-",dat[,col[i]],"-",1,sep=""),"%Y-%m-%d")
									},stop(stringr::str_c("Internal error, timesplit ",timesplit_," not working for seasonal plot"))									
							)
						}
						return(dat)
					}
					datadic<-fn_getbacktodate(dat=datadic,
							col=timesplit,
							timesplit_=timesplit)
					dat3<-fn_getbacktodate(dat=dat3,
							col=c("Q0","Q5","Q50","Q95","Q100","d90"),
							timesplit_=timesplit)
					datadic1<-dplyr::select_(datadic,timesplit,"bjo_annee","bjo_valeur")
					datadic1<-dplyr::group_by_(datadic1, "bjo_annee",timesplit)
					datadic1<-dplyr::summarize(datadic1,bjo_valeur=sum(bjo_valeur))
					datadic1<-dplyr::ungroup(datadic1)
					g<-ggplot(data=datadic1)+
							geom_rect(aes(xmin = Q0,xmax = Q100,ymin=year-0.5,ymax=year+0.5),fill="grey90",data=dat3)+
							geom_tile(aes_string(x=timesplit,y="bjo_annee", fill = "bjo_valeur"),color=ifelse(timesplit=="jour","transparent","grey80"))+ 
							scale_fill_distiller(palette = "Spectral", name="Effectif")+
							geom_path(aes(x=Q50,y=year),col="black",lty=2,data=dat3)+
							geom_point(aes(x=Q50,y=year),col="black",size=2,data=dat3)+
							geom_errorbarh(aes(x=Q50,y=year,xmin = Q5,xmax = Q95), height=0,data=dat3)+
							ylab(Hmisc::capitalize(gettext("year",domain="R-stacomiR")))+
							xlab(Hmisc::capitalize(timesplit))+
							scale_x_date(name=timesplit,date_breaks="month",
									date_minor_breaks=getvalue(new("Refperiode"),timesplit),
									date_labels="%b")+
							theme_bw()
					print(g)
					assign("g",g,envir=envir_stacomi)
					if (!silent) funout(gettext("Writing the graphical object into envir_stacomi environment : write g=get('g',envir_stacomi)\n",domain="R-stacomiR"))
					
				}
				
				else { # end if
					stop ("plot.type argument invalid")
				}
				
			}    else     {
				if (!silent) funout(gettext("Attention : you have to complete a migration summary for at least one of the selected year before launching a inter-annual summary",domain="R-stacomiR"))
			}
			
		})			


#' Plot of all interannual from top to bottom
#' @param h handler
#' @param ... additional parameters
hgraphBilanMigrationInterAnnuelle = function(h,...)
{
	bilanMigrationInterAnnuelle <- get("bilanMigrationInterAnnuelle",envir=envir_stacomi)
	bilanMigrationInterAnnuelle <- charge(bilanMigrationInterAnnuelle)
	bilanMigrationInterAnnuelle <- connect(bilanMigrationInterAnnuelle)
	plot(bilanMigrationInterAnnuelle,plot.type="line",silent=FALSE)
	
}

#'Plot of daily migrations
#' @param h handler
#' @param ... additional parameters
hgraphBilanMigrationInterAnnuelle2 = function(h,...)
{
	bilanMigrationInterAnnuelle <- get("bilanMigrationInterAnnuelle",envir=envir_stacomi)
	bilanMigrationInterAnnuelle <- charge(bilanMigrationInterAnnuelle)
	bilanMigrationInterAnnuelle <- connect(bilanMigrationInterAnnuelle)
	plot(bilanMigrationInterAnnuelle,plot.type="standard",silent=FALSE)
}  # end function 








#' Step plot with different years displayed on the same graph. One year
#' can be highlighted against the others
#' @param h handler
#' @param ... additional parameters
hgraphBilanMigrationInterAnnuelle3 = function(h,...)
{ 
	bilanMigrationInterAnnuelle <- get("bilanMigrationInterAnnuelle",envir=envir_stacomi)
	bilanMigrationInterAnnuelle <- charge(bilanMigrationInterAnnuelle)
	bilanMigrationInterAnnuelle <- connect(bilanMigrationInterAnnuelle)
	dat=bilanMigrationInterAnnuelle@data
	plot(bilanMigrationInterAnnuelle,plot.type="step",silent=FALSE)
}   



#' Plot comparing the migration  to the migration  
#' computed for all years available in the daily migration table.
#' This function plots comparisions for periods of 1 week, 2 weeks, month
#' @param h A handler
#' @param ... Additional parameters
hgraphBilanMigrationInterAnnuelle4 = function(h,...)
{
	timesplit=h$action    # timesplit="quinzaine" timesplit="semaine" timesplit="mois"
	bilanMigrationInterAnnuelle <- get("bilanMigrationInterAnnuelle",envir=envir_stacomi)
	bilanMigrationInterAnnuelle <- charge(bilanMigrationInterAnnuelle)
	bilanMigrationInterAnnuelle <- connect(bilanMigrationInterAnnuelle)
	plot(bilanMigrationInterAnnuelle,plot.type="barchart",timesplit=timesplit)
	
}  # end function 



#' Function displaying comparaison similar to \link{hgraphBilanMigrationInterAnnuelle4} but using pointrange and geom_bar
#' This function plots comparisions for periods of 1 week, 2 weeks, month
#' @param h A handler
#' @param ... Additional parameters
hgraphBilanMigrationInterAnnuelle5 = function(h,...)
{
	timesplit=h$action    # timesplit="quinzaine" # timesplit="mois"
	bilanMigrationInterAnnuelle <- get("bilanMigrationInterAnnuelle",envir=envir_stacomi)
	bilanMigrationInterAnnuelle <- charge(bilanMigrationInterAnnuelle)
	bilanMigrationInterAnnuelle <- connect(bilanMigrationInterAnnuelle)
	plot(bilanMigrationInterAnnuelle,plot.type="pointrange",timesplit=timesplit)
	
}  # end function 


#' This function creates a cumulated area plot to highlight seasonal trends in migration. Data are calculated by
#' 2 weeks period then centered and reduced
#' @param h A handler
#' @param ... Additional parameters
hgraphBilanMigrationInterAnnuelle7 = function(h,...)
{
	bilanMigrationInterAnnuelle <- get("bilanMigrationInterAnnuelle",envir=envir_stacomi)
	bilanMigrationInterAnnuelle <- charge(bilanMigrationInterAnnuelle)
	bilanMigrationInterAnnuelle <- connect(bilanMigrationInterAnnuelle)
	plot(bilanMigrationInterAnnuelle,plot.type="density")
	
}



#'Summary handler internal method
#' @param h A handler
#' @param ... Additional parameters
hsummaryBilanMigrationInterannuelle<-function(h,...){
	bilanMigrationInterAnnuelle <- get("bilanMigrationInterAnnuelle",envir=envir_stacomi)
	bilanMigrationInterAnnuelle <- charge(bilanMigrationInterAnnuelle)
	bilanMigrationInterAnnuelle <- connect(bilanMigrationInterAnnuelle)
	summary(bilanMigrationInterAnnuelle)
}

#' summary for BilanMigrationInterAnnuelle
#' provides summary statistics for the latest year (if silent=TRUE), or the year selected in the interface,
#' if silent=FALSE. Mean, min and max are historical statistics with the selected year excluded from the 
#' historical dataset.
#' @param object An object of class \code{\link{BilanMigrationInterAnnuelle-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @param ... Additional parameters (not used there)
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases summary.BilanMigrationInterannuelle
#' @export
setMethod("summary",signature=signature(object="BilanMigrationInterAnnuelle"),definition=function(object,silent=FALSE,...){
			# table generated with funtable
			# TODO traitement des poids
			#object<-bmi
			dat=object@data
			dat<-dat[dat$bjo_labelquantite=="Effectif_total",]
			dat<-stacomirtools::chnames(dat,c("bjo_dis_identifiant","bjo_tax_code","bjo_std_code","bjo_annee","bjo_jour","bjo_labelquantite","bjo_valeur","bjo_horodateexport"),    c("DC","Taxon","Stade","Annee","Jour","Label_quantite","Nombre","Date d'export du bilan"))
			dat$Annee=as.factor(dat$Annee)
			dat = dat[,-1]
			tmp = dat$Jour
			DC = object@dc@dc_selectionne
			# debut_pas must be column name in tableau
			dat<-chnames(dat,"Jour","debut_pas")
			funtable(tableau=dat,
					time.sequence=tmp,
					taxon=object@taxons@data$tax_nom_latin,
					stade=object@stades@data$std_libelle,
					DC,
					resum=NULL,
					silent=silent)
			# Summary statistics
			dat=object@data
			if (silent==FALSE){
				the_choice=as.numeric(select.list(choices=as.character(unique(dat$bjo_annee )[order(unique(dat$bjo_annee ))]),
								preselect=as.character(max(dat$bjo_annee )),
								"choice annee",multiple=FALSE))
			} else {
				the_choice=max((dat$bjo_annee))
			}
			dat<-fundat(dat,timesplit="mois")
			colnames(dat)[colnames(dat)=="maxtab"]<-"max"
			colnames(dat)[colnames(dat)=="mintab"]<-"min"
			dat<-dat[dat$annee==the_choice,]
			dat$mois=strftime(dat$mois,"%b")
			dat$moyenne<-round(dat$moyenne)
			dat<-dat[,c("annee","mois","min","moyenne","max","valeur")]
			colnames(dat)<-c("annee","mois","min","mean","max","valeur")
			return(dat)
		})

