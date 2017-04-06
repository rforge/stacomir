#' Class "BilanMigrationMultConditionEnv"
#' 
#' Enables to compute an annual overview of fish migration and environmental
#' conditions in the same chart
#' 
#' @include BilanMigrationMult.r 
#' @include BilanConditionEnv.r
#' @include create_generic.r
#' @include utilitaires.r
#' @slot bilanMigrationMult \link{BilanMigrationMult-class}
#' @slot bilanConditionEnv \link{BilanConditionEnv-class}
#' @author cedric.briand"at"eptb-vilaine.fr marion.legrand"at"logrami.fr
#' @family Bilan Objects
#' @keywords classes
#' @aliases BilanMigrationMultConditionEnv bilanmigrationmultconditionenv bmmCE
#' @keywords classes
#' @example inst/examples/bilanMigrationMultConditionEnv_example.R
#' @export

#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @family Bilan Objects
#' @keywords classes
#' @export 
setClass(Class="BilanMigrationMultConditionEnv",representation=
				representation(
						bilanMigrationMult="BilanMigrationMult",
						bilanConditionEnv="BilanConditionEnv"
				),
		prototype=prototype(
				bilanMigrationMult=new("BilanMigrationMult"),
				bilanConditionEnv=new("BilanConditionEnv")
		
		)
)


setValidity("BilanMigrationMultConditionEnv",
		function(object)
		{
			rep1=validObject(object@bilanMigrationMult, test=TRUE)
			rep2=validObject(object@bilanConditionEnv, test=TRUE)			
			return(ifelse(rep1 & rep2 ,TRUE,c(1:2)[!c(rep1, rep2)]))
		}   
)
#' connect method for BilanMigrationMultConditionEnv class
#' @param object An object of class \link{BilanMigrationMultConditionEnv-class}
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @return an object of BilanMigrationMultConditionEnv class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("connect",signature=signature("BilanMigrationMultConditionEnv"),definition=function(object,silent=FALSE) {
			#object<-bmmCE
			bmmCE<-object
			bmmCE@bilanMigrationMult<-connect(bmmCE@bilanMigrationMult,silent=silent)
			bmmCE@bilanConditionEnv<-connect(bmmCE@bilanConditionEnv,silent=silent)
			return(bmmCE)
		}
)
#' command line interface for BilanConditionEnv class
#' @param object An object of class \link{BilanConditionEnv-class}
#' @param stationmesure A character, the code of the monitoring station, which records environmental parameters \link{choice_c,RefStationMesure-method}
#' @param datedebut The starting date as a character, formats like \code{\%Y-\%m-\%d} or \code{\%d-\%m-\%Y} can be used as input
#' @param datefin The finishing date of the Bilan, for this class this will be used to calculate the number of daily steps.
#' @param silent Boolean default FALSE, if TRUE information messages not displayed.
#' @return An object of class \link{BilanConditionEnv-class}
#' The choice_c method fills in the data slot for RefStationMesure and  and then 
#' uses the choice_c methods of these object to select the data.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("choice_c",signature=signature("BilanMigrationMultConditionEnv"),definition=function(object,dc,taxons,stades,stationMesure,datedebut,datefin,silent=FALSE){
			# code for debug
			# dc=c(5,6,12);	taxons=c("Anguilla anguilla");stades=c("AGJ","AGG","CIV");
			# stationMesure=c("temp_gabion","coef_maree");
			# datedebut="2008-01-01";datefin="2008-12-31";silent=FALSE
			bmmCE<-object
			bmmCE@bilanMigrationMult=
					choice_c(bmmCE@bilanMigrationMult,
							dc=dc,
							taxons=taxons,
							stades=stades,
							datedebut=datedebut,
							datefin=datefin)
			bmmCE@bilanConditionEnv=choice_c(bmmCE@bilanConditionEnv,
					stationMesure=stationMesure,
					datedebut=datedebut,
					datefin=datefin,
					silent=silent)
			return(bmmCE)
		})
#' charge method for BilanMigrationMultConditionEnv class
#' @param object An object of class \link{BilanMigrationMultConditionEnv-class}
#' @inheritDotParams charge,BilanConditionEnv-method -object  
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("charge",signature=signature("BilanMigrationMultConditionEnv"),definition=function(object,silent) {
			# silent=FALSE
			bmmCE<-object
			bmmCE@bilanMigrationMult<-charge(bmmCE@bilanMigrationMult,silent=silent)
			# the values for date are not initiated by the interface
			assign("bilanConditionEnv_date_debut",get("pasDeTemps",envir_stacomi)@"dateDebut",envir_stacomi)
			assign("bilanConditionEnv_date_fin",as.POSIXlt(DateFin(get("pasDeTemps",envir_stacomi))),envir_stacomi)
			bmmCE@bilanConditionEnv<-charge(bmmCE@bilanConditionEnv,silent=silent)    		
			return(bmmCE)
		})

#' Internal handler function
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
hbmmCEcalc=function(h=null,...){
	bmmCE<-get("bmmCE",envir_stacomi)	
	bmmCE<-charge(bmmCE)
	bmmCE<-connect(bmmCE)
	bmmCE<-calcule(bmmCE)
	assign("bmmCE",bmmCE,envir_stacomi)
	enabled(toolbarlist[["Graph"]])<-TRUE
	return(invisible(NULL))	
}

#' Calculation for the BilanMigrationMultConditionEnv
#' 
#' @param object An object of class \code{\link{BilanMigrationMultConditionEnv-class}}
#' @return \code{\link{BilanMigrationMultConditionEnv-class}}
#' @export
setMethod("calcule",signature=signature("BilanMigrationMultConditionEnv"),definition=function(object,silent){ 
			# silent=FALSE
			bmmCE<-object
			bmmCE@bilanMigrationMult<-calcule(bmmCE@bilanMigrationMult)			
			funout(gettext("bmmCE object is stocked into envir_stacomi environment\n",domain="R-stacomiR"))
			return(bmmCE)
		})



#' internal method for graphical interface
#' @param h A handler
hbmmCEgraph = function(h=null,...){   
	bmmCE<-get("bmmCE",envir_stacomi)
	bmmCE<-plot(bmmCE)
	return(invisible(NULL))	
}

#' Plot method for BilanMigrationMultConditionEnv
#' @param x An object of class \link{BilanMigrationMultConditionEnv}
#' @param silent Stops displaying the messages.
#' @param color_station A named vector of station color (e.g. c("temp_gabion"="red","coef_maree"="blue","phases_lune"="green")) default null
#' @param color_dc A named vector giving the color for each dc default null (e.g. c("5"="#4D4D4D","6"="#E6E6E6","12"="#AEAEAE"))
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases plot.BilanMigrationMultConditionEnv plot.bmmCE
#' @export
setMethod("plot", signature(x = "BilanMigrationMultConditionEnv", y = "missing"), definition=function(x,  color_station=NULL,color_dc=NULL, silent=FALSE){ 
			#color_station=NULL;color_dc=NULL
			# color_station<-c("temp_gabion"="red","coef_maree"="blue","phases_lune"="green")
			# color_dc=c("5"="#4D4D4D","6"="#E6E6E6","12"="#AEAEAE")
			bmmCE<-x
			
			
			grdata<-fun_aggreg_for_plot(bmmCE@bilanMigrationMult)
			# we collect the dataset used to build the graph
			
			taxons= as.character(bmmCE@bilanMigrationMult@taxons@data$tax_nom_latin)
			stades= as.character(bmmCE@bilanMigrationMult@stades@data$std_libelle)
			dc<-unique(grdata$DC)
			# pour avoir dans le graphique le dc_code des dc 
			# ggplot passe les dc dans l'ordre dans lequel ils apparaissent dans le tableau
			# et unique fait ça aussi .... OUIIIII
			dc_code<-bmmCE@bilanMigrationMult@dc@data$dc_code[
					match(dc,bmmCE@bilanMigrationMult@dc@data$dc)]
			# tableau conditions environnementales
			tableauCE<-bmmCE@bilanConditionEnv@data  
			if (nrow(tableauCE)==0) {
				funout(gettext("You don't have any environmental conditions within the time period\n",domain="R-stacomiR"),arret=TRUE)
			}
			
			stations<-bmmCE@bilanConditionEnv@stationMesure@data
			#######################
			# color scheme for station
			#######################
			if (is.null(color_station)) {
				color_station=rep(RColorBrewer::brewer.pal(8,"Accent"),2)[1:nrow(stations)]
				names(color_station)<-stations$stm_libelle
			} else if (length(color_station)!=nrow(stations)){
				funout(gettextf("The color_station argument should have length %s",nrow(stations)),arret=TRUE)
			}
			if (!all(names(color_station)%in%stations$stm_libelle)) {
				stop (gettextf("The following name(s) %s do not match station name: %s",
								names(color_station)[!names(color_station)%in%stations$stm_libelle],
								paste(stations$stm_libelle, collapse=", ")))
			}
			
			cs<-cbind(stm_libelle=names(color_station),"color"=color_station)
			#######################
			# color scheme for dc
			#######################			
			if (is.null(color_dc)) {
				color_dc=grDevices::gray.colors(length(dc))
				names(color_dc)<-dc
			} else if (length(color_dc)!=length(dc)){
				funout(gettextf("The color_dc argument should have length %s",length(dc)),arret=TRUE)
			}
			if (!all(names(color_dc)%in%dc)) 
				stop (gettextf("The following name(s) %s do not match DC codes: %s",
								names(color_dc)[!names(color_dc)%in%dc],
								paste(dc, collapse=", ")))
			cdc<-cbind("DC"=names(color_dc),"color"=color_dc)
			
			# we collect libelle from station
			for (i in 1:length(unique(tableauCE$env_stm_identifiant))){
				tableauCE[unique(tableauCE$env_stm_identifiant)[i]==tableauCE$env_stm_identifiant,"stm_libelle"]<-
						stations[stations$stm_identifiant==unique(tableauCE$env_stm_identifiant)[i],"stm_libelle"]
			}
			# the data can be in the POSIXct format, we need to round them
			tableauCE$date<-as.POSIXct(round.POSIXt(tableauCE$env_date_debut,units="days"))
			qualitative<-!is.na(tableauCE$env_val_identifiant)
			tableauCEquan<-tableauCE[!qualitative,]
			tableauCEqual<-tableauCE[qualitative,]
			if (nrow(unique(cbind(tableauCE$date,tableauCE$stm_libelle)))!=	nrow(tableauCE)) {
				funout(gettextf("Attention, on one station :%s there are several entries for the same day :%s we will calculate average for numeric
										and use the first value for qualitative parameter",
								sta,
								paste(unique(tableauCEst$env_date_debut[duplicated(tableauCEst$env_date_debut)]),sep="")),
						arret=FALSE)	
				# for quantitative parameters we group by date and station and use the average to
				# extract one value per day
				tableauCEquan<-dplyr::select(tableauCEquan,date,stm_libelle,env_valeur_quantitatif)%>%
						dplyr::group_by(date,stm_libelle)%>%						
						dplyr::summarize(valeur=mean(env_valeur_quantitatif))%>%
						dplyr::ungroup()
				# for qualitative value, when there are several values for the same date
				# we arbitrarily select the first
				tableauCEqual<-dplyr::select(tableauCEqual,date,stm_libelle,env_val_identifiant)%>%
						dplyr::group_by(date,stm_libelle)%>%						
						dplyr::summarize(valeur=first(env_val_identifiant))%>%
						dplyr::ungroup()
			} else {
				# we want the same format as above
				tableauCEquan<-dplyr::select(tableauCEquan,date,stm_libelle,env_valeur_quantitatif)%>%
						dplyr::rename(valeur=env_valeur_quantitatif)
				tableauCEqual<-dplyr::select(tableauCEqual,date,stm_libelle,env_val_identifiant)%>%
						dplyr::rename(valeur=env_val_identifiant)
			}	
			variables_quant<-unique(tableauCEquan$stm_libelle)
			variables_qual<-unique(tableauCEqual$stm_libelle)
			grdata<-funtraitementdate(grdata,
					nom_coldt="debut_pas",
					annee=FALSE,
					mois=TRUE,
					quinzaine=TRUE,
					semaine=TRUE,
					jour_an=TRUE,
					jour_mois=FALSE,
					heure=FALSE)	
			
			# to rescale everything on the same graph
			maxeff=floor(log10(max(grdata$effectif_total,na.rm=TRUE)))
			
			for (i in 1:length(variables_quant)){
				diff=maxeff-round(log10(max(tableauCEquan[tableauCEquan$stm_libelle==variables_quant[i],"valeur"],na.rm=TRUE)))
				if (diff!=0 & !is.na(diff)){
					tableauCEquan[tableauCEquan$stm_libelle==variables_quant[i],"valeur"] = as.numeric(tableauCEquan[tableauCEquan$stm_libelle==variables_quant[i],"valeur"])*10^diff    
					variables_quant[i]=paste(variables_quant[i],".10^",diff,sep="")
				} # end if
			} #end for			
			yqualitatif=(10^(maxeff))/2
			
			ylegend=gettextf("Number, %s, %s",paste(variables_quant,collapse=", "),
					paste(variables_qual,collapse=", "))
			
			
			
			
			
			######################
			# traitement des données pour grouper par dc (group_by dc)
			# les stades et taxons seront aggrégés avec warning
			#################################
			if (length(unique(taxons))>1) warning(gettextf("you have %s taxa in the bilan, those will be aggregated",length(unique(taxons))))
			if (length(unique(stades))>1) warning(gettextf("you have %s stages in the bilan, those will be aggregated",length(unique(stades))))		
			plotdata<-dplyr::select(grdata,debut_pas,DC,effectif_total)%>%dplyr::rename(date=debut_pas)%>%
					dplyr::group_by(date,DC)%>%dplyr::summarize(effectif=sum(effectif_total))%>%
					dplyr::ungroup()
			
			# merging with colors
			plotdata<-killfactor(merge(plotdata,cdc,by="DC"))
			tableauCEquan<-killfactor(merge(tableauCEquan,cs,by="stm_libelle"))
			tableauCEqual<-killfactor(merge(tableauCEqual,cs,by="stm_libelle"))
			
			g<-ggplot(plotdata)+
					geom_bar(aes(x=date,y=effectif,fill =color),position="stack", stat="identity")+
					ylab(ylegend)+
					geom_line(aes(x=date,y=valeur,colour=color),data=tableauCEquan,size=1)+						
					geom_point(aes(x=date,shape=valeur,
									colour=color),
							y=yqualitatif,data=tableauCEqual,size=3)+
					scale_fill_identity(name=gettext("DC"),labels=dc_code,guide = "legend")+
					scale_colour_identity(name=gettext("stations"),
							labels=names(cs[,"color"]),
							breaks=cs[,"color"],
							guide = "legend")+
					scale_shape(guide="legend",name=gettext("Qualitative parm"))+
					theme_bw()	
			print(g)
			assign("g",g,envir_stacomi)
			funout(gettext("the ggplot object has been assigned to envir_stacomi, type g<-get('g',envir_stacomi)"))
			
		})# end function



