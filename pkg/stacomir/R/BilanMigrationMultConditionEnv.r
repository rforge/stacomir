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
			bmmCE@bilanConditionEnv<-charge(bmmCE@bilanConditionEnv,silent=silent)    		
			return(bmmCE)
		})



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
hbilanMigrationMultConditionEnvgraph = function(h){   
	bmmCE<-get("bmmCE",envir_stacomi)
	bmmCE<-charge(bmmCE)
	bmmCE<-connect(bmmCE)
	bmmCE<-calcule(bmmCE)
	bmmCE<-plot(bmmCE)
}

#' Plot method for BilanMigrationMultConditionEnv
#' @param x An object of class Bilan_carlot
#' @param silent Stops displaying the messages.
#' @param ... Additional arguments, see \code{plot}, \code{plot.default} and \code{par}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases plot.BilanMigrationMultConditionEnv plot.bmmCE
#' @export
setMethod("plot", signature(x = "BilanMigrationMultConditionEnv", y = "missing"), definition=function(x,  silent=FALSE){ 
			bmmCE<-object
			plot(bmmCE@bilanMigrationMult,plot.type="multiple")
			# on va chercher les données du graphique
			
			time.sequence<-as.Date(as.POSIXlt(bmmCE@bilanMigrationMult@time.sequence))
			tableau<-get("grdata",envir_stacomi)
			tableau<-cbind("time.sequence"=time.sequence,tableau)
			tableau$time.sequencechar<-as.character(tableau$time.sequence)
			
			# tableau conditions environnementales
			tableauCE<-bmmCE@bilanConditionEnv@data  
			if (nrow(tableauCE)==0) {
				funout(gettext("You don't have any environmental conditions within the time period\n",domain="R-stacomiR"),arret=TRUE)
			}
			
			stations<-bmmCE@bilanConditionEnv@stationMesure@data
			
			for (i in 1:length(unique(tableauCE$env_stm_identifiant))){
				tableauCE[unique(tableauCE$env_stm_identifiant)[i]==tableauCE$env_stm_identifiant,"stm_libelle"]<-
						stations[stations$stm_identifiant==unique(tableauCE$env_stm_identifiant)[i],"stm_libelle"]
			}
			tableauCE$env_date_debutchar=as.character(as.Date(tableauCE$env_date_debut))  
			
			for (sta in as.character(stations$stm_libelle)){
				tableauCEst<-tableauCE[tableauCE$stm_libelle==sta,] #tableau CE d'une station
				if (length(unique(tableauCEst$env_date_debutchar))!=length(tableauCEst$env_date_debutchar)) {
					funout(gettextf("Attention, on one station :%s there are several entries for the same day :%s only the first value will be incuded in the summary\n",
									sta,
									paste(unique(tableauCEst$env_date_debutchar[duplicated(tableauCEst$env_date_debutchar)]),sep="")),
							arret=FALSE)
					tableauCEst<-tableauCEst[induk(tableauCEst$env_date_debutchar),]
				}
				
				if (is.na(tableauCEst$env_val_identifiant[1])){
					#variable quantitative
					tableauCEst<-tableauCEst[,c("env_date_debutchar","env_valeur_quantitatif")]
					tableauCEst<-stacomirtools::chnames(tableauCEst,"env_valeur_quantitatif",sta)
					stations[stations$stm_libelle==sta,"stm_typevar"]<-"quantitatif"
					# je renomme la colonne e rentrer par le nom de la station
				}   else {
					# variable qualitative
					tableauCEst<-tableauCEst[,c("env_date_debutchar","env_val_identifiant")]
					tableauCEst$"env_val_identifiant"=as.factor(tableauCEst$"env_val_identifiant")
					tableauCEst<-stacomirtools::chnames(tableauCEst,"env_val_identifiant",sta)
					
					stations[stations$stm_libelle==sta,"stm_typevar"]<-"qualitatif"			
				} # end else
				# le merge ci dessous est l'equivalent d'une jointure gauche (LEFT JOIN)
				tableau<-merge(tableau,tableauCEst,by.x = "time.sequencechar", by.y = "env_date_debutchar",  all.x = TRUE)
				# les donnees sont normalement collees dans le tableau dans une nouvelle colonne et aux dates correspondantes
				if (length(time.sequence)!=nrow(tableau)) funout(gettextf("The number of lines of the environmental conditions table (%s) doesn't fit the duration of the migration summary  (%s)\n",
									nrow(tableau),
									length(time.sequence)),
							arret=TRUE)
				#si la jointure e rajoute des lignes ea craint je ne sais pas comment se fera le traitement
			} # end for
			taxon= as.character(bmmCE@bilanMigration@taxons@data$tax_nom_latin)
			stade= as.character(bmmCE@bilanMigration@stades@data$std_libelle)

				bilanMigrationConditionEnv@bilanMigration@dc<-get("refDC",envir_stacomi)
				annee=strftime(as.POSIXlt(mean(time.sequence)),"%Y")
				dis_commentaire=  as.character(bilanMigrationConditionEnv@bilanMigration@dc@data$dis_commentaires[bilanMigrationConditionEnv@bilanMigration@dc@data$dc%in%bilanMigrationConditionEnv@bilanMigration@dc@dc_selectionne]) # commentaires sur le DC
				tableau<-funtraitementdate(tableau,
						nom_coldt="time.sequence",
						annee=FALSE,
						mois=TRUE,
						quinzaine=TRUE,
						semaine=TRUE,
						jour_an=TRUE,
						jour_mois=FALSE,
						heure=FALSE)	
				couleurs=rep(RColorBrewer::brewer.pal(8,"Accent"),2)
				maxeff=floor(log10(max(tableau$Effectif_total,na.rm=TRUE)))
				lab_les_stations=stations$stm_libelle
				for (i in 1:nrow(stations)){
					tableau[,paste("couleur",i,sep="")]<-couleurs[i]
					if (stations$stm_typevar[i]=="quantitatif") {
						diff=maxeff-round(log10(max(tableau[,stations$stm_libelle[i]],na.rm=TRUE)))
						
						if (diff!=0 & !is.na(diff)){
							tableau[,stations$stm_libelle[i]] = as.numeric(tableau[,stations$stm_libelle[i]])*10^diff    
							lab_les_stations[i]=paste(stations$stm_libelle[i],".10^",diff,sep="")
						} # end if
					} #end if
				}  # end for
				tableau$yqualitatif=(10^(maxeff))/2
				name=gettextf("Number %s",paste(lab_les_stations,collapse=", "))
				g<-ggplot(tableau, aes(x=time.sequence,y=Effectif_total))+geom_bar(stat="identity",fill="grey50")+scale_x_date(name="Date")+
						scale_y_continuous(name=name)+labs(title=gettextf("Number %s, %s, %s, %s",dis_commentaire,taxon,stade,annee))
				for (i in 1:nrow(stations)){
					if (stations$stm_typevar[i]=="quantitatif") {
						if (all(!is.na(tableau[,stations$stm_libelle[i]]))){
							g<-g+geom_line(aes_string(x="time.sequence",y=stations$stm_libelle[i],colour=paste("couleur",i,sep="")),size=1)+
									scale_colour_identity(name="stations",breaks=couleurs[1:i],labels=stations$stm_libelle[1:i])
						} else {
							g<-g+geom_point(aes_string(x="time.sequence",y=stations$stm_libelle[i],colour=paste("couleur",i,sep="")),size=2)+
									scale_colour_identity(name="stations",breaks=couleurs[1:i],labels=stations$stm_libelle[1:i])
						}
					} else if (stations$stm_typevar[i]=="qualitatif") {
						stableau=subset(tableau, !is.na(tableau[,stations$stm_libelle[i]]))
						stableau[,stations$stm_libelle[i]]<- as.factor(as.character( stableau[,stations$stm_libelle[i]]))
						if (stations$stm_par_code[i]=="AAAA")# phases lunaires
							g<-g+geom_point(aes_string(x="time.sequence",y="yqualitatif",colour=paste("couleur",i,sep=""),shape=stations$stm_libelle[i]),data=stableau,size=3)+
									scale_colour_identity(name="stations",breaks=couleurs[1:i],labels=stations$stm_libelle[1:i])
					} else stop("internal error")
				} # end for
				assign("g",g,envir_stacomi)
				funout(gettext("Writing of the graphical object in the environment envir_stacomi : write g=get(g,envir_stacomi)\n",domain="R-stacomiR"))
				print(g)
			
	
}# end function



#' handler du graphique BilanMigrationMultConditionEnv
#' realise le calcul du bilan migration avec CE, l'ecrit dans l'environnement envir_stacomi
#' traite eventuellement les quantites de lots (si c'est des civelles)
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
hbilanMigrationMultConditionEnvcalc=function(h,...){
	calcule(h$action)
	enabled(toolbarlist[["Graph"]])<-TRUE
	# calcule(bilanMigrationMultConditionEnv)
}
