#' Function for class BilanMigrationEnv drawing both the response of
#' environment variables...
#' 
#' graph function for BilanMigrationEnv, draws both the response of environment
#' variables (temperature, moon phases...) and the migration for a species and
#' a stage
#' 
#' 
#' @param tableau data issued from a bilanMigration
#' @param time.sequence a vector of class POSIXt
#' @param taxon the species
#' @param stade the stage
#' @param stations one or several measure stations
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
fungraph_env=function(tableau,time.sequence,taxon,stade,stations){
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
}
