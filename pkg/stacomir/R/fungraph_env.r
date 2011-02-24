# Nom fichier :        fungraph_env.R

#' graph function for BilanMigrationEnv
#' @param tableau 
#' @param duree 
#' @param taxon 
#' @param stade 
#' @param stations 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
fungraph_env=function(tableau,duree,taxon,stade,stations){
	bilanMigrationConditionEnv@bilanMigration@dc<-get("refDC",envir_stacomi)
	annee=strftime(as.POSIXlt(mean(duree)),"%Y")
	dis_commentaire=  as.character(bilanMigrationConditionEnv@bilanMigration@dc@data$dis_commentaires[bilanMigrationConditionEnv@bilanMigration@dc@data$dc%in%bilanMigrationConditionEnv@bilanMigration@dc@dc_selectionne]) # commentaires sur le DC
	tableau<-funtraitementdate(tableau,
			nom_coldt="duree",
			annee=FALSE,
			mois=TRUE,
			quinzaine=TRUE,
			semaine=TRUE,
			jour_an=TRUE,
			jour_mois=FALSE,
			heure=FALSE)
	
	couleurs=rep(brewer.pal(8,"Accent"),2)
	maxeff=floor(log10(max(tableau$Effectif_total,na.rm=TRUE)))
	lab_les_stations=stations$stm_libelle
	for (i in 1:nrow(stations)){
		tableau[,paste("couleur",i,sep="")]<-couleurs[i]
		if (stations$stm_typevar[i]=="quantitatif") {
			diff=maxeff-round(log10(max(tableau[,stations$stm_libelle[i]],na.rm=TRUE)))
			if (diff!=0 ){
				tableau[,stations$stm_libelle[i]] = as.numeric(tableau[,stations$stm_libelle[i]])*10^diff    
				lab_les_stations[i]=paste(stations$stm_libelle[i],".10^",diff,sep="")
			} # end if
		} #end if
	}  # end for
	tableau$yqualitatif=(10^(maxeff))/2
	name=paste(get("msg",envir=envir_stacomi)$fungraph_env.1,", ",paste(lab_les_stations,collapse=", "),sep="")
	g<-ggplot(tableau, aes(x=duree,y=Effectif_total))+geom_bar(stat="identity",fill="grey50")+scale_x_date(name="Date")+
			scale_y_continuous(name=name)+opts(title=paste(get("msg",envir=envir_stacomi)$fungraph_env.1,",", dis_commentaire,",",taxon,",", stade,",", annee ))
	for (i in 1:nrow(stations)){
		if (stations$stm_typevar[i]=="quantitatif") {
			if (all(!is.na(tableau[,stations$stm_libelle[i]]))){
			g<-g+geom_line(aes_string(x="duree",y=stations$stm_libelle[i],colour=paste("couleur",i,sep="")),size=1)+
					scale_colour_identity(name="stations",breaks=couleurs[1:i],labels=stations$stm_libelle[1:i])
			} else {
				g<-g+geom_point(aes_string(x="duree",y=stations$stm_libelle[i],colour=paste("couleur",i,sep="")),size=2)+
						scale_colour_identity(name="stations",breaks=couleurs[1:i],labels=stations$stm_libelle[1:i])
			}
		} else if (stations$stm_typevar[i]=="qualitatif") {
			stableau=subset(tableau, !is.na(tableau[,stations$stm_libelle[i]]))
			stableau[,stations$stm_libelle[i]]<- as.factor(as.character( stableau[,stations$stm_libelle[i]]))
			if (stations$stm_par_code[i]=="AAAA")# phases lunaires
				g<-g+geom_point(aes_string(x="duree",y="yqualitatif",colour=paste("couleur",i,sep=""),shape=stations$stm_libelle[i]),data=stableau,size=3)+
						scale_colour_identity(name="stations",breaks=couleurs[1:i],labels=stations$stm_libelle[1:i])
		} else stop("erreur interne")
	} # end for
	assign("g",g,envir_stacomi)
	funout(get("msg",envir=envir_stacomi)$fungraph_env.2)
	print(g)
}
