# Nom fichier :        funraph.R
# Projet :             calcmig/fonctions

# Description :       graph pour bilan migration autres que civelles


#' Function for BilanMigration graphes including numbers DF DC operations
#' 
#' This graph is for species other than glass eel
#' 
#' 
#' @usage fungraph(bilanMigration, tableau, duree, taxon, stade)
#' @param bilanMigration an objet of class \code{\linkS4class{BilanMigration}}
#' @param table=tableau a data frame with the results
#' @param duree a vector POSIXt
#' @param taxon the species
#' @param stade the stage
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}

#' @export
fungraph=function(bilanMigration,tableau,duree,taxon,stade){
#mat <- matrix(1:6,3,2)
#layout(mat)
	if(length(unique(tableau$Type_de_quantite))>1) funout(get("msg",envir=envir_stacomi)$fungraph.1) 
	annee=unique(strftime(as.POSIXlt(duree),"%Y"))
	mois= months(duree)
	jour= strftime(as.POSIXlt(duree),"%j")
	jmois=strftime(as.POSIXlt(duree),"%d")
	mois=unique(mois)
	mois=paste("15",substr(as.character(mois),1,3))
	index=as.vector(tableau$No.pas[jmois==15])
	x=1:nrow(tableau)
	debut=unclass(as.POSIXct((min(duree))))[[1]] # attention arrondit � un jour de moins
	fin=unclass(as.POSIXct(max(duree)))[[1]]
	dis_commentaire=  as.character(bilanMigration@dc@data$dis_commentaires[bilanMigration@dc@data$dc%in%bilanMigration@dc@dc_selectionne]) # commentaires sur le DC
	###################################
	# Definition du layout
	####################################
	vec<-c(rep(1,15),rep(2,2),rep(3,2),rep(4,3))
	mat <- matrix(vec,length(vec),1)
	layout(mat)
	mypalette<-rev(brewer.pal(4,"Paired"))
	#par("bg"=gray(0.8))
	par("mar"=c(3, 4, 3, 2) + 0.1)
	###################################
	# Graph annuel couvrant sequence >0
	####################################
	gr<-matplot(x,cbind(tableau$Mesure+tableau$Calcule+tableau$Expert+tableau$Ponctuel,
					tableau$Mesure+tableau$Calcule+tableau$Expert,
					tableau$Mesure+tableau$Calcule,
					tableau$Mesure),
			col=mypalette[1:4],
			type=c("h","h","h","h"),
			pch=16,
			lty=1,
			xaxt="n",
			bty="l",
			ylab=get("msg",envir=envir_stacomi)$fungraph.2,
			xlab=get("msg",envir=envir_stacomi)$fungraph.3,
			main=paste(get("msg",envir=envir_stacomi)$fungraph.4,dis_commentaire,", ",taxon,", ",stade,", ",annee,sep=""),
			cex.main=1)
	print(gr)
	if(bilanMigration@pasDeTemps@dureePas=="86400"){ # pas de temps journalier
		index=as.vector(x[jmois==15])
		axis(side=1,at=index,tick=TRUE,labels=mois)
		#axis(side=1,at=as.vector(x[jmois==1]),tick=TRUE,labels=FALSE)
		
	} else {
		axis(side=1)
	}  	
	mtext(text=paste(get("msg",envir=envir_stacomi)$fungraph.6,
					round(sum(tableau$Mesure+tableau$Calcule+tableau$Expert+tableau$Ponctuel))),
			side=3,
			col=brewer.pal(5,"Paired")[5],
			cex=0.8)
	
	legend(x=0,
			y=max(tableau$Mesure+tableau$Calcule+tableau$Expert+tableau$Ponctuel,na.rm=TRUE),
			legend= get("msg",envir=envir_stacomi)$fungraph.5,
			pch=c(16),
			col=c(mypalette[1:4]))
	
	###################################         
	# Requete de la base
	################################### 
	req<-new("RequeteODBC")
	req@baseODBC<-get("baseODBC",envir=envir_stacomi)
	req@sql<-paste("SELECT * FROM  t_operation_ope ",
			"WHERE ope_date_debut >= '",
			strftime(as.POSIXlt(duree[min(x)]),format="%Y-%m-%d %H:%M:%S"),
			"' AND ope_date_fin <= '" ,
			strftime(as.POSIXlt(duree[max(x)]),format="%Y-%m-%d %H:%M:%S"),
			"' AND ope_dic_identifiant=",bilanMigration@dc@dc_selectionne,
			" ORDER BY ope_date_debut; ",sep = "")
	t_operation_ope<-connect(req)@query
	# sortie de commentaires
	dif=difftime(t_operation_ope$ope_date_fin,t_operation_ope$ope_date_debut, units ="days")
	funout(paste(get("msg",envir=envir_stacomi)$fungraph_civelle.5,nrow(t_operation_ope),"\n"))
	funout(paste(get("msg",envir=envir_stacomi)$fungraph_civelle.6,round(mean(as.numeric(dif)),2),get("msg",envir=envir_stacomi)$fungraph.8))
	funout(paste(get("msg",envir=envir_stacomi)$fungraph_civelle.7,round(max(as.numeric(dif)),2),get("msg",envir=envir_stacomi)$fungraph.8))
	funout(paste(get("msg",envir=envir_stacomi)$fungraph_civelle.8,round(min(as.numeric(dif)),2),get("msg",envir=envir_stacomi)$fungraph.8))
	
	req@sql<-fn_sql_dis(per_dis_identifiant=bilanMigration@dc@data$df[bilanMigration@dc@data$dc%in%bilanMigration@dc@dc_selectionne],
			dateDebut=strftime(as.POSIXlt(duree[min(x)]),format="%Y-%m-%d %H:%M:%S"),
			dateFin=strftime(as.POSIXlt(duree[max(x)]),format="%Y-%m-%d %H:%M:%S"))
	fonctionnementDF<-connect(req)@query
	
	req@sql<-fn_sql_dis(per_dis_identifiant=bilanMigration@dc@dc_selectionne,
			dateDebut=strftime(as.POSIXlt(duree[min(x)]),format="%Y-%m-%d %H:%M:%S"),
			dateFin=strftime(as.POSIXlt(duree[max(x)]),format="%Y-%m-%d %H:%M:%S"))
	fonctionnementDC<-connect(req)@query
	
	
	
	graphdate<-function(vectordate){
		attributes(vectordate)<-NULL
		vectordate=unclass(vectordate)
		vectordate[vectordate<debut]<-debut
		vectordate[vectordate>fin]<-fin
		return(vectordate)
	}
	
	
	###################################         
	# creation d'un graphique vide (2)
	###################################
	mypalette<-brewer.pal(12,"Paired")
	par("mar"=c(0, 4, 0, 2)+ 0.1)  
	plot(   as.POSIXct(duree),
			seq(0,3,length.out=nrow(tableau)),
			xlim=c(debut,fin), 
			type= "n", 
			xlab="",
			xaxt="n",
			yaxt="n", 
			ylab=get("msg",envir=envir_stacomi)$fungraph_civelle.11,
			bty="n",
			cex=1.2)
	
	###################################         
	# temps de fonctionnement du DF
	###################################
	
	if (dim(fonctionnementDF)[1]==0 ) {
		
		rect(   xleft=debut, 
				ybottom=2.1,
				xright=fin,
				ytop=3, 
				col = mypalette[4],
				border = NA, 
				lwd = 1)    
		rect(   xleft=debut, 
				ybottom=1.1,
				xright=fin,
				ytop=2, 
				col = mypalette[1],
				border = NA, 
				lwd = 1)           
		legend(  x= "bottom",
				legend= get("msg",envir=envir_stacomi)$fungraph_civelle.9,
				pch=c(16,16),
				col=c(mypalette[4],mypalette[6],mypalette[1]),
				horiz=TRUE,
				bty="n"
		)
		
		
	} else {
		
		# si il sort quelque chose
		if (sum(fonctionnementDF$per_etat_fonctionnement==1)>0){    
			rect(   xleft =graphdate(as.POSIXct(fonctionnementDF$per_date_debut[
											fonctionnementDF$per_etat_fonctionnement==1])), 
					ybottom=2.1,
					xright=graphdate(as.POSIXct(fonctionnementDF$per_date_fin[
											fonctionnementDF$per_etat_fonctionnement==1])),
					ytop=3, 
					col = mypalette[4],
					border = NA, 
					lwd = 1)       }
		if (sum(fonctionnementDF$per_etat_fonctionnement==0)>0){              
			rect(   xleft =graphdate(as.POSIXct(fonctionnementDF$per_date_debut[
											fonctionnementDF$per_etat_fonctionnement==0])), 
					ybottom=2.1,
					xright=graphdate(as.POSIXct(fonctionnementDF$per_date_fin[
											fonctionnementDF$per_etat_fonctionnement==0])),
					ytop=3, 
					col = mypalette[6],
					border = NA, 
					lwd = 1)  }
		#creation d'une liste par categorie d'arret contenant vecteurs dates    
		listeperiode<-
				fn_table_per_dis(typeperiode=fonctionnementDF$per_tar_code,
						tempsdebut= fonctionnementDF$per_date_debut,
						tempsfin=fonctionnementDF$per_date_fin,
						libelle=fonctionnementDF$libelle,
						date=FALSE)
		nomperiode<-vector()
		for (j in 1 : length(listeperiode)){
			#recuperation du vecteur de noms (dans l'ordre) � partir de la liste
			nomperiode[j]<-substr(listeperiode[[j]]$nom,1,17) 
			#ecriture pour chaque type de periode                       
			rect(   xleft=graphdate(listeperiode[[j]]$debut), 
					ybottom=1.1,
					xright=graphdate(listeperiode[[j]]$fin),
					ytop=2, 
					col = mypalette[j],
					border = NA, 
					lwd = 1) 
		}       
		
		legend  (x= debut,
				y=1.2,
				legend= c(get("msg",envir=envir_stacomi)$fungraph_civelle.10,nomperiode),
				pch=c(15,15),
				col=c(mypalette[4],mypalette[6],mypalette[1:length(listeperiode)]),
				bty="n",
				ncol=7,
				text.width=(fin-debut)/6)
	}
	
	###################################         
	# creation d'un graphique vide (3)
	###################################                 
	
	par("mar"=c(0, 4, 0, 2)+ 0.1)  
	plot(   as.POSIXct(duree),
			seq(0,3,length.out=nrow(tableau)),
			xlim=c(debut,fin), 
			type= "n", 
			xlab="",
			xaxt="n",
			yaxt="n", 
			ylab=get("msg",envir=envir_stacomi)$fungraph_civelle.12,
			bty="n",
			cex=1.2)             
	###################################         
	# temps de fonctionnement du DC
	###################################                 
	
	
	if (dim(fonctionnementDC)[1]==0 ) {
		
		rect(      xleft=debut, 
				ybottom=2.1,
				xright=fin,
				ytop=3, 
				col = mypalette[4],
				border = NA, 
				lwd = 1)               
		
		rect(      xleft=debut, 
				ybottom=1.1,
				xright=fin,
				ytop=2, 
				col = mypalette[1],
				border = NA, 
				lwd = 1)
		legend(  x= "bottom",
				legend= c(get("msg",envir=envir_stacomi)$fungraph_civelle.9),
				pch=c(16,16),
				col=c(mypalette[4],mypalette[6],mypalette[1]),
				#horiz=TRUE,
				ncol=5,
				bty="n")
		
		
	} else {
		
		if (sum(fonctionnementDC$per_etat_fonctionnement==1)>0){ 
			rect(   xleft =graphdate(as.POSIXct(fonctionnementDC$per_date_debut[
											fonctionnementDC$per_etat_fonctionnement==1])), 
					ybottom=2.1,
					xright=graphdate(as.POSIXct(fonctionnementDC$per_date_fin[
											fonctionnementDC$per_etat_fonctionnement==1])),
					ytop=3, 
					col = mypalette[4],
					border = NA, 
					lwd = 1) }
		if (sum(fonctionnementDC$per_etat_fonctionnement==0)>0)
		{ 
			rect(   xleft =graphdate(as.POSIXct(fonctionnementDC$per_date_debut[
											fonctionnementDC$per_etat_fonctionnement==0])), 
					ybottom=2.1,
					xright=graphdate(as.POSIXct(fonctionnementDC$per_date_fin[
											fonctionnementDC$per_etat_fonctionnement==0])),
					ytop=3, 
					col = mypalette[6],
					border = NA, 
					lwd = 1) }
		listeperiode<-
				fn_table_per_dis(typeperiode=fonctionnementDC$per_tar_code,
						tempsdebut= fonctionnementDC$per_date_debut,
						tempsfin=fonctionnementDC$per_date_fin,
						libelle=fonctionnementDC$libelle,
						date=FALSE)
		nomperiode<-vector()
		
		for (j in 1 : length(listeperiode)){
			nomperiode[j]<-substr(listeperiode[[j]]$nom,1,17)   
			rect(   xleft=graphdate(listeperiode[[j]]$debut), 
					ybottom=1.1,
					xright=graphdate(listeperiode[[j]]$fin),
					ytop=2, 
					col = mypalette[j],
					border = NA, 
					lwd = 1)        
		}
		
		legend  (x= debut,
				y=1.2,
				legend= c(get("msg",envir=envir_stacomi)$fungraph_civelle.10,nomperiode),
				pch=c(15,15),
				col=c(mypalette[4],mypalette[6],mypalette[1:length(listeperiode)]),
				bty="n",
				ncol=7,
				text.width=(fin-debut)/6)
	}
	
	###################################         
	# creation d'un graphique vide (4)
	###################################                 
	
	
	par("mar"=c(2, 4, 0, 2)+ 0.1)  
	plot(   as.POSIXct(duree),
			seq(0,1,length.out=nrow(tableau)),
			xlim=c(debut,fin), 
			type= "n", 
			xlab="",
			xaxt="n",
			yaxt="n", 
			ylab=get("msg",envir=envir_stacomi)$fungraph_civelle.13,
			bty="n",
			cex=1.2)             
	###################################         
	# operations
	###################################  
	if(is.odd(bilanMigration@dc@dc_selectionne)) brew="Paired" else brew="Accent"
	rect(   xleft =graphdate(as.POSIXct(t_operation_ope$ope_date_debut)), 
			ybottom=0,
			xright=graphdate(as.POSIXct(t_operation_ope$ope_date_fin)),
			ytop=1, 
			col = brewer.pal(8,brew),
			border = NA, 
			lwd = 1)
	
	
	###################################
	# Graph mensuel 
	####################################
	X11(7,4)
	stktab=cbind(stack(tableau[,c(2,3,4,5)]),"duree"=rep(duree,4))
	stktab<-funtraitementdate(stktab,
			nom_coldt="duree",
			annee=FALSE,
			mois=TRUE,
			quinzaine=TRUE,
			semaine=TRUE,
			jour_an=TRUE,
			jour_mois=FALSE,
			heure=FALSE)
	names(stktab)=get("msg",envir=envir_stacomi)$fungraph.7
	g<-ggplot(stktab, aes(x=mois,y=Effectifs,fill=type))+geom_bar(position="stack", stat="identity")+scale_fill_brewer(name="type",palette="Paired")
	print(g)
	# pour l'instant je ne peux pas combiner avec les autres => deux graphes
}






