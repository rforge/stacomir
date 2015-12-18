# Nom fichier :        fungraph_civelle.R
# Date de creation :  01/02/2006 15:37:44
# Description :       graph pour bilan migration journalier civelles


#' Graph function for glass eel migration. Differs from fungraph as it does not
#' draw the ggplot graph for month
#' 
#' This graph will also plot numbers and bars according to whether the glass
#' eel have been counted through weight or numbers
#' 
#' 
#' @usage fungraph_civelle(bilanMigration, table, duree, taxon, stade)
#' @param bilanMigration an objet of class \code{\linkS4class{BilanMigration}}
#' @param table=tableau a data frame with the results
#' @param duree a vector POSIXt
#' @param taxon the species
#' @param stade the stage
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
fungraph_civelle=function(bilanMigration,table,duree,taxon,stade){
# calcul des variables
	annee=unique(strftime(as.POSIXlt(duree),"%Y"))
	mois= months(duree)
	jour= strftime(as.POSIXlt(duree),"%j")
	index=table$No.pas+1
	eff=table$Effectif_total
	eff.p=table$Effectif_total.p
	debut=unclass(as.Date(duree[min(index)]))[[1]]
	fin=unclass(as.Date(duree[max(index)]))[[1]]
	eff[eff==0]<-NA #pour les besoins du graphe
	eff.p[eff.p==0]<-NA
	
	bilanMigration<-bilanMigration
	dis_commentaire=  as.character(bilanMigration@dc@data$dis_commentaires[bilanMigration@dc@data$dc%in%bilanMigration@dc@dc_selectionne])
	funout(paste(get("msg",envir=envir_stacomi)$fungraph_civelle.1,dis_commentaire,"\n"))
	###################################
	# Graph annuel couvrant sequence >0
	####################################
	
	vec<-c(rep(1,15),rep(2,2),rep(3,2),4,rep(5,6))
	mat <- matrix(vec,length(vec),1)
	layout(mat)
	mypalette<-brewer.pal(12,"Paired")
	#par("bg"=gray(0.8))
	par("mar"=c(3, 4, 3, 2) + 0.1)
	#mypalette<-rainbow(20)
	plot(as.Date(duree),eff/1000,
			col=mypalette[8],
			type="h",
			xlim=c(debut,fin),
			ylim=c(0,max(eff/1000,na.rm=TRUE))*1.2 ,
			lty=1,
			xaxt="n",
			ylab=get("msg",envir=envir_stacomi)$fungraph_civelle.2,
			#xlab="date",
			cex.main=1,
			font.main=1,
			main=paste(get("msg",envir=envir_stacomi)$fungraph_civelle.3, dis_commentaire,",", 
					taxon,",", stade,",", annee ))
	#print(plot,position = c(0, .3, 1, .9), more = TRUE)
	r <- as.Date(round(range(duree), "day"))
	axis.Date(1, at=seq(r[1], r[2], by="weeks"),format="%d-%b")
	
	points(as.Date(duree),eff.p/1000,
			type="h",
			lty=1,
			col=mypalette[10])
	
	legend(x="topright",
			inset=0.01,
			legend= get("msg",envir=envir_stacomi)$fungraph_civelle.4,
			pch=c(16,16),
			col=mypalette[c(10,8)])
	
	text(  x=debut+(fin-debut)/8,
			y=max(eff/1000,na.rm=TRUE)*1.15,
			labels=paste(round(sum(table$poids_depuis_effectifs)/1000,2)," kg"),
			col=mypalette[8], 
			adj=1)
	text(  x=debut+3*(fin-debut)/8 ,
			y=max(eff/1000,na.rm=TRUE)*1.15,
			labels= paste("N=",round(sum(table$Effectif_total.e,na.rm=TRUE))),
			col=mypalette[8], 
			adj=1)
	text(  x=debut+(fin-debut)/8,
			y=max(eff/1000,na.rm=TRUE)*1.2,
			labels=paste(round(sum(table$Poids_total)/1000,2)," kg"),
			col=mypalette[10], 
			adj=1)
	text(  x=debut+3*(fin-debut)/8,
			y=max(eff/1000,na.rm=TRUE)*1.2,
			labels= paste("N=",round(sum(eff.p,na.rm=TRUE))),
			col=mypalette[10], 
			adj=1)
	text(  x=debut+3+(fin-debut)/8,
			y=max(eff/1000,na.rm=TRUE)*1.1,
			labels=paste(round(sum(table$Poids_total,table$poids_depuis_effectifs)/1000,2)," kg"),
			col="black", 
			adj=1)
	text(  x=debut+3*(fin-debut)/8,
			y=max(eff/1000,na.rm=TRUE)*1.1,
			labels= paste("N=",round(sum(eff,na.rm=TRUE))),
			col="black", 
			adj=1)
	segments(x0=debut,y0=max(eff/1000,na.rm=TRUE)*1.125,
			x1=debut+3*(fin-debut)/8,y1=max(eff/1000,na.rm=TRUE)*1.125)
	
	
	
	###################################         
	# Requete de la base
	################################### 
	req<-new("RequeteODBC")
	req@baseODBC<-get("baseODBC",envir=envir_stacomi)
	req@sql=paste("SELECT * FROM  t_operation_ope ",
			"WHERE ope_date_debut >= '",
			as.Date(duree[min(index)]),
			" 00:00:00' ",
			"AND ope_date_fin <= '" ,
			as.Date(duree[max(index)]),
			" 00:00:00' ",
			"AND ope_dic_identifiant=",bilanMigration@dc@dc_selectionne,
			"ORDER BY ope_date_debut; ",sep = "")
	t_operation_ope<-connect(req)@query
	# sortie de commentaires
	dif=difftime(t_operation_ope$ope_date_fin,t_operation_ope$ope_date_debut, units ="days")
	funout(paste(get("msg",envir=envir_stacomi)$fungraph_civelle.5,nrow(t_operation_ope),"\n"))
	funout(paste(get("msg",envir=envir_stacomi)$fungraph_civelle.6,round(mean(as.numeric(dif)),2),"jours","\n"))
	funout(paste(get("msg",envir=envir_stacomi)$fungraph_civelle.7,round(max(as.numeric(dif)),2),"jours","\n"))
	funout(paste(get("msg",envir=envir_stacomi)$fungraph_civelle.8,round(min(as.numeric(dif)),2),"jours","\n"))
	

	req@sql<-fn_sql_dis(per_dis_identifiant=
					bilanMigration@dc@data$df[bilanMigration@dc@data$dc%in%bilanMigration@dc@dc_selectionne],
			dateDebut=as.Date(duree[min(index)]),
			dateFin=as.Date(duree[max(index)]))
	fonctionnementDF<-connect(req)@query
	
	
	req@sql<-fn_sql_dis(per_dis_identifiant=bilanMigration@dc@dc_selectionne,
			dateDebut=as.Date(duree[min(index)]),
			dateFin=as.Date(duree[max(index)]))
	fonctionnementDC<-connect(req)@query
	
	
	graphdate<-function(vectordate){
		attributes(vectordate)<-NULL
		unclass(vectordate)
	}
	
	
	###################################         
	# creation d'un graphique vide (2)
	###################################
	
	par("mar"=c(0, 4, 0, 2)+ 0.1)  
	plot(   as.Date(duree),
			seq(0,3,length.out=length(eff)),
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
			rect(   xleft =graphdate(as.Date(fonctionnementDF$per_date_debut[
											fonctionnementDF$per_etat_fonctionnement==1])), 
					ybottom=2.1,
					xright=graphdate(as.Date(fonctionnementDF$per_date_fin[
											fonctionnementDF$per_etat_fonctionnement==1])),
					ytop=3, 
					col = mypalette[4],
					border = NA, 
					lwd = 1)       }
		if (sum(fonctionnementDF$per_etat_fonctionnement==0)>0){              
			rect(   xleft =graphdate(as.Date(fonctionnementDF$per_date_debut[
											fonctionnementDF$per_etat_fonctionnement==0])), 
					ybottom=2.1,
					xright=graphdate(as.Date(fonctionnementDF$per_date_fin[
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
						libelle=fonctionnementDF$libelle)
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
	plot(   as.Date(duree),
			seq(0,3,length.out=length(eff)),
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
				legend=get("msg",envir=envir_stacomi)$fungraph_civelle.9,
				pch=c(16,16),
				col=c(mypalette[4],mypalette[6],mypalette[1]),
				#horiz=TRUE,
				ncol=5,
				bty="n")
		
		
	} else {
		
		if (sum(fonctionnementDC$per_etat_fonctionnement==1)>0){ 
			rect(   xleft =graphdate(as.Date(fonctionnementDC$per_date_debut[
											fonctionnementDC$per_etat_fonctionnement==1])), 
					ybottom=2.1,
					xright=graphdate(as.Date(fonctionnementDC$per_date_fin[
											fonctionnementDC$per_etat_fonctionnement==1])),
					ytop=3, 
					col = mypalette[4],
					border = NA, 
					lwd = 1) }
		if (sum(fonctionnementDC$per_etat_fonctionnement==0)>0)
		{ 
			rect(   xleft =graphdate(as.Date(fonctionnementDC$per_date_debut[
											fonctionnementDC$per_etat_fonctionnement==0])), 
					ybottom=2.1,
					xright=graphdate(as.Date(fonctionnementDC$per_date_fin[
											fonctionnementDC$per_etat_fonctionnement==0])),
					ytop=3, 
					col = mypalette[6],
					border = NA, 
					lwd = 1) }
		listeperiode<-
				fn_table_per_dis(typeperiode=fonctionnementDC$per_tar_code,
						tempsdebut= fonctionnementDC$per_date_debut,
						tempsfin=fonctionnementDC$per_date_fin,
						libelle=fonctionnementDC$libelle)
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
	
	
	par("mar"=c(0, 4, 0, 2)+ 0.1)  
	plot(   as.Date(duree),
			seq(0,1,length.out=length(eff)),
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
	rect(   xleft =graphdate(as.Date(t_operation_ope$ope_date_debut)), 
			ybottom=0,
			xright=graphdate(as.Date(t_operation_ope$ope_date_fin)),
			ytop=1, 
			col = brewer.pal(4,"Paired"),
			border = NA, 
			lwd = 1)
	
	
	###################################
	# Graph mensuel 
	####################################
	par("mar"=c(4, 4, 1, 2) + 0.1)
	petitmois=substr(as.character(mois),1,3)
	effmois=tapply(eff, mois, sum, na.rm=TRUE)[c(5,4,9,2,8,7,6,1,12,11,10,3)]
	effmois.p=tapply(eff.p, mois, sum, na.rm=TRUE)[c(5,4,9,2,8,7,6,1,12,11,10,3)]
	effmois<-data.frame("eff"=effmois)
	effmois.p<-data.frame("eff"=effmois.p)
	tablemens<-rbind(cbind("eff"=effmois-effmois.p,"type"=2,"mois"=1:12),
			cbind(effmois.p,"type"="1","mois"=1:12))
	
	
	superpose.polygon<-trellis.par.get("superpose.polygon")
	superpose.polygon$col=   mypalette[c(10,8)]
	superpose.polygon$border=FALSE
	trellis.par.set("superpose.polygon",superpose.polygon)
	fontsize<-trellis.par.get("fontsize")
	fontsize$text=10
	trellis.par.set("fontsize",fontsize)
	par.main.text<-trellis.par.get("par.main.text")
	par.main.text$cex=1
	par.main.text$font=1
	trellis.par.set("par.main.text",par.main.text)
	
	
	par.ylab.text<-trellis.par.get("par.ylab.text")
	par.ylab.text$cex=0.8
	trellis.par.set("par.ylab.text",par.ylab.text) 
	par.xlab.text<-trellis.par.get("par.xlab.text")
	par.xlab.text$cex=0.8
	trellis.par.set("par.xlab.text",par.xlab.text)
	
	
	bar<-barchart(eff/1000~as.factor(mois),
			groups=as.factor(type),
			xlab=get("msg",envir=envir_stacomi)$fungraph_civelle.14,
			ylab=get("msg",envir=envir_stacomi)$fungraph_civelle.15,
			#    main=list(label=paste("Donnees mensuelles")),
			data=tablemens,
			allow.multiple=FALSE,
			key=simpleKey(text=get("msg",envir=envir_stacomi)$fungraph_civelle.16,
					rectangles = TRUE, 
					points=FALSE, 
					x=0.70,
					y=0.85,
					cex=0.8),
			strip=FALSE,
			stack=TRUE)
	print(bar,position = c(0, 0, 1, .25),newpage = FALSE)
	
}