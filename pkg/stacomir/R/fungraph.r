#' Function for BilanMigration graphes including numbers DF DC operations
#' 
#' This graph is for species other than glass eel
#' 
#' 
#' @usage fungraph(bilanMigration, tableau, time.sequence, taxon, stade, dc=NULL,silent,...)
#' @param bilanMigration An object of class \code{\linkS4class{BilanMigration}}
#' @param tableau A data frame with the with the following columns : No.pas,debut_pas,fin_pas,              
#' ope_dic_identifiant,lot_tax_code,lot_std_code,type_de_quantite,MESURE,CALCULE,               
#' EXPERT,PONCTUEL,Effectif_total,taux_d_echappement,coe_valeur_coefficient
#' @note this function is intended to be called from the plot method in BilanMigrationMult and BilanMigration
#' @param time.sequence A vector POSIXt
#' @param taxon The species
#' @param stade The stage
#' @param dc The DC
#' @param silent Message displayed or not
#' @param ... other parameters passed from the plot method to the matplot function
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
fungraph=function(bilanMigration,tableau,time.sequence,taxon,stade,dc=NULL,silent,...){
#mat <- matrix(1:6,3,2)
#layout(mat)
	#browser() 
	#cat("fungraph")
	if (is.null(dc)) dc=bilanMigration@dc@dc_selectionne[1]
	annee=unique(strftime(as.POSIXlt(time.sequence),"%Y"))
	mois= months(time.sequence)
	jour= strftime(as.POSIXlt(time.sequence),"%j")
	jmois=strftime(as.POSIXlt(time.sequence),"%d")
	mois=unique(mois)
	mois=paste("15",substr(as.character(mois),1,3))
	index=as.vector(tableau$No.pas[jmois==15])
	x=1:nrow(tableau)
	debut=unclass(as.POSIXct((min(time.sequence))))[[1]] # attention arrondit e un jour de moins
	fin=unclass(as.POSIXct(max(time.sequence)))[[1]]
	dis_commentaire=  as.character(bilanMigration@dc@data$dis_commentaires[bilanMigration@dc@data$dc%in%dc]) # commentaires sur le DC
	###################################
	# Definition du layout
	####################################
	vec<-c(rep(1,15),rep(2,2),rep(3,2),4,rep(5,6))
	mat <- matrix(vec,length(vec),1)
	layout(mat)
	mypalette<-rev(c("black","deepskyblue","chartreuse2","indianred"))
	#par("bg"=grDevices::gray(0.8))
	graphics::par("mar"=c(3, 4, 3, 2) + 0.1)
	###################################
	# Graph annuel couvrant sequence >0
	####################################
	matplot(x,cbind(tableau$MESURE+tableau$CALCULE+tableau$EXPERT+tableau$PONCTUEL,
					tableau$MESURE+tableau$CALCULE+tableau$EXPERT,
					tableau$MESURE+tableau$CALCULE,
					tableau$MESURE),
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
	if(bilanMigration@pasDeTemps@stepDuration=="86400"){ # pas de temps journalier
		index=as.vector(x[jmois==15])
		axis(side=1,at=index,tick=TRUE,labels=mois)
		#axis(side=1,at=as.vector(x[jmois==1]),tick=TRUE,labels=FALSE)
		
	} else {
		axis(side=1)
	}  	
	mtext(text=paste(get("msg",envir=envir_stacomi)$fungraph.6,
					round(sum(tableau$MESURE,tableau$CALCULE,tableau$EXPERT,tableau$PONCTUEL,na.rm=TRUE))),
			side=3,
			col=RColorBrewer::brewer.pal(5,"Paired")[5],
			cex=0.8)
	
	legend(x=0,
			y=max(tableau$MESURE,tableau$CALCULE,tableau$EXPERT,tableau$PONCTUEL,na.rm=TRUE),
			legend= get("msg",envir=envir_stacomi)$fungraph.5,
			pch=c(16),
			col=rev(c(mypalette[1:4])))
	bilanOperation<-get("bilanOperation",envir=envir_stacomi)
	t_operation_ope<-bilanOperation@data[bilanOperation@data$ope_dic_identifiant==dc,]
	dif=difftime(t_operation_ope$ope_date_fin,t_operation_ope$ope_date_debut, units ="days")
	
	if (!silent){
		funout(paste(get("msg",envir=envir_stacomi)$fungraph_civelle.5,nrow(t_operation_ope),"\n"))
		funout(paste(get("msg",envir=envir_stacomi)$fungraph_civelle.6,round(mean(as.numeric(dif)),2),get("msg",envir=envir_stacomi)$fungraph.8))
		funout(paste(get("msg",envir=envir_stacomi)$fungraph_civelle.7,round(max(as.numeric(dif)),2),get("msg",envir=envir_stacomi)$fungraph.8))
		funout(paste(get("msg",envir=envir_stacomi)$fungraph_civelle.8,round(min(as.numeric(dif)),2),get("msg",envir=envir_stacomi)$fungraph.8))
	}
	

	df<-bilanMigration@dc@data$df[bilanMigration@dc@data$dc==dc]
	bilanFonctionnementDF<-get("bilanFonctionnementDF",envir=envir_stacomi)
	bilanFonctionnementDC<-get("bilanFonctionnementDC", envir=envir_stacomi)
	bilanFonctionnementDF@data<-bilanFonctionnementDF@data[bilanFonctionnementDF@data$per_dis_identifiant==df,]
	bilanFonctionnementDC@data<-bilanFonctionnementDC@data[bilanFonctionnementDC@data$per_dis_identifiant==dc,]

	
	
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
	mypalette<-RColorBrewer::brewer.pal(12,"Paired")
	graphics::par("mar"=c(0, 4, 0, 2)+ 0.1)  
	plot(   as.POSIXct(time.sequence),
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
	
	if (dim(bilanFonctionnementDF@data)[1]==0 ) {
		
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
		if (sum(bilanFonctionnementDF@data$per_etat_fonctionnement==1)>0){    
			rect(   xleft =graphdate(as.POSIXct(bilanFonctionnementDF@data$per_date_debut[
											bilanFonctionnementDF@data$per_etat_fonctionnement==1])), 
					ybottom=2.1,
					xright=graphdate(as.POSIXct(bilanFonctionnementDF@data$per_date_fin[
											bilanFonctionnementDF@data$per_etat_fonctionnement==1])),
					ytop=3, 
					col = mypalette[4],
					border = NA, 
					lwd = 1)       }
		if (sum(bilanFonctionnementDF@data$per_etat_fonctionnement==0)>0){              
			rect(   xleft =graphdate(as.POSIXct(bilanFonctionnementDF@data$per_date_debut[
											bilanFonctionnementDF@data$per_etat_fonctionnement==0])), 
					ybottom=2.1,
					xright=graphdate(as.POSIXct(bilanFonctionnementDF@data$per_date_fin[
											bilanFonctionnementDF@data$per_etat_fonctionnement==0])),
					ytop=3, 
					col = mypalette[6],
					border = NA, 
					lwd = 1)  }
		#creation d'une liste par categorie d'arret contenant vecteurs dates    
		listeperiode<-
				fn_table_per_dis(typeperiode=bilanFonctionnementDF@data$per_tar_code,
						tempsdebut= bilanFonctionnementDF@data$per_date_debut,
						tempsfin=bilanFonctionnementDF@data$per_date_fin,
						libelle=bilanFonctionnementDF@data$libelle,
						date=FALSE)
		nomperiode<-vector()
		for (j in 1 : length(listeperiode)){
			#recuperation du vecteur de noms (dans l'ordre) e partir de la liste
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
				text.width=(fin-debut)/10)
	}
	
	###################################         
	# creation d'un graphique vide (3)
	###################################                 
	
	graphics::par("mar"=c(0, 4, 0, 2)+ 0.1)  
	plot(   as.POSIXct(time.sequence),
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
	
	
	if (dim(bilanFonctionnementDC@data)[1]==0 ) {
		
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
		
		if (sum(bilanFonctionnementDC@data$per_etat_fonctionnement==1)>0){ 
			rect(   xleft =graphdate(as.POSIXct(bilanFonctionnementDC@data$per_date_debut[
											bilanFonctionnementDC@data$per_etat_fonctionnement==1])), 
					ybottom=2.1,
					xright=graphdate(as.POSIXct(bilanFonctionnementDC@data$per_date_fin[
											bilanFonctionnementDC@data$per_etat_fonctionnement==1])),
					ytop=3, 
					col = mypalette[4],
					border = NA, 
					lwd = 1) }
		if (sum(bilanFonctionnementDC@data$per_etat_fonctionnement==0)>0)
		{ 
			rect(   xleft =graphdate(as.POSIXct(bilanFonctionnementDC@data$per_date_debut[
											bilanFonctionnementDC@data$per_etat_fonctionnement==0])), 
					ybottom=2.1,
					xright=graphdate(as.POSIXct(bilanFonctionnementDC@data$per_date_fin[
											bilanFonctionnementDC@data$per_etat_fonctionnement==0])),
					ytop=3, 
					col = mypalette[6],
					border = NA, 
					lwd = 1) }
		listeperiode<-
				fn_table_per_dis(typeperiode=bilanFonctionnementDC@data$per_tar_code,
						tempsdebut= bilanFonctionnementDC@data$per_date_debut,
						tempsfin=bilanFonctionnementDC@data$per_date_fin,
						libelle=bilanFonctionnementDC@data$libelle,
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
				text.width=(fin-debut)/10)
	}
	
	###################################         
	# creation d'un graphique vide (4=op)
	###################################                 
	
	
	graphics::par("mar"=c(0, 4, 0, 2)+ 0.1)  
	plot(   as.POSIXct(time.sequence),
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
	if(stacomirtools::is.odd(dc)) brew="Paired" else brew="Accent"
	rect(   xleft =graphdate(as.POSIXct(t_operation_ope$ope_date_debut)), 
			ybottom=0,
			xright=graphdate(as.POSIXct(t_operation_ope$ope_date_fin)),
			ytop=1, 
			col = RColorBrewer::brewer.pal(8,brew),
			border = NA, 
			lwd = 1)
	
	
	###################################
	# Graph mensuel 
	####################################
	# Pour les noms modifier get("msg",envir=envir_stacomi)$fungraph.7
	graphics::par("mar"=c(4, 4, 1, 2) + 0.1)
	tableau$mois=factor(months(tableau$debut_pas,abbreviate=TRUE),
			levels=unique(months(tableau$debut_pas,abbreviate=TRUE)))
	tableaum<-reshape2::melt(data=tableau[,c("MESURE","CALCULE","EXPERT","PONCTUEL","mois")],							
			id.vars=c("mois"),
			measure.vars=c("MESURE","CALCULE","EXPERT","PONCTUEL"),
			variable.name="type",
			value.name="number")
	levels(tableaum$type)<-get("msg",envir=envir_stacomi)$fungraph.5
	superpose.polygon<-lattice::trellis.par.get("plot.polygon")
	superpose.polygon$col=  c("black","deepskyblue","chartreuse2","indianred")
	superpose.polygon$border=rep("transparent",6)
	lattice::trellis.par.set("superpose.polygon",superpose.polygon)
	fontsize<-lattice::trellis.par.get("fontsize")
	fontsize$text=10
	lattice::trellis.par.set("fontsize",fontsize)
	par.main.text<-lattice::trellis.par.get("par.main.text")
	par.main.text$cex=1
	par.main.text$font=1
	lattice::trellis.par.set("par.main.text",par.main.text)
	# lattice::show.settings()
	
	par.ylab.text<-lattice::trellis.par.get("par.ylab.text")
	par.ylab.text$cex=0.8
	lattice::trellis.par.set("par.ylab.text",par.ylab.text) 
	par.xlab.text<-lattice::trellis.par.get("par.xlab.text")
	par.xlab.text$cex=0.8
	lattice::trellis.par.set("par.xlab.text",par.xlab.text)
	
	bar<-lattice::barchart(number/1000~mois,
			groups=type,
			xlab=get("msg",envir=envir_stacomi)$fungraph_civelle.14,
			ylab=get("msg",envir=envir_stacomi)$fungraph_civelle.15,
			#    main=list(label=paste("Donnees mensuelles")),
			data=tableaum,
			allow.multiple=FALSE,
			strip=FALSE,
			stack=TRUE,
			key=lattice::simpleKey(text=levels(tableaum$type),
					rectangles = TRUE, 
					points=FALSE, 
					space="right", 
					cex=0.8),
			origin=0)
	print(bar,position = c(0, 0, 1, .25),newpage = FALSE)
	
	
#	
#	
#	X11(7,4)
#	stktab=cbind(utils::stack(tableau[,c("MESURE","CALCULE","EXPERT","PONCTUEL")]),"time.sequence"=rep(time.sequence,4))
#	stktab<-funtraitementdate(stktab,
#			nom_coldt="time.sequence",
#			annee=FALSE,
#			mois=TRUE,
#			quinzaine=TRUE,
#			semaine=TRUE,
#			jour_an=TRUE,
#			jour_mois=FALSE,
#			heure=FALSE)
#	stktab$ind<-factor(stktab$ind, levels = c("MESURE","CALCULE","EXPERT","PONCTUEL"))
#	fillname<-get("msg",envir=envir_stacomi)$fungraph.7[2] #type
#	mypalette<-rev(c("black","deepskyblue","chartreuse2","indianred"))
#	g<-ggplot(stktab, aes(x=mois,y=values,fill=ind))+
#			geom_bar(position="dodge", stat="identity")+
#			scale_fill_manual(name=fillname,values=c("MESURE"=mypalette[4],
#							"CALCULE"=mypalette[3],
#							"EXPERT"=mypalette[2],
#							"PONCTUEL"=mypalette[1]))+
#			xlab(get("msg",envir=envir_stacomi)$fungraph.7[4])+ # mois or month+
#			ylab(get("msg",envir=envir_stacomi)$fungraph.7[1]) # Nombre ou Numbers
#	nothing<-print(g)
	# pour l'instant je ne peux pas combiner avec les autres => deux graphes
	return(invisible(NULL))
}






