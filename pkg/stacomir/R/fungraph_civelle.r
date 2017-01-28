#' Graph function for glass eel migration. Differs from fungraph as it does not
#' draw the ggplot graph for month
#' 
#' This graph will also plot numbers and bars according to whether the glass
#' eel have been counted through weight or numbers
#' 
#' 
#' @param bilanMigration an object of class \code{\linkS4class{BilanMigration}} or an
#' #' object of class \code{\linkS4class{BilanMigrationMult}}
#' @param table a data frame with the results
#' @param time.sequence a vector POSIXt
#' @param taxon the species
#' @param stade the stage
#' @param dc the counting device, default to null, only necessary for \code{\linkS4class{BilanMigrationMult}}
#' @param silent Message displayed or not
#' @param ... additional parameters passed from the plot method to plot
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
fungraph_civelle=function(bilanMigration,table,time.sequence,taxon,stade,dc=null,silent,...){
# calcul des variables
	# pour adapter aux bilanMigrationMult, ligne par defaut...
	#cat("fungraph_civelle")
	if (is.null(dc)) dc=bilanMigration@dc@dc_selectionne[1]
	annee=paste(unique(strftime(as.POSIXlt(time.sequence),"%Y")),collapse=",")
	mois= months(time.sequence)
	jour= strftime(as.POSIXlt(time.sequence),"%j")
	index=table$No.pas+1
	eff=table$Effectif_total
	eff.p=table$Effectif_total.p
	debut=unclass(as.Date(time.sequence[min(index)]))[[1]]
	fin=unclass(as.Date(time.sequence[max(index)]))[[1]]
	eff[eff==0]<-NA #pour les besoins du graphe
	eff.p[eff.p==0]<-NA
	dis_commentaire=  as.character(bilanMigration@dc@data$dis_commentaires[bilanMigration@dc@data$dc%in%dc])
	if (!silent) funout(gettextf("Glass eels graph %s\n",dis_commentaire))
	###################################
	# Graph annuel couvrant sequence >0
	####################################
	
	vec<-c(rep(1,15),rep(2,2),rep(3,2),4,rep(5,6))
	mat <- matrix(vec,length(vec),1)
	layout(mat)
	mypalette<-RColorBrewer::brewer.pal(12,"Paired")
	#par("bg"=grDevices::gray(0.8))
	graphics::par("mar"=c(3, 4, 3, 2) + 0.1)
	#mypalette<-grDevices::rainbow(20)
	plot(as.Date(time.sequence,"Europe/Paris"),eff/1000,
			col=mypalette[8],
			type="h",
			xlim=c(debut,fin),
			ylim=c(0,max(eff/1000,na.rm=TRUE))*1.2 ,
			lty=1,
			xaxt="n",
			ylab=gettext("Number of glass eels (x1000)"),
			#xlab="date",
			cex.main=1,
			font.main=1,
			main=gettextf("Glass eels graph %s, %s, %s, %s,...",dis_commentaire,taxon,stade,annee))
	#print(plot,position = c(0, .3, 1, .9), more = TRUE)
	r <- as.Date(round(range(time.sequence), "day"))
	axis.Date(1, at=seq(r[1], r[2], by="weeks"),format="%d-%b")
	
	points(as.Date(time.sequence,"Europe/Paris"),eff.p/1000,
			type="h",
			lty=1,
			col=mypalette[10])
	
	legend(x="topright",
			inset=0.01,
			legend= gettext("weight of the daily number","daily number counted"),
			pch=c(16,16),
			col=mypalette[c(10,8)])
	
	text(  x=debut+(fin-debut)/8,
			y=max(eff/1000,na.rm=TRUE)*1.15,
			labels=paste(round(sum(table$poids_depuis_effectifs,na.rm=TRUE)/1000,2)," kg"),
			col=mypalette[8], 
			adj=1)
	text(  x=debut+3*(fin-debut)/8 ,
			y=max(eff/1000,na.rm=TRUE)*1.15,
			labels= paste("N=",round(sum(table$Effectif_total.e,na.rm=TRUE))),
			col=mypalette[8], 
			adj=1)
	text(  x=debut+(fin-debut)/8,
			y=max(eff/1000,na.rm=TRUE)*1.2,
			labels=paste(round(sum(table$Poids_total,na.rm=TRUE)/1000,2)," kg"),
			col=mypalette[10], 
			adj=1)
	text(  x=debut+3*(fin-debut)/8,
			y=max(eff/1000,na.rm=TRUE)*1.2,
			labels= paste("N=",round(sum(eff.p,na.rm=TRUE))),
			col=mypalette[10], 
			adj=1)
	text(  x=debut+3+(fin-debut)/8,
			y=max(eff/1000,na.rm=TRUE)*1.1,
			labels=paste(round(sum(table$Poids_total,table$poids_depuis_effectifs,na.rm=TRUE)/1000,2)," kg"),
			col="black", 
			adj=1)
	text(  x=debut+3*(fin-debut)/8,
			y=max(eff/1000,na.rm=TRUE)*1.1,
			labels= paste("N=",round(sum(eff,na.rm=TRUE))),
			col="black", 
			adj=1)
	segments(x0=debut,y0=max(eff/1000,na.rm=TRUE)*1.125,
			x1=debut+3*(fin-debut)/8,y1=max(eff/1000,na.rm=TRUE)*1.125)
	
	
	bilanOperation<-get("bilanOperation",envir=envir_stacomi)
	t_operation_ope<-bilanOperation@data[bilanOperation@data$ope_dic_identifiant==dc,]
	dif=difftime(t_operation_ope$ope_date_fin,t_operation_ope$ope_date_debut, units ="days")
	
	if (!silent){
		funout(gettextf("number of operations =%s\n",nrow(t_operation_ope)))
		funout(gettextf("average trapping time = %sdays\n",round(mean(as.numeric(dif)),2)))
		funout(gettextf("maximum term = %sdays\n",round(max(as.numeric(dif)),2)))
		funout(gettextf("minimum term = %sdays\n",round(min(as.numeric(dif)),2)))
	}
	
	df<-bilanMigration@dc@data$df[bilanMigration@dc@data$dc==dc]
	bilanFonctionnementDF<-get("bilanFonctionnementDF",envir=envir_stacomi)
	bilanFonctionnementDC<-get("bilanFonctionnementDC", envir=envir_stacomi)
	bilanFonctionnementDF@data<-bilanFonctionnementDF@data[bilanFonctionnementDF@data$per_dis_identifiant==df,]
	bilanFonctionnementDC@data<-bilanFonctionnementDC@data[bilanFonctionnementDC@data$per_dis_identifiant==dc,]
	
	
	graphdate<-function(vectordate){
		attributes(vectordate)<-NULL
		unclass(vectordate)
	}
	
	
	###################################         
	# creation d'un graphique vide (2)
	###################################
	
	graphics::par("mar"=c(0, 4, 0, 2)+ 0.1)  
	plot(   as.Date(time.sequence),
			seq(0,3,length.out=length(eff)),
			xlim=c(debut,fin), 
			type= "n", 
			xlab="",
			xaxt="n",
			yaxt="n", 
			ylab="Fishway",
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
				legend= c(gettext("working"),gettext("stopped"),gettext("normal operation")),
				pch=c(16,16),
				col=c(mypalette[4],mypalette[6],mypalette[1]),
				horiz=TRUE,
				bty="n"
		)
		
		
	} else {
		
		
		# si il sort quelque chose
		if (sum(bilanFonctionnementDF@data$per_etat_fonctionnement==1)>0){    
			rect(   xleft =graphdate(as.Date(bilanFonctionnementDF@data$per_date_debut[
											bilanFonctionnementDF@data$per_etat_fonctionnement==1])), 
					ybottom=2.1,
					xright=graphdate(as.Date(bilanFonctionnementDF@data$per_date_fin[
											bilanFonctionnementDF@data$per_etat_fonctionnement==1])),
					ytop=3, 
					col = mypalette[4],
					border = NA, 
					lwd = 1)       }
		if (sum(bilanFonctionnementDF@data$per_etat_fonctionnement==0)>0){              
			rect(   xleft =graphdate(as.Date(bilanFonctionnementDF@data$per_date_debut[
											bilanFonctionnementDF@data$per_etat_fonctionnement==0])), 
					ybottom=2.1,
					xright=graphdate(as.Date(bilanFonctionnementDF@data$per_date_fin[
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
						libelle=bilanFonctionnementDF@data$libelle)
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
				legend= c(gettext("work"),gettext("stop"),nomperiode),
				pch=c(15,15),
				col=c(mypalette[4],mypalette[6],mypalette[1:length(listeperiode)]),
				bty="n",
				ncol=7,
				text.width=(fin-debut)/10)
	}
	
	###################################         
	# creation d'un graphique vide (3=DC)
	###################################                 
	
	
	graphics::par("mar"=c(0, 4, 0, 2)+ 0.1)  
	plot(   as.Date(time.sequence),
			seq(0,3,length.out=length(eff)),
			xlim=c(debut,fin), 
			type= "n", 
			xlab="",
			xaxt="n",
			yaxt="n", 
			ylab=gettext("CD"),
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
				legend=c(gettext("working"),gettext("stopped"),gettext("normal operation")),
				pch=c(16,16),
				col=c(mypalette[4],mypalette[6],mypalette[1]),
				#horiz=TRUE,
				ncol=5,
				bty="n")
		
		
	} else {
		
		if (sum(bilanFonctionnementDC@data$per_etat_fonctionnement==1)>0){ 
			rect(   xleft =graphdate(as.Date(bilanFonctionnementDC@data$per_date_debut[
											bilanFonctionnementDC@data$per_etat_fonctionnement==1])), 
					ybottom=2.1,
					xright=graphdate(as.Date(bilanFonctionnementDC@data$per_date_fin[
											bilanFonctionnementDC@data$per_etat_fonctionnement==1])),
					ytop=3, 
					col = mypalette[4],
					border = NA, 
					lwd = 1) }
		if (sum(bilanFonctionnementDC@data$per_etat_fonctionnement==0)>0)
		{ 
			rect(   xleft =graphdate(as.Date(bilanFonctionnementDC@data$per_date_debut[
											bilanFonctionnementDC@data$per_etat_fonctionnement==0])), 
					ybottom=2.1,
					xright=graphdate(as.Date(bilanFonctionnementDC@data$per_date_fin[
											bilanFonctionnementDC@data$per_etat_fonctionnement==0])),
					ytop=3, 
					col = mypalette[6],
					border = NA, 
					lwd = 1) }
		listeperiode<-
				fn_table_per_dis(typeperiode=bilanFonctionnementDC@data$per_tar_code,
						tempsdebut= bilanFonctionnementDC@data$per_date_debut,
						tempsfin=bilanFonctionnementDC@data$per_date_fin,
						libelle=bilanFonctionnementDC@data$libelle)
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
				legend= c("working","stopped",nomperiode),
				pch=c(15,15),
				col=c(mypalette[4],mypalette[6],mypalette[1:length(listeperiode)]),
				bty="n",
				ncol=7,
				text.width=(fin-debut)/10)
	}
	
	###################################         
	# creation d'un graphique vide (4=OP)
	###################################                 
	
	
	graphics::par("mar"=c(0, 4, 0, 2)+ 0.1)  
	plot(   as.Date(time.sequence),
			seq(0,1,length.out=length(eff)),
			xlim=c(debut,fin), 
			type= "n", 
			xlab="",
			xaxt="n",
			yaxt="n", 
			ylab=gettext("Op"),
			bty="n",
			cex=1.2)             
	###################################         
	# operations
	###################################  
	rect(   xleft =graphdate(as.Date(t_operation_ope$ope_date_debut)), 
			ybottom=0,
			xright=graphdate(as.Date(t_operation_ope$ope_date_fin)),
			ytop=1, 
			col = RColorBrewer::brewer.pal(4,"Paired"),
			border = NA, 
			lwd = 1)
	
	
	###################################
	# Graph mensuel 
	####################################
	graphics::par("mar"=c(4, 4, 1, 2) + 0.1)
	petitmois=substr(as.character(mois),1,3)
	effmois=tapply(eff, mois, sum, na.rm=TRUE)[c(5,4,9,2,8,7,6,1,12,11,10,3)]
	effmois.p=tapply(eff.p, mois, sum, na.rm=TRUE)[c(5,4,9,2,8,7,6,1,12,11,10,3)]
	effmois<-data.frame("eff"=effmois)
	effmois.p<-data.frame("eff"=effmois.p)
	tablemens<-rbind(cbind("eff"=effmois-effmois.p,"type"=2,"mois"=1:12),
			cbind(effmois.p,"type"="1","mois"=1:12))
	
	
	superpose.polygon<-lattice::trellis.par.get("superpose.polygon")
	superpose.polygon$col=   mypalette[c(10,8)]
	superpose.polygon$border=rep("transparent",6)
	lattice::trellis.par.set("superpose.polygon",superpose.polygon)
	fontsize<-lattice::trellis.par.get("fontsize")
	fontsize$text=10
	lattice::trellis.par.set("fontsize",fontsize)
	par.main.text<-lattice::trellis.par.get("par.main.text")
	par.main.text$cex=1
	par.main.text$font=1
	lattice::trellis.par.set("par.main.text",par.main.text)
	
	
	par.ylab.text<-lattice::trellis.par.get("par.ylab.text")
	par.ylab.text$cex=0.8
	lattice::trellis.par.set("par.ylab.text",par.ylab.text) 
	par.xlab.text<-lattice::trellis.par.get("par.xlab.text")
	par.xlab.text$cex=0.8
	lattice::trellis.par.set("par.xlab.text",par.xlab.text)
	
	
	bar<-lattice::barchart(eff/1000~as.factor(mois),
			groups=as.factor(type),
			xlab=gettext("Month"),
			ylab=gettext("Number (x1000)"),
			#    main=list(label=paste("Donnees mensuelles")),
			data=tablemens,
			allow.multiple=FALSE,
#			key=lattice::simpleKey(text=c(gettext("weight of monthly number"),gettext("monthly number counted")),
#					rectangles = TRUE, 
#					points=FALSE, 
#					space="right",
#					cex=0.8),
			strip=FALSE,
			stack=TRUE)
	print(bar,position = c(0, 0, 1, .25),newpage = FALSE)
	
}
