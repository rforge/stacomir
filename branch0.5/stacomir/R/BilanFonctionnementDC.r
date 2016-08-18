#' Class "BilanFonctionnementDC" Bilan du fonctionnement du dispositif de
#' comptage
#' 
#' The counting device is not always working. It may me stopped either
#' following a monitoring protocol, or due to misfunction of the device, this
#' class allows to draw graphics allowing an overview of the device operation
#' 
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanFonctionnementDC", ...)}.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Class \code{\linkS4class{Bilan_lot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @references \url{http://w3.eptb-vilaine.fr:8080/tracstacomi}
#' @concept Bilan Object 
#' @examples
#' 
#' showClass("BilanFonctionnementDC")
#' 
#' @export 
setClass(Class="BilanFonctionnementDC",
		representation= representation(data="data.frame",
				dc="RefDC",
				horodate="RefHorodate",
				requete="RequeteODBCwheredate"), 
		prototype=prototype(data=data.frame(),
				dc=new("RefDC"),
				horodate=new("RefHorodate"),
				requete=new("RequeteODBCwheredate"))
)




#' connect method for BilanFonctionnementDC
#' 
#' loads the working periods and type of arrest or disfunction of the DC
#' @return  An object of class \ref{BilanFonctionnementDC}
#' 
#' @author cedric.briand
setMethod("connect",signature=signature("BilanFonctionnementDC"),definition=function(object,h) {
#  construit une requete ODBCwheredate
			object@requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			object@requete@select= sql<-paste("SELECT",
					" per_dis_identifiant,",
					" per_date_debut,",
					" per_date_fin,",
					" per_commentaires,",
					" per_etat_fonctionnement,",
					" per_tar_code,",
					" tar_libelle AS libelle",
					" FROM  ",get("sch",envir=envir_stacomi),"t_periodefonctdispositif_per per",
					" INNER JOIN ref.tr_typearretdisp_tar tar ON tar.tar_code=per.per_tar_code",sep="")
			object@requete@colonnedebut<-"per_date_debut"
			object@requete@colonnefin<-"per_date_fin"
			object@requete@order_by<-"ORDER BY per_date_debut"
			object@requete@and<-paste("AND per_dis_identifiant=",object@dc@dc_selectionne )
#object@requete@where=#defini dans la methode ODBCwheredate
			object@requete<-connect(object@requete) # appel de la methode connect de l'object ODBCWHEREDATE
			funout(get("msg",envir_stacomi)$BilanFonctionnementDC.1)
			return(object)
		})

#' charge method for BilanFonctionnementDC
#' 
#' used by the graphical interface to retreive the objects of Referential classes
#' assigned to envir_stacomi
#' @return  An object of class \code{BilanFonctionnementDC}
#' 
#' @author cedric.briand
setMethod("charge",signature=signature("BilanFonctionnementDC"),definition=function(object,h) {
#  construit une requete ODBCwheredate
			# chargement des donnees dans l'environnement de la fonction
			if (exists("refDC",envir_stacomi)) {
				object@dc<-get("refDC",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.1,arret=TRUE)				}     
			
			if (exists("fonctionnementDC_date_debut",envir_stacomi)) {
				object@requete@datedebut<-get("fonctionnementDC_date_debut",envir_stacomi)@horodate
			} else {
				funout(get("msg",envir_stacomi)$ref.5,arret=TRUE)	
			}
			
			if (exists("fonctionnementDC_date_fin",envir_stacomi)) {
				object@requete@datefin<-get("fonctionnementDC_date_fin",envir_stacomi)@horodate
			} else {
				funout(get("msg",envir_stacomi)$ref.6,arret=TRUE)	
			}			
			object<-connect(object)			
			return(object)
		})
# Methode permettant l'affichage d'un graphique en lattice (barchart) du fonctionnement mensuel du dispositif
# Compte tenu de la structure des donnees ce n'est pas si simple... 

#' Function to create a barchart (lattice) corresponding to the periods
#' @param h A handler
#' @param ... 
#' @return assigns the data frame \code{periodeDC} allowing to build the lattice graph in the environment envir_stacomi
#' 
#' @author cedric.briand
#' @export
funbarchartDC = function(h,...) {
	fonctionnementDC=charge(fonctionnementDC)
	
	if( nrow(fonctionnementDC@requete@query)==0 ) {
		funout(get("msg",envir_stacomi)$BilanFonctionnementDC.2, arret=TRUE)
	}
	
	t_periodefonctdispositif_per<-fonctionnementDC@requete@query # on recupere le data.frame   
	# l'objectif du programme ci dessous est de calculer la duree mensuelle de fonctionnement du dispositif.
	tempsdebut<-strptime(t_periodefonctdispositif_per$per_date_debut,"%Y-%m-%d %H:%M:%S", tz = "GMT")
	tempsfin<-strptime(t_periodefonctdispositif_per$per_date_fin,"%Y-%m-%d %H:%M:%S", tz = "GMT")
	# test la premiere horodate peut etre avant le choix de temps de debut, remplacer cette date par requete@datedebut
	tempsdebut[tempsdebut<fonctionnementDC@requete@datedebut]<-fonctionnementDC@requete@datedebut
	# id pour fin
	tempsfin[tempsfin>fonctionnementDC@requete@datefin]<-fonctionnementDC@requete@datefin
	t_periodefonctdispositif_per=cbind(t_periodefonctdispositif_per,tempsdebut,tempsfin) # rajoute les 2 colonnes tempsdebut et tempsfin
	# BUG 06/02/2009 11:51:49 si la date choisie n'est pas le debut du mois
	seqmois<-seq(from=tempsdebut[1],to=tempsfin[nrow(t_periodefonctdispositif_per)],by="month",tz = "GMT")
	seqmois<-as.POSIXlt(round(seqmois,digits="months"))
	t_periodefonctdispositif_per_mois<-t_periodefonctdispositif_per[1,]
	
	z=0 # compteur tableau t_periodefonctdispositif_per_mois  
	for(j in 1:nrow(t_periodefonctdispositif_per)){     # pour toutes les lignes du ResultSet...
		#cat( j )
		if (j>1) t_periodefonctdispositif_per_mois=rbind(t_periodefonctdispositif_per_mois, t_periodefonctdispositif_per[j,])
		lemoissuivant<-seqmois[seqmois>tempsdebut[j]][1] # le premier mois superieur � tempsdebut
		
		# on est a cheval sur deux periodes 
		while (tempsfin[j]>lemoissuivant)
		{      	
			#if (z>0) stop("erreur")
			z=z+1
			t_periodefonctdispositif_per_mois<-rbind(t_periodefonctdispositif_per_mois, t_periodefonctdispositif_per[j,])
			t_periodefonctdispositif_per_mois[j+z,"tempsdebut"]<-as.POSIXct(lemoissuivant)
			t_periodefonctdispositif_per_mois[j+z-1,"tempsfin"]<-as.POSIXct(lemoissuivant)
			lemoissuivant<-seqmois[match(as.character(lemoissuivant),as.character(seqmois))+1] # on decale de 1 mois avant de rerentrer dans la boucle
			if (is.na(lemoissuivant) ) break
		}  
	}
	t_periodefonctdispositif_per_mois$sumduree<-as.numeric(difftime(t_periodefonctdispositif_per_mois$tempsfin, t_periodefonctdispositif_per_mois$tempsdebut,units = "hours"))
	t_periodefonctdispositif_per_mois$mois1<-strftime(as.POSIXlt(t_periodefonctdispositif_per_mois$tempsdebut),"%b")
	t_periodefonctdispositif_per_mois$mois<-strftime(as.POSIXlt(t_periodefonctdispositif_per_mois$tempsdebut),"%m")
	t_periodefonctdispositif_per_mois$annee<-strftime(as.POSIXlt(t_periodefonctdispositif_per_mois$tempsdebut),"%Y")
	superpose.polygon<-lattice::trellis.par.get("superpose.polygon")
	superpose.polygon$col<-c("#4C00FF","orange")    
	superpose.polygon$border<-FALSE
	lattice::trellis.par.set("superpose.polygon",superpose.polygon) 
	bar<-lattice::barchart(
			as.numeric(t_periodefonctdispositif_per_mois$sumduree)~as.factor(t_periodefonctdispositif_per_mois$mois)|as.factor(t_periodefonctdispositif_per_mois$annee),
			groups=t_periodefonctdispositif_per_mois$per_tar_code,
			stack=TRUE,
			xlab=get("msg",envir_stacomi)$BilanFonctionnementDC.3,
			ylab=get("msg",envir_stacomi)$BilanFonctionnementDC.4,
			main=list(label=paste(get("msg",envir_stacomi)$BilanFonctionnementDC.5,fonctionnementDC@dc@dc_selectionne), gp=grid::gpar(col="grey", fontsize=8)), 
			auto.key=list(rectangles=TRUE,space="bottom",
					text=c(get("msg",envir_stacomi)$BilanFonctionnementDC.6,get("msg",envir_stacomi)$FonctionnementDC.7)),
			scales= list(x=list(t_periodefonctdispositif_per_mois$mois),
					cex=0.5))    
	print(bar) 
	assign("periodeDC",t_periodefonctdispositif_per_mois,envir_stacomi)
	funout(get("msg",envir_stacomi)$BilanFonctionnementDC.8)	
}   


#' function used for some lattice graph 
#' 
#' @param h A handler
#' @param ... 
#' @export
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
funboxDC = function(h,...) {  
	fonctionnementDC=charge(fonctionnementDC)
	
	if( nrow(fonctionnementDC@requete@query)==0 ) {
		funout(get("msg",envir_stacomi)$BilanFonctionnementDC.2, arret=TRUE)
	}  
	t_periodefonctdispositif_per<-fonctionnementDC@requete@query # on recupere le data.frame
	duree<-seq.POSIXt(from=fonctionnementDC@requete@datedebut,to=fonctionnementDC@requete@datefin,by="day")
	debut<-unclass(as.Date(duree[1]))[[1]]
	fin<-unclass(as.Date(duree[length(duree)]))[[1]]
	mypalette<-RColorBrewer::brewer.pal(12,"Paired")
	#display.brewer.all()
	mypalette1<-c("#1B9E77","#AE017E","orange", RColorBrewer::brewer.pal(12,"Paired"))

	graphdate<-function(vectordate){
		attributes(vectordate)<-NULL
		unclass(vectordate)
	}
	###################################         
	# creation d'un graphique vide (2)
	###################################
	plot(   as.Date(duree),
			seq(0,1,length.out=length(duree)),
			xlim=c(debut,fin), 
			type= "n", 
			xlab="",
			xaxt="n",
			yaxt="n", 
			ylab=get("msg",envir_stacomi)$BilanFonctionnementDC.9,
			#bty="n",
			cex=0.8)
	r <- as.Date(round(range(duree), "day"))
	graphics::axis.Date(1, at=seq(r[1], r[2], by="weeks"),format="%d-%b")
	if (dim(t_periodefonctdispositif_per)[1]==0 ) {    # s'il n'y a pas de periode de fontionnement dans la base
		graphics::rect(   xleft=debut, 
				ybottom=0.6,
				xright=fin,
				ytop=0.9, 
				col = mypalette[4],
				border = NA, 
				lwd = 1)                     
		graphics::rect(   xleft=debut, 
				ybottom=0.1,
				xright=fin,
				ytop=0.4, 
				col = mypalette[1],
				border = NA, 
				lwd = 1)
		graphics::legend(  x= "bottom",
				legend=get("msg",envir_stacomi)$BilanFonctionnementDC.10 ,# three terms in the legend
				pch=c(16,16),
				col=c(mypalette[4],mypalette[6],mypalette[1]),
				#horiz=TRUE,
				ncol=5,
				bty="n")
	} else {
		
		if (sum(t_periodefonctdispositif_per$per_etat_fonctionnement==1)>0){ 
			graphics::rect(   xleft =graphdate(as.Date(t_periodefonctdispositif_per$per_date_debut[t_periodefonctdispositif_per$per_etat_fonctionnement==1])), 
					ybottom=0.6,
					xright=graphdate(as.Date(t_periodefonctdispositif_per$per_date_fin[t_periodefonctdispositif_per$per_etat_fonctionnement==1])),
					ytop=0.9, 
					col = mypalette[4],
					border = NA, 
					lwd = 1) }
		if (sum(t_periodefonctdispositif_per$per_etat_fonctionnement==0)>0)                           { 
			graphics::rect(   xleft =graphdate(as.Date(t_periodefonctdispositif_per$per_date_debut[t_periodefonctdispositif_per$per_etat_fonctionnement==0])), 
					ybottom=0.6,
					xright=graphdate(as.Date(t_periodefonctdispositif_per$per_date_fin[t_periodefonctdispositif_per$per_etat_fonctionnement==0])),
					ytop=0.9, 
					col = mypalette[6],
					border = NA, 
					lwd = 1) }
		listeperiode<-
				fn_table_per_dis(typeperiode=t_periodefonctdispositif_per$per_tar_code,
						tempsdebut= t_periodefonctdispositif_per$per_date_debut,
						tempsfin=t_periodefonctdispositif_per$per_date_fin,
						libelle=t_periodefonctdispositif_per$libelle)
		nomperiode<-vector()
		
		for (j in 1 : length(listeperiode)){
			nomperiode[j]<-substr(listeperiode[[j]]$nom,1,17)   
			graphics::rect(   xleft=graphdate(listeperiode[[j]]$debut), 
					ybottom=0.1,
					xright=graphdate(listeperiode[[j]]$fin),
					ytop=0.4, 
					col = mypalette1[j],
					border = NA, 
					lwd = 1)        
		}
		graphics::legend  (x= debut,
				y=0.6,
				legend= get("msg",envir_stacomi)$BilanFonctionnementDC.11,
				pch=c(15,15),
				col=c(mypalette[4],mypalette[6]),
				bty="n",
				horiz=TRUE,
				text.width=(fin-debut)/6 ,
				cex=0.8
		)                                               
		graphics::legend  (x= debut,
				y=0.1,
				legend= c(nomperiode),
				pch=c(15,15),
				col=c(mypalette1[1:length(listeperiode)]),
				bty="n",
				horiz=TRUE,
				text.width=(fin-debut)/4,
				cex=0.8
		)
		graphics::text(x=debut,y=0.95, label=get("msg",envir_stacomi)$BilanFonctionnementDC.12,font=4,pos=4) 
		graphics::text(x=debut,y=0.45, label=get("msg",envir_stacomi)$BilanFonctionnementDC.13, font=4,pos=4)
	}
}   



#' FuntableDC create a table output for BilanFonctionnementDC class
#' @param h a handler
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
funtableDC = function(h,...) {
	fonctionnementDC=charge(fonctionnementDC)
	
	if( nrow(fonctionnementDC@requete@query)==0 ) {
		funout(get("msg",envir_stacomi)$BilanFonctionnementDC.2, arret=TRUE)
	}
	
	t_periodefonctdispositif_per<-fonctionnementDC@requete@query # on recupere le data.frame
	t_periodefonctdispositif_per$per_date_debut<-as.character(t_periodefonctdispositif_per$per_date_debut)
	t_periodefonctdispositif_per$per_date_fin<-as.character(t_periodefonctdispositif_per$per_date_fin)
	gdf(t_periodefonctdispositif_per, container=TRUE)
	annee=paste(unique(strftime(as.POSIXlt(t_periodefonctdispositif_per$per_date_debut),"%Y")),collapse="+")
	path1=file.path(path.expand(get("datawd",envir=envir_stacomi)),paste("t_periodefonctdispositif_per_DC_",fonctionnementDC@dc@dc_selectionne,"_",annee,".csv",sep=""),fsep ="\\")
	write.table(t_periodefonctdispositif_per,file=path1,row.names=FALSE,col.names=TRUE,sep=";")
	funout(paste(get("msg",envir_stacomi)$BilanFonctionnementDC.14,path1,"\n"))
	path1html<-file.path(path.expand(get("datawd",envir=envir_stacomi)),paste("t_periodefonctdispositif_per_DC_",fonctionnementDC@dc@dc_selectionne,"_",annee,".html",sep=""),fsep ="\\")
	funout(paste(get("msg",envir_stacomi)$BilanFonctionnementDC.14,path1html,get("msg",envir_stacomi)$BilanFonctionnementDC.15))
	funhtml(t_periodefonctdispositif_per,
			caption=paste("t_periodefonctdispositif_per_DF_",fonctionnementDF@df@df_selectionne,"_",annee,sep=""),
			top=TRUE,
			outfile=path1html,
			clipboard=FALSE,
			append=FALSE,
			digits=2
	)
}
