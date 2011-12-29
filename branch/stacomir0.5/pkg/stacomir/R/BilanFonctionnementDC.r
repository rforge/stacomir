# Nom fichier :        BilanFonctionnementDC.R    (classe)
# Projet :             controle migrateur / suivi passe
# Organisme :          IAV/CSP
# Auteur :             Cedric Briand
# Contact :            cedric.briand@lavilaine.com
# Date de creation :   12/01/2009 14:38:09
# Compatibilite :      
# Etat :               developpement en cours, fonctionne 
# programmer l'affichage du choix de la date  (classe horodate devt en cours) =>  PB à regler dans la relation calendar horodate
# 05/02/2009 21:21:40 OK fonctionne
# programmer les graphiques, et notamment les coupures entre les mois, a partir de la table des dates de debut et de fin
# il faut inserer des lignes correspondant aux debuts et fin de mois afin qu'aucun intervalle ne se trouve a cheval sur deux mois
# 05/02/2009 21:21:54 OK done => il faudra penser à inserer ce type de modif pour le fonctionnement du DF
# Description          Calcul du fonctionnement du DC à partir des parametres
#                      extraits de la base migrateur
# Interface graphique : attribue au bouton DC
# Creer un graphique ("en boites") des resultats de fonctionnement du DC avec deux choix de graphiques  OK fonctionne


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

# Methode pour donner les attributs de la classe RequeteODBCwheredate correspondant à l'objet fonctionnement DC
setMethod("connect",signature=signature("BilanFonctionnementDC"),definition=function(objet,h) {
#  construit une requete ODBCwheredate
			objet@requete@baseODBC<-baseODBC
			objet@requete@select= sql<-paste("SELECT",
					" per_dis_identifiant,",
					" per_date_debut,",
					" per_date_fin,",
					" per_commentaires,",
					" per_etat_fonctionnement,",
					" per_tar_code,",
					" tar_libelle AS libelle",
					" FROM  ",sch,"t_periodefonctdispositif_per per",
					" INNER JOIN ref.tr_typearretdisp_tar tar ON tar.tar_code=per.per_tar_code",sep="")
			objet@requete@colonnedebut<-"per_date_debut"
			objet@requete@colonnefin<-"per_date_fin"
			objet@requete@order_by<-"ORDER BY per_date_debut"
			objet@requete@and<-paste("AND per_dis_identifiant=",objet@dc@dc_selectionne )
#objet@requete@where=#defini dans la methode ODBCwheredate
			objet@requete<-connect(objet@requete) # appel de la methode connect de l'objet ODBCWHEREDATE
			funout(get("msg",envir_stacomi)$BilanFonctionnementDC.1)
			return(objet)
		})

setMethod("charge",signature=signature("BilanFonctionnementDC"),definition=function(objet,h) {
#  construit une requete ODBCwheredate
			# chargement des donnees dans l'environnement de la fonction
			if (exists("refDC",envir_stacomi)) {
				objet@dc<-get("refDC",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.1,arret=TRUE)				}     
			
			if (exists("fonctionnementDC_date_debut",envir_stacomi)) {
				objet@requete@datedebut<-get("fonctionnementDC_date_debut",envir_stacomi)@horodate
			} else {
				funout(get("msg",envir_stacomi)$ref.5,arret=TRUE)	
			}
			
			if (exists("fonctionnementDC_date_fin",envir_stacomi)) {
				objet@requete@datefin<-get("fonctionnementDC_date_fin",envir_stacomi)@horodate
			} else {
				funout(get("msg",envir_stacomi)$ref.6,arret=TRUE)	
			}			
			objet<-connect(objet)			
			return(objet)
		})
# Methode permettant l'affichage d'un graphique en lattice (barchart) du fonctionnement mensuel du dispositif
# Compte tenu de la structure des donnees ce n'est pas si simple... 

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
		lemoissuivant<-seqmois[seqmois>tempsdebut[j]][1] # le premier mois superieur à tempsdebut
		
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
	superpose.polygon<-trellis.par.get("superpose.polygon")
	superpose.polygon$col<-c("#4C00FF","orange")    
	superpose.polygon$border<-FALSE
	trellis.par.set("superpose.polygon",superpose.polygon) 
	bar<-barchart(
			as.numeric(t_periodefonctdispositif_per_mois$sumduree)~as.factor(t_periodefonctdispositif_per_mois$mois)|as.factor(t_periodefonctdispositif_per_mois$annee),
			groups=t_periodefonctdispositif_per_mois$per_tar_code,
			stack=TRUE,
			xlab=get("msg",envir_stacomi)$BilanFonctionnementDC.3,
			ylab=get("msg",envir_stacomi)$BilanFonctionnementDC.4,
			main=list(label=paste(get("msg",envir_stacomi)$BilanFonctionnementDC.5,fonctionnementDC@dc@dc_selectionne), gp=gpar(col="grey", fontsize=8)), 
			auto.key=list(rectangles=TRUE,space="bottom",
					text=c(get("msg",envir_stacomi)$BilanFonctionnementDC.6,get("msg",envir_stacomi)$FonctionnementDC.7)),
			scales= list(x=list(t_periodefonctdispositif_per_mois$mois),
					cex=0.5))    
	print(bar) 
	assign("periodeDC",t_periodefonctdispositif_per_mois,envir_stacomi)
	funout(get("msg",envir_stacomi)$BilanFonctionnementDC.8)	
}   

funboxDC = function(h,...) {  
	fonctionnementDC=charge(fonctionnementDC)
	
	if( nrow(fonctionnementDC@requete@query)==0 ) {
		funout(get("msg",envir_stacomi)$BilanFonctionnementDC.2, arret=TRUE)
	}  
	t_periodefonctdispositif_per<-fonctionnementDC@requete@query # on recupere le data.frame
	duree<-seq.POSIXt(from=fonctionnementDC@requete@datedebut,to=fonctionnementDC@requete@datefin,by="day")
	debut<-unclass(as.Date(duree[1]))[[1]]
	fin<-unclass(as.Date(duree[length(duree)]))[[1]]
	mypalette<-brewer.pal(12,"Paired")
	#display.brewer.all()
	mypalette1<-c("#1B9E77","#AE017E","orange", brewer.pal(12,"Paired"))
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
	axis.Date(1, at=seq(r[1], r[2], by="weeks"),format="%d-%b")
	if (dim(t_periodefonctdispositif_per)[1]==0 ) {    # s'il n'y a pas de periode de fontionnement dans la base
		rect(   xleft=debut, 
				ybottom=0.6,
				xright=fin,
				ytop=0.9, 
				col = mypalette[4],
				border = NA, 
				lwd = 1)                     
		rect(   xleft=debut, 
				ybottom=0.1,
				xright=fin,
				ytop=0.4, 
				col = mypalette[1],
				border = NA, 
				lwd = 1)
		legend(  x= "bottom",
				legend=get("msg",envir_stacomi)$BilanFonctionnementDC.10 ,# three terms in the legend
				pch=c(16,16),
				col=c(mypalette[4],mypalette[6],mypalette[1]),
				#horiz=TRUE,
				ncol=5,
				bty="n")
	} else {
		
		if (sum(t_periodefonctdispositif_per$per_etat_fonctionnement==1)>0){ 
			rect(   xleft =graphdate(as.Date(t_periodefonctdispositif_per$per_date_debut[t_periodefonctdispositif_per$per_etat_fonctionnement==1])), 
					ybottom=0.6,
					xright=graphdate(as.Date(t_periodefonctdispositif_per$per_date_fin[t_periodefonctdispositif_per$per_etat_fonctionnement==1])),
					ytop=0.9, 
					col = mypalette[4],
					border = NA, 
					lwd = 1) }
		if (sum(t_periodefonctdispositif_per$per_etat_fonctionnement==0)>0)                           { 
			rect(   xleft =graphdate(as.Date(t_periodefonctdispositif_per$per_date_debut[t_periodefonctdispositif_per$per_etat_fonctionnement==0])), 
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
			rect(   xleft=graphdate(listeperiode[[j]]$debut), 
					ybottom=0.1,
					xright=graphdate(listeperiode[[j]]$fin),
					ytop=0.4, 
					col = mypalette1[j],
					border = NA, 
					lwd = 1)        
		}
		legend  (x= debut,
				y=0.6,
				legend= get("msg",envir_stacomi)$BilanFonctionnementDC.11,
				pch=c(15,15),
				col=c(mypalette[4],mypalette[6]),
				bty="n",
				horiz=TRUE,
				text.width=(fin-debut)/6 ,
				cex=0.8
		)                                               
		legend  (x= debut,
				y=0.1,
				legend= c(nomperiode),
				pch=c(15,15),
				col=c(mypalette1[1:length(listeperiode)]),
				bty="n",
				horiz=TRUE,
				text.width=(fin-debut)/4,
				cex=0.8
		)
		text(x=debut,y=0.95, label=get("msg",envir_stacomi)$BilanFonctionnementDC.12,font=4,pos=4) 
		text(x=debut,y=0.45, label=get("msg",envir_stacomi)$BilanFonctionnementDC.13, font=4,pos=4)
	}
}   
#
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
	path1=file.path(path.expand(datawd),paste("t_periodefonctdispositif_per_DC_",fonctionnementDC@dc@dc_selectionne,"_",annee,".csv",sep=""),fsep ="\\")
	write.table(t_periodefonctdispositif_per,file=path1,row.names=FALSE,col.names=TRUE,sep=";")
	funout(paste(get("msg",envir_stacomi)$BilanFonctionnementDC.14,path1,"\n"))
	path1html<-file.path(path.expand(datawd),paste("t_periodefonctdispositif_per_DC_",fonctionnementDC@dc@dc_selectionne,"_",annee,".html",sep=""),fsep ="\\")
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
