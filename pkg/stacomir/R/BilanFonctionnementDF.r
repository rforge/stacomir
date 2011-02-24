# Nom fichier :        BilanFonctionnementDF.R    (classe)
# Projet :             controle migrateur / suivi passe
# Organisme :          IAV/CSP
# Auteur :             Cedric Briand
# Contact :            cedric.briand@lavilaine.comF
# Date de creation :   07/02/2009 21:30:54
# Compatibilite :      
# Etat :               developpement en cours

#**********************************************************************
#*
#* Modifications :
#* ---------------
#* JJ-MM-AAAA #No Prenom NOM [INITIALES] :
#*    explication de la modification
#*
#********************************************
###########################################
# Definiton de la classe et des methodes
############################################
#' Class "BilanFonctionnementDF" Bilan du fonctionnement du dispositif de franchissement
#' The DF (Dispositif de Franchissement) is a fishway. It may be automated and work only at certain times
#' This Bilan allows to see the detail of its work.
#' @slot data = Object of class \code{"data.frame"}
#' @slot df = Object of class \code{"RefDF"}
#' @slot horodate = Object of class \code{"RefHorodate"}
#' @slot requete = Object of class \code{"RequeteODBCwheredate"}
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @seealso     Other Bilan Class
#'	\code{\linkS4class{Bilan_lot}}
#'	\code{\linkS4class{Bilan_poids_moyen}}
#'	\code{\linkS4class{Bilan_stades_pigm}}	
#'	\code{\linkS4class{Bilan_taille}}
#'	\code{\linkS4class{BilanConditionEnv}}
#'	\code{\linkS4class{BilanEspeces}}
#'	\code{\linkS4class{BilanFonctionnementDC}}
#'	\code{\linkS4class{BilanFonctionnementDF}}	
#'	\code{\linkS4class{BilanMigration}}	
#'	\code{\linkS4class{BilanMigrationConditionEnv}}
#'	\code{\linkS4class{BilanMigrationInterAnnuelle}}
#'	\code{\linkS4class{BilanMigrationPar}}	
#' @references \url{http://62.160.92.241:8066/tracstacomi/wiki/StacomiR} 
setClass(Class="BilanFonctionnementDF",
		representation= representation(data="data.frame",
				df="RefDF",
				horodate="RefHorodate",
				requete="RequeteODBCwheredate"),
		prototype=prototype(data=data.frame(),df=new("RefDF"),
				horodate=new("RefHorodate"),
				requete=new("RequeteODBCwheredate"))
)

# Methode pour donner les attributs de la classe RequeteODBCwheredate correspondant à l'objet fonctionnement DC
setMethod("connect",signature=signature("BilanFonctionnementDF"),definition=function(objet,h) {
#  construit une requete ODBCwheredate
			objet@requete@baseODBC=baseODBC
			objet@requete@select= paste("SELECT",
					" per_dis_identifiant,",
					" per_date_debut,",
					" per_date_fin,",
					" per_commentaires,",
					" per_etat_fonctionnement,",
					" per_tar_code,",
					" tar_libelle AS libelle",
					" FROM  ",sch,"t_periodefonctdispositif_per per",
					" INNER JOIN ref.tr_typearretdisp_tar tar ON tar.tar_code=per.per_tar_code",sep="")
			objet@requete@colonnedebut="per_date_debut"
			objet@requete@colonnefin="per_date_fin"
			objet@requete@order_by="ORDER BY per_date_debut"
			objet@requete@and=paste("AND per_dis_identifiant=",objet@df@df_selectionne )
#objet@requete@where=#defini dans la methode ODBCwheredate
			objet@requete<-connect(objet@requete) # appel de la methode connect de l'objet ODBCWHEREDATE
			funout(get("msg",envir=envir_stacomi)$BilanFonctionnementDF.1)
			return(objet)
		})

#    objet = new("BilanFonctionnementDF")
setMethod("charge",signature=signature("BilanFonctionnementDF"),definition=function(objet,h) {
#  construit une requete ODBCwheredate
			# chargement des donnees dans l'environnement de la fonction
			if (exists("refDF",envir=envir_stacomi)) {
				objet@df<-get("refDF",envir=envir_stacomi)
			} else {
				funout(get("msg",envir=envir_stacomi)$ref.12,arret=TRUE)
			}     
			
			if (exists("fonctionnementDF_date_debut",envir=envir_stacomi)) {
				objet@requete@datedebut<-get("fonctionnementDF_date_debut",envir=envir_stacomi)@horodate
			} else {
				funout(get("msg",envir=envir_stacomi)$ref.5,arret=TRUE)
			}
			
			if (exists("fonctionnementDF_date_fin",envir=envir_stacomi)) {
				objet@requete@datefin<-get("fonctionnementDF_date_fin",envir=envir_stacomi)@horodate
			} else {
				funout(get("msg",envir=envir_stacomi)$ref.6,arret=TRUE)
			}			
			objet<-connect(objet)
			
			return(objet)
		})
# Methode permettant l'affichage d'un graphique en lattice (barchart) du fonctionnement mensuel du dispositif
# Compte tenu de la structure des donnees ce n'est pas si simple...
#' funbarchartDF creates a barchart for BilanFonctionnementDF class
#' @param choix a handler
#' @param ... 
#' @returnType 
#' @return 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
funbarchartDF = function(choix,...) {
	fonctionnementDF=charge(fonctionnementDF)
	
	if( nrow(fonctionnementDF@requete@query)==0 ) {
	   funout(get("msg",envir=envir_stacomi)$BilanFonctionnementDF.2, arret=TRUE)
  }
	
	funout(get("msg",envir=envir_stacomi)$BilanFonctionnementDF.3)
	t_periodefonctdispositif_per=fonctionnementDF@requete@query # on recupere le data.frame   
	# l'objectif du programme ci dessous est de calculer la duree mensuelle de fonctionnement du dispositif.
	tempsdebut<-strptime(t_periodefonctdispositif_per$per_date_debut,"%Y-%m-%d %H:%M:%S", tz = "GMT")
	tempsfin<-strptime(t_periodefonctdispositif_per$per_date_fin,"%Y-%m-%d %H:%M:%S", tz = "GMT")
	# test la premiere horodate peut etre avant le choix de temps de debut, remplacer cette date par requete@datedebut
	tempsdebut[tempsdebut<fonctionnementDF@requete@datedebut]<-fonctionnementDF@requete@datedebut
	# id pour fin
	tempsfin[tempsfin>fonctionnementDF@requete@datefin]<-fonctionnementDF@requete@datefin
	t_periodefonctdispositif_per=cbind(t_periodefonctdispositif_per,tempsdebut,tempsfin)
	# BUG 06/02/2009 11:51:49 si la date choisie n'est pas le debut du mois
	seqmois=seq(from=tempsdebut[1],to=round.POSIXt(tempsfin[nrow(t_periodefonctdispositif_per)],"month"),by="month",tz = "GMT")
	seqmois=as.POSIXlt(round(seqmois,digits="months"))
	t_periodefonctdispositif_per_mois=t_periodefonctdispositif_per[1,]
		progres<-winProgressBar(title =get("msg",envir=envir_stacomi)$BilanFonctionnementDF.4,
			label = get("msg",envir=envir_stacomi)$BilanFonctionnementDF.5,
			min = 0,
			max = 1, 
			initial = 0,
			width = 400)
     	z=0 # compteur tableau t_periodefonctdispositif_per_mois
	for(j in 1:nrow(t_periodefonctdispositif_per)){
		#cat( j )
		setWinProgressBar(progres,j/nrow(t_periodefonctdispositif_per),
							title=get("msg",envir=envir_stacomi)$BilanFonctionnementDF.4,
							label=sprintf("%d%% progression",round(100*j/nrow(t_periodefonctdispositif_per)))) 
		if (j>1) t_periodefonctdispositif_per_mois=rbind(t_periodefonctdispositif_per_mois, t_periodefonctdispositif_per[j,])
		lemoissuivant=seqmois[seqmois>tempsdebut[j]][1] # le premier mois superieur à tempsdebut
		while (tempsfin[j]>lemoissuivant){    # on est a cheval sur deux periodes    
			
			#if (z>0) stop("erreur")
			z=z+1
			t_periodefonctdispositif_per_mois=rbind(t_periodefonctdispositif_per_mois, t_periodefonctdispositif_per[j,])
			t_periodefonctdispositif_per_mois[j+z,"tempsdebut"]=as.POSIXct(lemoissuivant)
			t_periodefonctdispositif_per_mois[j+z-1,"tempsfin"]=as.POSIXct(lemoissuivant)
			lemoissuivant=seqmois[match(as.character(lemoissuivant),as.character(seqmois))+1] # on decale de 1 mois avant de rerentrer dans la boucle
			if (is.na(lemoissuivant) ) break
		}  
	}
	t_periodefonctdispositif_per_mois$sumduree<-as.numeric(difftime(t_periodefonctdispositif_per_mois$tempsfin, t_periodefonctdispositif_per_mois$tempsdebut,units = "hours"))
	t_periodefonctdispositif_per_mois$mois1= strftime(as.POSIXlt(t_periodefonctdispositif_per_mois$tempsdebut),"%b")
	t_periodefonctdispositif_per_mois$mois=strftime(as.POSIXlt(t_periodefonctdispositif_per_mois$tempsdebut),"%m")
	t_periodefonctdispositif_per_mois$annee=strftime(as.POSIXlt(t_periodefonctdispositif_per_mois$tempsdebut),"%Y")

 close(progres)
     
# graphique 
  t_periodefonctdispositif_per_mois<-chnames(t_periodefonctdispositif_per_mois,  old_variable_name=c("sumduree","per_tar_code","per_etat_fonctionnement"),
		  	new_variable_name=get("msg",envir_stacomi)$BilanFonctionnementDF.6)
#modif de l'ordre pour apparence graphique
  
	t_periodefonctdispositif_per_mois=t_periodefonctdispositif_per_mois[order(t_periodefonctdispositif_per_mois$type_fonct., decreasing = TRUE),]
	g<- ggplot(t_periodefonctdispositif_per_mois,aes(x=mois,y=duree,fill=libelle))+facet_grid(annee~.)+opts(title=paste(get("msg",envir_stacomi)$BilanFonctionnementDF.7,fonctionnementDF@df@df_selectionne))
	g<-g+geom_bar(stat='identity')+scale_fill_manual(values = c("#E41A1C","#E6AB02", "#9E0142","#1B9E77","#999999"))
	#modif de l'ordre pour apparence graphique
	t_periodefonctdispositif_per_mois=t_periodefonctdispositif_per_mois[order(t_periodefonctdispositif_per_mois$fonctionnement),]
	t_periodefonctdispositif_per_mois$fonctionnement=as.factor(	t_periodefonctdispositif_per_mois$fonctionnement)
	g1<- ggplot(t_periodefonctdispositif_per_mois,aes(x=mois,y=duree))+facet_grid(annee~.)+opts(title=paste(get("msg",envir_stacomi)$BilanFonctionnementDF.7,fonctionnementDF@df@df_selectionne))
	g1<-g1+geom_bar(stat='identity',aes(fill=fonctionnement))+scale_fill_manual(values = c("#E41A1C","#4DAF4A")) 
   
   if(length(unique(t_periodefonctdispositif_per_mois$annee))>1)  {
    x11(40,40); print(g)
    x11 (40,40) ;print(g1)
   }else    {
   x11(60,40)  
   vplayout <- function(x, y) { viewport(layout.pos.row = x, layout.pos.col = y)   }
   grid.newpage()
   pushViewport(viewport(layout = grid.layout(1,2,just="center")))
   print(g, vp=vplayout(1,1))
   print(g1, vp=vplayout(1,2))
   }
	assign("periodeDF",t_periodefonctdispositif_per_mois,envir_stacomi)
	funout(get("msg",envir=envir_stacomi)$BilanFonctionnementDF.8)
}   

#' FunboxDF draws rectangles to describe the DF work for BilanFonctionnementDF class
#' @param h 
#' @param ... 
#' @returnType 
#' @return 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
funboxDF = function(h,...) {
	
	fonctionnementDF=charge(fonctionnementDF)
	
	if( nrow(fonctionnementDF@requete@query)==0 ) {
	   funout(get("msg",envir=envir_stacomi)$BilanFonctionnementDF.2, arret=TRUE)
  }
  
	t_periodefonctdispositif_per=fonctionnementDF@requete@query # on recupere le data.frame
	graphdate<-function(vectordate){
		vectordate<-as.POSIXct(vectordate)
		attributes(vectordate)<-NULL
		unclass(vectordate)
		return(vectordate)
	}
	duree=seq.POSIXt(from=fonctionnementDF@requete@datedebut,to=fonctionnementDF@requete@datefin,by="day")
	debut=graphdate(duree[1])
	fin=graphdate(duree[length(duree)])
	mypalette<-brewer.pal(12,"Paired")
	#display.brewer.all()
	mypalette1<-c("#1B9E77","#AE017E","orange", brewer.pal(12,"Paired"))
	
	###################################         
	# creation d'un graphique vide (2)
	###################################
	plot(   graphdate(duree),
			seq(0,1,length.out=length(duree)),
			xlim=c(debut,fin), 
			type= "n", 
			xlab="",
			xaxt="n",
			yaxt="n", 
			ylab=get("msg",envir=envir_stacomi)$BilanFonctionnementDF.9,
			#bty="n",
			cex=0.8)
	r <- round(range(duree), "day")
	axis(1, at=graphdate(seq(r[1], r[2], by="weeks")),label=strftime(as.POSIXlt(seq(r[1], r[2], by="weeks")),format="%d-%b"))
	if (dim(t_periodefonctdispositif_per)[1]==0 ) {
		rect(      xleft=debut, 
				ybottom=0.6,
				xright=fin,
				ytop=0.9, 
				col = mypalette[4],
				border = NA, 
				lwd = 1)                     
		rect(      xleft=debut, 
				ybottom=0.1,
				xright=fin,
				ytop=0.4, 
				col = mypalette[1],
				border = NA, 
				lwd = 1)
		legend(  x= "bottom",
				legend= get("msg",envir=envir_stacomi)$BilanFonctionnementDC.10,
				pch=c(16,16),
				col=c(mypalette[4],mypalette[6],mypalette[1]),
				#horiz=TRUE,
				ncol=5,
				bty="n")
	} else {
		
		if (sum(t_periodefonctdispositif_per$per_etat_fonctionnement==1)>0){ 
			rect(   xleft =graphdate(t_periodefonctdispositif_per$per_date_debut[t_periodefonctdispositif_per$per_etat_fonctionnement==1]), 
					ybottom=0.6,
					xright=graphdate(t_periodefonctdispositif_per$per_date_fin[t_periodefonctdispositif_per$per_etat_fonctionnement==1]),
					ytop=0.9, 
					col = mypalette[4],
					border = NA, 
					lwd = 1) }
		if (sum(t_periodefonctdispositif_per$per_etat_fonctionnement==0)>0)                           { 
			rect(   xleft =graphdate(t_periodefonctdispositif_per$per_date_debut[t_periodefonctdispositif_per$per_etat_fonctionnement==0]), 
					ybottom=0.6,
					xright=graphdate(t_periodefonctdispositif_per$per_date_fin[t_periodefonctdispositif_per$per_etat_fonctionnement==0]),
					ytop=0.9, 
					col = mypalette[6],
					border = NA, 
					lwd = 1) }
		listeperiode<-
				fn_table_per_dis(typeperiode=t_periodefonctdispositif_per$per_tar_code,
						tempsdebut= t_periodefonctdispositif_per$per_date_debut,
						tempsfin=t_periodefonctdispositif_per$per_date_fin,
						libelle=t_periodefonctdispositif_per$libelle,
						date=FALSE)
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
				legend= get("msg",envir=envir_stacomi)$BilanFonctionnementDC.11,
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
				text.width=(fin-debut)/8,
				cex=0.7
				)
		text(x=debut,y=0.95, label=get("msg",envir=envir_stacomi)$BilanFonctionnementDF.7,font=4,pos=4) 
		text(x=debut,y=0.45, label=get("msg",envir=envir_stacomi)$BilanFonctionnementDF.10, font=4,pos=4)
	}
}   
#
funtableDF = function(h,...) {
	fonctionnementDF=charge(fonctionnementDF)
	
		if( nrow(fonctionnementDF@requete@query)==0 ) {
	   funout(get("msg",envir=envir_stacomi)$BilanFonctionnementDF.2, arret=TRUE)
  }
  
	t_periodefonctdispositif_per=fonctionnementDF@requete@query # on recupere le data.frame
	t_periodefonctdispositif_per$per_date_debut=as.character(t_periodefonctdispositif_per$per_date_debut)
	t_periodefonctdispositif_per$per_date_fin=as.character(t_periodefonctdispositif_per$per_date_fin)
	gdf(t_periodefonctdispositif_per, container=TRUE)
	annee=paste(unique(strftime(as.POSIXlt(t_periodefonctdispositif_per$per_date_debut),"%Y")),collapse="+")
  path1=file.path(path.expand(datawd),paste("t_periodefonctdispositif_per_DF_",fonctionnementDF@df@df_selectionne,"_",annee,".csv",sep=""),fsep ="\\")
	write.table(t_periodefonctdispositif_per,file=path1,row.names=FALSE,col.names=TRUE,sep=";")
	funout(paste(get("msg",envir=envir_stacomi)$FonctionnementDC.14,path1,"\n"))
	path1html=file.path(path.expand(datawd),paste("t_periodefonctdispositif_per_DF_",fonctionnementDF@df@df_selectionne,"_",annee,".html",sep=""),fsep ="\\")
   	funout(paste(get("msg",envir=envir_stacomi)$FonctionnementDC.14,path1html,get("msg",envir=envir_stacomi)$BilanFonctionnementDF.15))
   funhtml(t_periodefonctdispositif_per,
            caption=paste("t_periodefonctdispositif_per_DF_",fonctionnementDF@df@df_selectionne,"_",annee,sep=""),
            top=TRUE,
            outfile=path1html,
            clipboard=FALSE,
            append=FALSE,
            digits=2
            )

}
