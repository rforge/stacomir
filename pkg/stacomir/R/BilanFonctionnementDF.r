#' Class "BilanFonctionnementDF" Report fishway work
#' 
#' The DF (Dispositif de Franchissement) is a fishway. It may be automated and
#' work only at certain times This report allows to see the detail of its work.
#' 
#' 
#' @include RefDF.r
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanFonctionnementDF")}.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Class \code{\linkS4class{Bilan_carlot}}
#' \code{\linkS4class{Bilan_poids_moyen}}
#' \code{\linkS4class{Bilan_stades_pigm}} 
#' \code{\linkS4class{Bilan_taille}}
#' \code{\linkS4class{BilanConditionEnv}} 
#' \code{\linkS4class{BilanEspeces}}
#' \code{\linkS4class{BilanFonctionnementDC}}
#' \code{\linkS4class{BilanFonctionnementDF}}
#' \code{\linkS4class{BilanMigration}}
#' \code{\linkS4class{BilanMigrationConditionEnv}}
#' \code{\linkS4class{BilanMigrationInterAnnuelle}}
#' \code{\linkS4class{BilanMigrationPar}}
#' @concept Bilan Object 
#' @export 
setClass(Class="BilanFonctionnementDF",
		representation= representation(data="data.frame",
				df="RefDF",
				horodatedebut="RefHorodate",
				horodatefin="RefHorodate",
				requete="RequeteODBCwheredate"),
		prototype=prototype(data=data.frame(),df=new("RefDF"),
				horodatedebut=new("RefHorodate"),
				horodatefin=new("RefHorodate"),
				requete=new("RequeteODBCwheredate"))
)


#' connect method for BilanFonctionnementDF
#' 
#' @param object An object of class \link{BilanFonctionnementDF-class}
#' loads the working periods and type of arrest or disfunction of the DF
#' @return  An object of class \code{BilanFonctionnementDF}
#' 
#' @author cedric.briand
setMethod("connect",signature=signature("BilanFonctionnementDF"),definition=function(object) {
#  construit une requete ODBCwheredate
			object@requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			object@requete@select= paste("SELECT",
					" per_dis_identifiant,",
					" per_date_debut,",
					" per_date_fin,",
					" per_commentaires,",
					" per_etat_fonctionnement,",
					" per_tar_code,",
					" tar_libelle AS libelle",
					" FROM  ",get("sch",envir=envir_stacomi),"t_periodefonctdispositif_per per",
					" INNER JOIN ref.tr_typearretdisp_tar tar ON tar.tar_code=per.per_tar_code",sep="")
			object@requete@colonnedebut="per_date_debut"
			object@requete@colonnefin="per_date_fin"
			object@requete@order_by="ORDER BY per_date_debut"
			object@requete@and=paste("AND per_dis_identifiant=",object@df@df_selectionne )
#object@requete@where=#defini dans la methode ODBCwheredate
			object@requete<-stacomirtools::connect(object@requete) # appel de la methode connect de l'object ODBCWHEREDATE
			funout(get("msg",envir=envir_stacomi)$BilanFonctionnementDF.1)
			return(object)
		})


#' charge method for BilanFonctionnementDF
#' 
#' 
#' used by the graphical interface to retrieve the objects of Referential classes
#' assigned to envir_stacomi
#' @note Fishways (DF) are of various nature, from very simple eel ladders fed by water discharged from the river,
#' to more complex fishways with levels adjusted by the opening of various gates and regulators. 
#' The objective of this class is to provide an assessment of the working status of a fishway throughout the year.
#' A number of fishes ascending a fishway has meaning only if we know that the fishway is operational, and that the counting 
#' orerated on the fishway has remained operational.
#' In the database the operation of the fishway (DF) and counting device (DC) is agregated in one table (t_periodefonctdispositif_per).
#' The column  per_etat_fonctionnement indicates whether the fishway is operational (with a boolean) and the column per_tar_code indicates
#' the status of either the fishway or DC. 
#' @param object An object of class \link{BilanFonctionnementDF-class}
#' @return  An object of class \link{BilanFonctionnementDF-class}
#' 
#' @author cedric.briand
setMethod("charge",signature=signature("BilanFonctionnementDF"),definition=function(object) {
#  construit une requete ODBCwheredate
			# chargement des donnees dans l'environnement de la fonction
			if (exists("refDF",envir=envir_stacomi)) {
				object@df<-get("refDF",envir=envir_stacomi)
			} else {
				funout(get("msg",envir=envir_stacomi)$ref.12,arret=TRUE)
			}     
			
			if (exists("fonctionnementDF_date_debut",envir=envir_stacomi)) {
				object@horodatedebut<-get("fonctionnementDF_date_debut",envir=envir_stacomi)@horodate
			} else {
				funout(get("msg",envir=envir_stacomi)$ref.5,arret=TRUE)
			}
			
			if (exists("fonctionnementDF_date_fin",envir=envir_stacomi)) {
				object@horodatefin<-get("fonctionnementDF_date_fin",envir=envir_stacomi)@horodate
			} else {
				funout(get("msg",envir=envir_stacomi)$ref.6,arret=TRUE)
			}			
			object<-connect(object)
			
			return(object)
		})

#' command line interface for BilanFonctionnementDF class
#' 
#' The choice_c method fills in the data slot for RefDC, and then 
#' uses the choice_c methods of these object to "select" the data.
#' @param object An object of class \link{RefDC-class}
#' @param df The df to set
#' @param horodatedebut A POSIXt or Date or character to fix the date of beginning of the Bilan
#' @param horodatefin A POSIXt or Date or character to fix the last date of the Bilan
#' @return An object of class \link{RefDC-class} with slots filled
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("choice_c",signature=signature("BilanFonctionnementDF"),definition=function(object,df,horodatedebut,horodatefin,...){
			# fonctionnementDF<-BfDF
			fonctionnementDF<-object
			assign("fonctionnementDF",fonctionnementDF,envir=envir_stacomi)    
			funout(get("msg",envir=envir_stacomi)$interface_BilanFonctionnementDC.1)
			fonctionnementDF@df<-charge(fonctionnementDF@df)    
			fonctionnementDF@df<-choice_c(fonctionnementDF@df,df)
			# assigns the parameter (horodatedebut) of the method to the object using choice_c method for RefDF
			fonctionnementDF@horodatedebut<-choice_c(fonctionnementDF@horodatedebut,
					nomassign="fonctionnementDF_date_debut",
					funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.5,
					decal=-2,
					horodate=horodatedebut)
			fonctionnementDF@horodatefin<-choice_c(fonctionnementDF@horodate,
					nomassign="fonctionnementDF_date_fin",
					funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.6,
					decal=-1,
					horodate=horodatefin)
			return(fonctionnementDF)
		})



#' funbarchartDF creates a barchart for BilanFonctionnementDF class
#' 
#' @note The program cuts periods which overlap between two month
#' @param h handler
#' @param ... additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
funbarchartDF = function(h,...) {
	# TEMP 2015
	fonctionnementDF=charge(fonctionnementDF)
	
	if( nrow(fonctionnementDF@requete@query)==0 ) {
		funout(get("msg",envir=envir_stacomi)$BilanFonctionnementDF.2, arret=TRUE)
	}
	
	funout(get("msg",envir=envir_stacomi)$BilanFonctionnementDF.3)
	t_periodefonctdispositif_per=fonctionnementDF@requete@query # on recupere le data.frame   
	# l'objectif du programme ci dessous est de calculer la time.sequence mensuelle de fonctionnement du dispositif.
	#tempsdebut<-strptime(t_periodefonctdispositif_per$per_date_debut,"%Y-%m-%d %H:%M:%S", tz = "GMT")
	#tempsfin<-strptime(t_periodefonctdispositif_per$per_date_fin,"%Y-%m-%d %H:%M:%S", tz = "GMT")
	tempsdebut<-t_periodefonctdispositif_per$per_date_debut
	tempsfin<-t_periodefonctdispositif_per$per_date_fin
	# test la premiere horodate peut etre avant le choice de temps de debut, remplacer cette date par requete@datedebut
	tempsdebut[tempsdebut<fonctionnementDF@requete@datedebut]<-fonctionnementDF@requete@datedebut
	# id pour fin
	tempsfin[tempsfin>fonctionnementDF@requete@datefin]<-fonctionnementDF@requete@datefin
	t_periodefonctdispositif_per=cbind(t_periodefonctdispositif_per,tempsdebut,tempsfin)
	# BUG 06/02/2009 11:51:49 si la date choisie n'est pas le debut du mois
	seqmois=seq(from=tempsdebut[1],to=tempsfin[nrow(t_periodefonctdispositif_per)],by="month",tz = "GMT")
	seqmois=as.POSIXlt(round(seqmois,digits="months"))
	#seqmois<-c(seqmois,seqmois[length(seqmois)]+months(1))
	t_periodefonctdispositif_per_mois=t_periodefonctdispositif_per[1,]
	############################
	#progress bar
	###########################
	mygtkProgressBar(
			title=get("msg",envir=envir_stacomi)$BilanFonctionnementDF.4,
			progress_text=get("msg",envir=envir_stacomi)$BilanFonctionnementDF.5)
	# this function assigns
	z=0 # compteur tableau t_periodefonctdispositif_per_mois
	for(j in 1:nrow(t_periodefonctdispositif_per)){
		#cat( j 
		progress_bar$setFraction(j/nrow(t_periodefonctdispositif_per)) 
		progress_bar$setText(sprintf("%d%% progression",round(100*j/nrow(t_periodefonctdispositif_per))))
		RGtk2::gtkMainIterationDo(FALSE)
		if (j>1) t_periodefonctdispositif_per_mois=rbind(t_periodefonctdispositif_per_mois, t_periodefonctdispositif_per[j,])
		lemoissuivant=seqmois[seqmois>tempsdebut[j]][1] # le premier mois superieur a tempsdebut
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
	t_periodefonctdispositif_per_mois$sumtime.sequence<-as.numeric(difftime(t_periodefonctdispositif_per_mois$tempsfin, t_periodefonctdispositif_per_mois$tempsdebut,units = "hours"))
	t_periodefonctdispositif_per_mois$mois1= strftime(as.POSIXlt(t_periodefonctdispositif_per_mois$tempsdebut),"%b")
	t_periodefonctdispositif_per_mois$mois=strftime(as.POSIXlt(t_periodefonctdispositif_per_mois$tempsdebut),"%m")
	t_periodefonctdispositif_per_mois$annee=strftime(as.POSIXlt(t_periodefonctdispositif_per_mois$tempsdebut),"%Y")
	progress_bar$setText("All done.")
	
	
# graphique 
	t_periodefonctdispositif_per_mois<-stacomirtools::chnames(t_periodefonctdispositif_per_mois,  old_variable_name=c("sumtime.sequence","per_tar_code","per_etat_fonctionnement"),
			new_variable_name=get("msg",envir_stacomi)$BilanFonctionnementDF.6)
#modif de l'ordre pour apparence graphique
	
	t_periodefonctdispositif_per_mois=t_periodefonctdispositif_per_mois[order(t_periodefonctdispositif_per_mois$type_fonct., decreasing = TRUE),]
	g<- ggplot(t_periodefonctdispositif_per_mois,
			aes(x=mois,y=time.sequence,fill=libelle))+
	facet_grid(annee~.)+ggtitle(paste(get("msg",envir_stacomi)$BilanFonctionnementDF.7,fonctionnementDF@df@df_selectionne))
	g<-g+geom_bar(stat='identity')+
			scale_fill_manual(values = c("#E41A1C","#E6AB02", "#9E0142","#1B9E77","#999999"))
	#modif de l'ordre pour apparence graphique
	t_periodefonctdispositif_per_mois=t_periodefonctdispositif_per_mois[order(t_periodefonctdispositif_per_mois$fonctionnement),]
	t_periodefonctdispositif_per_mois$fonctionnement=as.factor(	t_periodefonctdispositif_per_mois$fonctionnement)
	g1<- ggplot(t_periodefonctdispositif_per_mois,aes(x=mois,y=time.sequence))+facet_grid(annee~.)+ggtitle(paste(get("msg",envir_stacomi)$BilanFonctionnementDF.7,fonctionnementDF@df@df_selectionne))
	g1<-g1+
			geom_bar(stat='identity',aes(fill=fonctionnement))+
			scale_fill_manual(values = c("#E41A1C","#4DAF4A")) 
	
	if(length(unique(t_periodefonctdispositif_per_mois$annee))>1)  {
		grDevices::X11(40,40); print(g)
		grDevices::X11 (40,40) ;print(g1)
	}else    {
		X11(60,40)  
		vplayout <- function(x, y) { grid::viewport(layout.pos.row = x, layout.pos.col = y)   }
		grid::grid.newpage()
		grid::pushViewport(grid::viewport(layout = grid::grid.layout(1,2,just="center")))
		print(g, vp=vplayout(1,1))
		print(g1, vp=vplayout(1,2))
	}
	assign("periodeDF",t_periodefonctdispositif_per_mois,envir_stacomi)
	funout(get("msg",envir=envir_stacomi)$BilanFonctionnementDF.8)
	dispose(progres)
}   

#' FunboxDF draws rectangles to describe the DF work for BilanFonctionnementDF class
#' @param h handler
#' @param ... additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
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
	time.sequence=seq.POSIXt(from=fonctionnementDF@requete@datedebut,to=fonctionnementDF@requete@datefin,by="day")
	debut=graphdate(time.sequence[1])
	fin=graphdate(time.sequence[length(time.sequence)])
	mypalette<-RColorBrewer::brewer.pal(12,"Paired")
	#display.brewer.all()
	mypalette1<-c("#1B9E77","#AE017E","orange", RColorBrewer::brewer.pal(12,"Paired"))
	
	###################################         
	# creation d'un graphique vide (2)
	###################################
	plot(   graphdate(time.sequence),
			seq(0,1,length.out=length(time.sequence)),
			xlim=c(debut,fin), 
			type= "n", 
			xlab="",
			xaxt="n",
			yaxt="n", 
			ylab=get("msg",envir=envir_stacomi)$BilanFonctionnementDF.9,
			#bty="n",
			cex=0.8)
	r <- round(range(time.sequence), "day")
	graphics::axis(1, at=graphdate(seq(r[1], r[2], by="weeks")),labels=strftime(as.POSIXlt(seq(r[1], r[2], by="weeks")),format="%d-%b"))
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
#' FuntableDF create a table output for BilanFonctionnementDF class
#' @param h handler
#' @param ... additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
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
	path1=file.path(path.expand(get("datawd",envir=envir_stacomi)),paste("t_periodefonctdispositif_per_DF_",fonctionnementDF@df@df_selectionne,"_",annee,".csv",sep=""),fsep ="\\")
	write.table(t_periodefonctdispositif_per,file=path1,row.names=FALSE,col.names=TRUE,sep=";")
	funout(paste(get("msg",envir=envir_stacomi)$FonctionnementDC.14,path1,"\n"))
	path1html=file.path(path.expand(get("datawd",envir=envir_stacomi)),paste("t_periodefonctdispositif_per_DF_",fonctionnementDF@df@df_selectionne,"_",annee,".html",sep=""),fsep ="\\")
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
