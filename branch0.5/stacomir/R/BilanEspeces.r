# Nom fichier :        BilanEspeces    (classe)
# Projet :             controle migrateur calmig/prog/classe
# Date de creation :   31/03/2008 17:21:18

#' Class "BilanEspeces" Report of the species present at a counting device for
#' a given period
#' 
#' this class is used to make the assessment of all species, and their number,
#' per month it writes either an histogram or a pie chart of number per
#' year/week/month
#' 
#' 
#' @include RefDC.r
#' @include RefListe.r
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanEspeces", ...)}.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Classes \code{\linkS4class{Bilan_lot}}
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
#' showClass("BilanEspeces")
#' 
#' @export 
setClass(Class="BilanEspeces",
		representation=
				representation(dc="RefDC",
						horodate="RefHorodate",
						datedebut="POSIXlt",
						datefin="POSIXlt",
						data="data.frame",
						liste="RefListe"),
		prototype=prototype(dc=new("RefDC"),
				horodate=new("RefHorodate"),
				data=data.frame(),
				liste=new("RefListe")
		)
)

setValidity("BilanEspeces",function(object)
		{
			rep1=length(object@dc)==1
			return(ifelse(rep1, TRUE ,c(1:6)[!c(rep1)]))
		}   
)

#' connect method for BilanEspeces
#' @return bilanEspeces instance with request corresponding to the user choices
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("connect",signature=signature("BilanEspeces"),definition=function(object) {
			bilanEspeces<-object # pour faciliter la debug, l'argument formel de la classe doit etre forcement object !
			requete=new("RequeteODBCwheredate")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@datedebut=bilanEspeces@datedebut
			requete@datefin=bilanEspeces@datefin
			requete@colonnedebut="ope_date_debut"
			requete@colonnefin="ope_date_fin"
			requete@select= paste("SELECT lot_identifiant, ope_date_debut, ope_date_fin,",
					" lot_effectif, lot_tax_code, lot_std_code, tax_nom_latin, std_libelle,",
					" date_part('year', ope_date_debut) as annee,",
					" date_part('month',ope_date_debut) as mois,",
					" date_part('week',ope_date_debut) as semaine",
					" FROM ",get("sch",envir=envir_stacomi),"t_operation_ope",
					" INNER JOIN ",get("sch",envir=envir_stacomi),"t_lot_lot ON ope_identifiant=lot_ope_identifiant",
					" INNER JOIN ref.tr_taxon_tax on tax_code=lot_tax_code",
					" INNER JOIN ref.tr_stadedeveloppement_std on std_code=lot_std_code",
					sep="")
			requete@and=paste(" AND ope_dic_identifiant=",
					bilanEspeces@dc@dc_selectionne,
					" AND lot_lot_identifiant IS NULL",
					" AND lot_effectif IS NOT NULL",
					sep="")
			requete<-connect(requete)	
			if (requete@etat!="Requete reussie \n") funout(get("msg",envir=envir_stacomi)$BilanEspeces.3,arret=TRUE)
			bilanEspeces@data<-requete@query					
			return(bilanEspeces)
		})

#' handler du calcul du BilanEspeces
#' realise le calcul du bilan especes, l'ecrit dans l'environnement envir_stacomi
#' traite eventuellement les quantites de lots (si c'est des civelles)
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
hBilanEspecescalc=function(h,...){
	charge(h$action)
}


#' charge method for BilanEspeces
#' verifies the content of objects and calls the connect method
#' @return BilanEspeces with slots filled by user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
#' @examples bilanEspeces=new("BilanEspeces")
setMethod("charge",signature=signature("BilanEspeces"),definition=function(object,...){
			funout(get("msg",envir_stacomi)$BilanEspeces.7)
			bilanEspeces<-object
			if (exists("refDC",envir_stacomi)) {
				bilanEspeces@dc<-get("refDC",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.1,arret=TRUE)	
			}
			# rem pas tres satisfaisant car ce nom est choisi dans l'interface
			if (exists("bilanEspeces_date_debut",envir_stacomi)) {
				bilanEspeces@datedebut<-get("bilanEspeces_date_debut",envir_stacomi)@horodate
			} else {
				funout(get("msg",envir=envir_stacomi)$ref.5,arret=TRUE)
			}
			# rem id
			if (exists("bilanEspeces_date_fin",envir_stacomi)) {
				bilanEspeces@datefin<-get("bilanEspeces_date_fin",envir_stacomi)@horodate
			} else {
				funout(get("msg",envir=envir_stacomi)$ref.6,arret=TRUE)
			} 
			if (exists("refliste",envir_stacomi)) {      
				bilanEspeces@liste<-get("refliste",envir_stacomi)      
			} else {      
				funout(get("msg",envir_stacomi)$ref.9, arret=TRUE)             
			} 
			bilanEspeces<-connect(bilanEspeces)
			
			assign("bilanEspeces",bilanEspeces,envir_stacomi)
			funout(get("msg",envir_stacomi)$BilanEspeces.1)
		})


#' handler du calcul hCamembert
#' trace un camembert des especes ou un camembert par periode...
#' @note pas besoin de refaire tourner calcul si une autre liste � �t� charg�e, les effectifs <0 sont transform�s en positifs
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export

hCamembert = function(h,...) {	
	if (exists("bilanEspeces",envir_stacomi)) {
		bilanEspeces<-get("bilanEspeces",envir_stacomi)
	} else {      
		funout(get("msg",envir_stacomi)$BilanEspeces.4,arret=TRUE)
	}
	DC=as.numeric(bilanEspeces@dc@dc_selectionne)	
	# update of refliste which does not need calcul button pushed
	bilanEspeces@liste<-get("refliste",envir_stacomi)   
	
	tableEspeces=bilanEspeces@data
	if (nrow(tableEspeces)==0) funout(get("msg",envir_stacomi)$BilanEspeces.5,arret=TRUE)
	tableEspeces$taxon_stades=paste(tableEspeces$tax_nom_latin,tableEspeces$std_libelle,sep="_")
	# je ne garde taxons_stades que pour les esp�ces pr�sentant plusieurs stades
	nbstades=tapply(tableEspeces$tax_nom_latin,tableEspeces$taxon_stades,function(X)(length(unique(X))))
	if (length(nbstades[nbstades>1])>0){
		les_multiples=names(nbstades[nbstades>1])
		tableEspeces[!tableEspeces$taxon_stades%in%les_multiples,"taxon_stades"]<-tableEspeces$tax_nom_latin[!tableEspeces$taxon_stades%in%les_multiples]
	} else tableEspeces$taxon_stades<-tableEspeces$tax_nom_latin
	# TODO ajouter les effectifs en fin de taxons_stades ???
	nb=length(unique(tableEspeces$taxon_stade))
	if (min(tableEspeces$lot_effectif)<0) {funout(get("msg",envir_stacomi)$BilanEspeces.6)
		tableEspeces$lot_effectif=abs(tableEspeces$lot_effectif)
	}
	sumEspeces=switch(bilanEspeces@liste@listechoix,
			"annee"=as.data.frame(xtabs(lot_effectif~taxon_stades+annee,data=tableEspeces)),
			"mois"=as.data.frame(xtabs(lot_effectif~taxon_stades+mois,data=tableEspeces)),
			"semaine"=as.data.frame(xtabs(lot_effectif~taxon_stades+semaine,data=tableEspeces)),
			"aucun"=as.data.frame(xtabs(lot_effectif~taxon_stades,data=tableEspeces)))
	colnames(sumEspeces)[colnames(sumEspeces)=="Freq"]<-"Effectif" # pas forcement le m nb de colonnes
# graphique ggplot
	
	g<-ggplot(sumEspeces)
	g<-g+geom_bar(aes(x="",y=Effectif,fill=taxon_stades,width=1),stat="identity") + 
			ggtitle(paste("Bilan Especes, DC",bilanEspeces@dc@dc_selectionne,"\n",bilanEspeces@datedebut,"=>",bilanEspeces@datefin))
			#theme(axis.line.x=element_line("none"))+theme(axis.title.x= element_text("none"))
	if (bilanEspeces@liste@listechoix!="aucun"){
		facet<-switch(bilanEspeces@liste@listechoix,
				"annee"=as.formula(~annee),
				"mois"=as.formula(~mois),
				"semaine"=as.formula(~semaine))
		g<-g+facet_wrap(facet,scales="fixed")
	}
	if (nb<=8) {
		g<-g+scale_fill_brewer(palette="Accent",name="Taxa")   
	} else if (nb<=12){
		p<-g+scale_fill_brewer(palette="Set3",name="Taxa")   
	}else{
		g<-g+scale_fill_manual(values=rainbow(nb))
	}
	if(h$action=="pie"){
		g<-g+ coord_polar(theta="y", start=pi)+xlab('') +ylab('')
	}
	print(g)   
	g<<-g
}


#' handler du calcul BilanEspeces : traitements 
#' appelle les fonctions funstat et funtable pour faire le bilan des migrations
#' dans des fichiers csv
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
hTableBilanEspeces=function(h,...) {
	if (exists("bilanEspeces",envir_stacomi)) {
		bilanEspeces<-get("bilanEspeces",envir_stacomi)
	} else {      
		funout(get("msg",envir_stacomi)$BilanEspeces.4,arret=TRUE)
	}
	DC=as.numeric(bilanEspeces@dc@dc_selectionne)	
	# update of refliste which does not need calcul button pushed
	bilanEspeces@liste<-get("refliste",envir_stacomi)   
	
	tableEspeces=bilanEspeces@data
	if (nrow(tableEspeces)==0) funout(get("msg",envir_stacomi)$BilanEspeces.5,arret=TRUE)
	tableEspeces$taxon_stades=paste(tableEspeces$tax_nom_latin,tableEspeces$std_libelle,sep="_")
	# je ne garde taxons_stades que pour les esp�ces pr�sentant plusieurs stades
	nbstades=tapply(tableEspeces$tax_nom_latin,tableEspeces$taxon_stades,function(X)(length(unique(X))))
	if (length(nbstades[nbstades>1])>0){
		les_multiples=names(nbstades[nbstades>1])
		tableEspeces[!tableEspeces$taxon_stades%in%les_multiples,"taxon_stades"]<-tableEspeces$tax_nom_latin[!tableEspeces$taxon_stades%in%les_multiples]
	} else tableEspeces$taxon_stades<-tableEspeces$tax_nom_latin
	# TODO ajouter les effectifs en fin de taxons_stades ???
	nb=length(unique(tableEspeces$taxon_stade))
	if (min(tableEspeces$lot_effectif)<0) {funout(get("msg",envir_stacomi)$BilanEspeces.6)
		tableEspeces$lot_effectif=abs(tableEspeces$lot_effectif)
	}
	now<-bilanEspeces@horodate@horodate
	sumEspeces=switch(bilanEspeces@liste@listechoix,
			"annee"=as.data.frame(xtabs(lot_effectif~taxon_stades+annee,data=tableEspeces)),
			"mois"=as.data.frame(xtabs(lot_effectif~taxon_stades+mois,data=tableEspeces)),
			"semaine"=as.data.frame(xtabs(lot_effectif~taxon_stades+semaine,data=tableEspeces)),
			"aucun"=as.data.frame(xtabs(lot_effectif~taxon_stades,data=tableEspeces)))
	colnames(sumEspeces)[colnames(sumEspeces)=="Freq"]<-"Effectif" # pas forcement le m nb de colonnes	funout(get("msg",envir_stacomi)$BilanMigration.9)  	
	path=file.path(normalizePath(path.expand(get("datawd",envir=envir_stacomi))),paste("tableEspece",now,".csv",sep=""),fsep ="\\")
	write.table(sumEspeces,path,row.names=TRUE,col.names=TRUE,sep=";",append=FALSE)
	funout(paste(get("msg",envir=envir_stacomi)$funtable.1,path,"\n"))
}

#' Interface for BilanEspece class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
#' @examples interface_BilanEspeces()
interface_BilanEspeces=function(){
	bilanEspeces=new("BilanEspeces")
	assign("bilanEspeces",bilanEspeces,envir = .GlobalEnv)
	funout(get("msg",envir=envir_stacomi)$interface_BilanEspeces.1)
	bilanEspeces@dc=charge(bilanEspeces@dc)   
	bilanEspeces@liste=charge(object=bilanEspeces@liste,
			vecteur=c("aucun","semaine","mois","annee"),
			label=get("msg",envir=envir_stacomi)$interface_BilanEspeces.7)
	quitte()
	group = ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	assign("group",group,envir = .GlobalEnv)  
	gl=glabel(text=get("msg",envir=envir_stacomi)$interface_BilanEspeces.2,container=group)
	add(ggroupboutons,group)
	choix(bilanEspeces@horodate,
			label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.3,
			nomassign="bilanEspeces_date_debut",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.5,
			decal=-2,
			affichecal=FALSE)
	choix(bilanEspeces@horodate,
			label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.4,
			nomassign="bilanEspeces_date_fin",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.6,
			decal=-1,
			affichecal=FALSE)
	choix(bilanEspeces@dc,objectBilan=bilanEspeces,is.enabled=TRUE)
	choix(bilanEspeces@liste)	
	ggroupboutonsbas = ggroup(horizontal=FALSE)
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)
	add(ggroupboutons,ggroupboutonsbas)
	toolbarlist = list(
			Calc=gaction(handler=hBilanEspecescalc, action=bilanEspeces, icon="new", label="calcul", tooltip=get("msg",envir=envir_stacomi)$interface_BilanEspeces.3),
			Graph=gaction(label="pie",tooltip=get("msg",envir=envir_stacomi)$interface_BilanEspeces.4,icon="bubbles",handler=hCamembert,action="pie"),
			Graph2=gaction(handler=hCamembert, icon="barplot", label="histo", tooltip=get("msg",envir=envir_stacomi)$interface_BilanEspeces.5,action="graph"),
			Stat=gaction(handler=hTableBilanEspeces, icon="dataframe", label="stat", tooltip=get("msg",envir=envir_stacomi)$interface_BilanEspeces.6),    
			annuler=gaction(handler= quitte,icon = "close",label="quitter")
	)    
	#add(ggroupboutonsbas, gtoolbar(toolbarlist))
	#addSpring(group)
	#graphes=ggraphics(width=600,height=400)
	add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
	x11()
	assign("graphes",graphes,envir=.GlobalEnv) 
}