# Nom fichier :        BilanEspeces    (classe)
# Projet :             controle migrateur calmig/prog/classe
# Date de creation :   31/03/2008 17:21:18

#' this class is used to make the assessment of all species, and their number, per month
#' #' it writes an histogram of number per month
#' it
#' class BilanEspeces
#' @slot dc="RefDC"
#' @slot horodate="RefHorodate"
#' @slot stades="RefStades"
#' @slot datedebut="POSIXlt"
#' @slot datefin="POSIXlt"
#' @slot data="data.frame"
#' @slot duree="POSIXct"
#' @slot liste="RefListe" liste des pas de temps possibles pour le découpage, les valeurs sont passées dans l'interface
#' @method calcule
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
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
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
setMethod("connect",signature=signature("BilanEspeces"),definition=function(objet) {
			bilanEspeces<-objet # pour faciliter la debug, l'argument formel de la classe doit etre forcement objet !
			requete=new("RequeteODBCwheredate")
			requete@baseODBC=baseODBC
			requete@datedebut=bilanEspeces@datedebut
			requete@datefin=bilanEspeces@datefin
			requete@colonnedebut="ope_date_debut"
			requete@colonnefin="ope_date_fin"
			requete@select= paste("SELECT lot_identifiant, ope_date_debut, ope_date_fin,",
					" lot_effectif, lot_tax_code, lot_std_code, tax_nom_latin, std_libelle,",
					" date_part('year', ope_date_debut) as annee,",
					" date_part('month',ope_date_debut) as mois,",
					" date_part('week',ope_date_debut) as semaine",
					" FROM ",sch,"t_operation_ope",
					" INNER JOIN ",sch,"t_lot_lot ON ope_identifiant=lot_ope_identifiant",
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
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
hBilanEspecescalc=function(h,...){
	charge(h$action)
}


#' charge method for BilanEspeces
#' verifies the content of objects and calls the connect method
#' @return BilanEspeces with slots filled by user choice
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
#' @expamples bilanEspeces=new("BilanEspeces")
setMethod("charge",signature=signature("BilanEspeces"),definition=function(objet,...){
			funout(get("msg",envir_stacomi)$BilanEspeces.7)
			bilanEspeces<-objet
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
#' @note pas besoin de refaire tourner calcul si une autre liste à été chargée, les effectifs <0 sont transformés en positifs
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export

hCamembert = function(h,...) {	
	# TODO règler le pb de handler et transformation en histo ?
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
	# je ne garde taxons_stades que pour les espèces présentant plusieurs stades
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
	colnames(sumEspeces)[colnames(sumEspeces)=="Freq"]<-"Effectif" # pas forcement le mê nb de colonnes
# graphique ggplot
	
	p<-ggplot(sumEspeces)
	p<-p+geom_bar(aes(x="",y=Effectif,fill=taxon_stades,width=1)) + 
			opts(title = paste("Bilan Especes, DC",bilanEspeces@dc@dc_selectionne,"\n",bilanEspeces@datedebut,"=>",bilanEspeces@datefin))+
			opts(axis.title.x=theme_blank())+opts(plot.title = theme_text(size=15, colour="gray",vjust=1))
	if (bilanEspeces@liste@listechoix!="aucun"){
		facet<-switch(bilanEspeces@liste@listechoix,
				"annee"=as.formula(~annee),
				"mois"=as.formula(~mois),
				"semaine"=as.formula(~semaine))
		p<-p+facet_wrap(facet,scales="free")
	}
	if (nb<=8) {
		p<-p+scale_fill_brewer(palette="Accent",name="Taxa")   
	} else if (nb<=12){
		p<-p+scale_fill_brewer(palette="Set3",name="Taxa")   
	}else{
		p<-p+scale_fill_manual(values=rainbow(nb))
	}
	if(h$action=="pie"){
		p<-p+ coord_polar(theta="y", start=pi)
	}
	print(p)   
}


#TODO développer cette fonction
#' handler du calcul hHistogramme
#' trace un histogramme des espèces fréquentant la passe... 
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
hHistogramme = function(h,...) {
	if (exists("BilanEspeces",envir_stacomi)) {
		BilanEspeces<-get("BilanEspeces",envir_stacomi)
	} else {      
		funout(get("msg",envir_stacomi)$BilanMigration.5,arret=TRUE)
	}
	taxon= as.character(BilanEspeces@taxons@data$tax_nom_latin)
	stade= as.character(BilanEspeces@stades@data$std_libelle)
	DC=as.numeric(BilanEspeces@dc@dc_selectionne)	
	if (BilanEspeces@pasDeTemps@dureePas==86400 & BilanEspeces@pasDeTemps@dureePas==86400) {
		BilanEspeces@data$duree=BilanEspeces@duree
		# pour sauvegarder sous excel
		BilanEspeces@data<-funtraitementdate(BilanEspeces@data,
				nom_coldt="duree",
				annee=FALSE,
				mois=TRUE,
				quinzaine=TRUE,
				semaine=TRUE,
				jour_an=TRUE,
				jour_mois=FALSE,
				heure=FALSE)
		BilanEspeces@data$Cumsum=cumsum(BilanEspeces@data$Effectif_total)
		# pour sauvegarder sous excel
		annee=unique(strftime(as.POSIXlt(BilanEspeces@duree),"%Y"))
		dis_commentaire=  as.character(BilanEspeces@dc@data$dis_commentaires[BilanEspeces@dc@data$dc%in%BilanEspeces@dc@dc_selectionne]) 
		update_geom_defaults("step", aes(size = 3))
		p<-ggplot(BilanEspeces@data)+
				geom_step(aes(x=duree,y=Cumsum,colour=mois))+
				ylab(get("msg",envir_stacomi)$BilanMigration.6)+
				opts(plot.title=theme_text(size=10,colour="blue"),
						title=paste(get("msg",envir_stacomi)$BilanMigration.7,dis_commentaire,", ",taxon,", ",stade,", ",annee,sep=""))   
		print(p)	
	} else {
		funout(get("msg",envir_stacomi)$BilanMigration.8)
	}
}
#TODO developper cette fonction (pour l'instant fausse)
#' handler du calcul BilanEspeces : traitements 
#' appelle les fonctions funstat et funtable pour faire le bilan des migrations
#' dans des fichiers csv
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
hTableBilanEspeces=function(h,...) {
	funout("Tableau de sortie \n")
	if (exists("BilanEspeces",envir_stacomi)) 
	{
		BilanEspeces<-get("BilanEspeces",envir_stacomi)
	} 
	else 
	{      
		funout(get("msg",envir_stacomi)$BilanMigration.5,arret=TRUE)
	}
	taxon= as.character(BilanEspeces@taxons@data$tax_nom_latin)
	stade= as.character(BilanEspeces@stades@data$std_libelle)
	DC=as.numeric(BilanEspeces@dc@dc_selectionne)	
	funout(get("msg",envir_stacomi)$BilanMigration.9)  	
	resum=funstat(tableau=BilanEspeces@data,duree=BilanEspeces@duree,taxon,stade,DC)
	funtable(tableau=BilanEspeces@data,duree=BilanEspeces@duree,taxon,stade,DC,resum)
}

#' Interface for BilanEspece class
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
#' @expamples interface_BilanEspeces()
interface_BilanEspeces=function(){
	bilanEspeces=new("BilanEspeces")
	assign("bilanEspeces",bilanEspeces,envir = .GlobalEnv)
	funout(get("msg",envir=envir_stacomi)$interface_BilanEspeces.1)
	bilanEspeces@dc=charge(bilanEspeces@dc)   
	bilanEspeces@liste=charge(objet=bilanEspeces@liste,
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
	choix(bilanEspeces@dc,objetBilan=bilanEspeces,is.enabled=TRUE)
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
	add(ggroupboutonsbas, gtoolbar(toolbarlist))
	addSpring(group)
	graphes=ggraphics(width=650,height=650)
	add(ggrouptotal1,graphes )  # on ajoute au groupe horizontal
	assign("graphes",graphes,envir=.GlobalEnv) 
}