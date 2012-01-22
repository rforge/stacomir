# Nom fichier :        BilanMigrationInterAnnuel.R
#' class BilanMigrationInterannuelle
#' Several year overview of fish migrations. It enables to draw 2 compare a year with statistics from several year, and to extract data
#' This class need prior call from bilanMigration to fill in the t_bilanmigrationjour_bjo as it loads the data in this table
#' @slot dc =Object of class \code{"RefDC"} The counting device of the fishway
#' @slot taxons =Object of class \code{"RefTaxon"} : the fish taxa
#' @slot stades =Object of class \code{"RefStades"} : the fish stage
#' @slot data =Object of class \code{"RefStades"} : migration data
#' @slot anneeDebut =Object of class \code{"RefAnnee"} : the starting year
#' @slot anneeFin =Object of class \code{"RefAnnee"} : the finishing year
#' @method connect
#' @method supprime
#' @method charge
#' @references \url{http://trac.eptb-vilaine.fr:8066/tracstacomi} 
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
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
setClass(Class="BilanMigrationInterAnnuelle",representation=
				representation(
						dc="RefDC",
						taxons="RefTaxon",
						stades="RefStades",
						data="data.frame",
						anneeDebut="RefAnnee",
						anneeFin="RefAnnee"
				),
		prototype=prototype(dc=new("RefDC"),
				taxons=new("RefTaxon"),
				stades=new("RefStades"),
				data=data.frame(),
				anneeDebut=new("RefAnnee"),
				anneeFin=new("RefAnnee")
		)
)

#' connect method for BilanMigrationInterannuelle class
#' @returnType S4 class BilanMigrationInterannuelle
#' @return bilanMigrationInterannuelle an instantianted object with values filled with user choice
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
setMethod("connect",signature=signature("BilanMigrationInterAnnuelle"),
		definition=function(objet,...)
		{ 
			# tableau contenant toutes les annees
			les_annees = (objet@anneeDebut@annee_selectionnee):(objet@anneeFin@annee_selectionnee)
			tax = objet@taxons@data$tax_code
			std = objet@stades@data$std_code
			dic= objet@dc@dc_selectionne
			requete=new("RequeteODBCwhere")
			requete@baseODBC=baseODBC
			requete@where=paste("WHERE bjo_annee IN ",vector_to_listsql(les_annees)," AND bjo_tax_code='",tax,"' AND bjo_std_code='",std,"' AND bjo_dis_identifiant=",dic,sep="")
			requete@select=paste("SELECT * FROM ",sch,"t_bilanmigrationjournalier_bjo",sep="")
			requete@order_by=" ORDER BY bjo_jour "
			requete<-connect(requete)
			
			# resultat de la requete
			objet@data<- killfactor(requete@query)
			
			# recuperation des indices des annees presentes dans la base
			index=unique(objet@data$bjo_annee) %in% les_annees
			
			# s'il manque des donnees pour certaines annees selectionnnees" 
			if (length(les_annees[!index]>0)) 
			{
				funout(paste(get("msg",envir=envir_stacomi)$BilanMigrationInterannuelle.1,
								paste(les_annees[!index],collapse=","),get("msg",envir=envir_stacomi)$BilanMigrationInterannuelle.2,"\n"))
			} # end if    
			
			# si toutes les annees sont presentes
			if (length(les_annees[index]>0)){
				funout(paste(get("msg",envir=envir_stacomi)$BilanMigrationInterannuelle.3,
								paste(les_annees[index],collapse=","), "\n")) 
			}  
			return(objet)
		}
)

# supprime les enregistrements de la base pour l'annee courante
# objet<-bmi
setMethod("supprime",signature=signature("BilanMigrationInterAnnuelle"),
		definition=function(objet,...)
		{ 
			# recuperation des annees taxons et stades concernes
			les_annees = (objet@anneeDebut@annee_selectionnee):(objet@anneeFin@annee_selectionnee)
			tax = objet@taxons@data$tax_code
			std = objet@stades@data$std_code
			dic= objet@dc@dc_selectionne
			requete=new("RequeteODBCwhere")
			requete@baseODBC=baseODBC
			requete@select=str_c("DELETE from ",sch,"t_bilanmigrationjournalier_bjo ")
			requete@where=paste("WHERE bjo_annee IN (",paste(les_annees,collapse=","),") AND bjo_tax_code='",tax,"' AND bjo_std_code='",std,"' AND bjo_dis_identifiant=",dic,sep="")
			requete<-connect(requete)
			requete=new("RequeteODBCwhere")
			requete@baseODBC=baseODBC
			requete@select=str_c("DELETE from ",sch,"t_bilanmigrationmensuel_bme ")
			requete@where=paste("WHERE bme_annee IN (",paste(les_annees,collapse=","),") AND bme_tax_code='",tax,"' AND bme_std_code='",std,"' AND bme_dis_identifiant=",dic,sep="")
			requete<-connect(requete)
		}
)

#  objet = bilanMigrationInterAnnuelle
setMethod("charge",signature=signature("BilanMigrationInterAnnuelle"),
		definition=function(objet,...)
		{ 
			if (exists("refDC",envir_stacomi)) {
				objet@dc<-get("refDC",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.1,arret=TRUE)
			}
			if (exists("refTaxons",envir_stacomi)) {
				objet@taxons<-get("refTaxons",envir_stacomi)
			} else {      
				funout(get("msg",envir_stacomi)$ref.2,arret=TRUE)
			}
			if (exists("refStades",envir_stacomi)){
				objet@stades<-get("refStades",envir_stacomi)
			} else 
			{
				funout(get("msg",envir_stacomi)$ref.3,arret=TRUE)
			}
			if (exists("anneeDebut",envir_stacomi)) {
				objet@anneeDebut<-get("anneeDebut",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.10,arret=TRUE)
			}  	
			if (exists("anneeFin",envir_stacomi)) {
				objet@anneeFin<-get("anneeFin",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.11,arret=TRUE)
			}
			objet<-connect(objet)
			assign("bilanMigrationInterannuelle",objet,envir_stacomi)
			funout(get("msg",envir_stacomi)$BilanMigrationInterannuelle.11)
			return(objet)
		}
)
# graphique de toutes les migrations interannuelles les unes sur les autres    
# objet = bilanMigrationInterAnnuelle = objet
hgraphBilanMigrationInterAnnuelle = function(h,...)
{
	bilanMigrationInterAnnuelle = charge(bilanMigrationInterAnnuelle)
	
	if(nrow(bilanMigrationInterAnnuelle@data)>0)
	{
		# TODO traitement des poids
		dat=bilanMigrationInterAnnuelle@data        
		dat<-dat[dat$bjo_labelquantite=="Effectif_total",]
		dat<-chnames(dat,c("bjo_annee","bjo_jour","bjo_labelquantite","bjo_valeur"),    c("annee","jour","labelquantite","valeur"))
		# il faut un champ date, on ramene tout les monde à
		dat$jour = as.POSIXct(strptime(strftime(dat$jour,'2000-%m-%d %H:%M:%S'),format='%Y-%m-%d %H:%M:%S'),tz="GMT")
		dat$annee=as.factor(dat$annee)
		
		dat=killfactor(dat)
		
		titre=paste(get("msg",envir_stacomi)$BilanMigrationInterannuelle.4,
				paste(min(dat$annee),max(dat$annee), collapse=":"),
				", ",
				bilanMigrationInterAnnuelle@dc@data$dis_commentaires[bilanMigrationInterAnnuelle@dc@data$dc==bilanMigrationInterAnnuelle@dc@dc_selectionne])
		soustitre=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin, ", ", bilanMigrationInterAnnuelle@stades@data$std_libelle, sep="")
		g<-ggplot(dat,aes(x=jour,y=valeur))
		g<-g+geom_line(aes(color=annee),position="dodge")+ opts(title=paste(titre, "\n", soustitre))+
				scale_x_datetime(name="date",major="months",minor="weeks", format="%d-%m")
		print(g)
		assign("g",g,envir=envir_stacomi)
		funout(get("msg",envir_stacomi)$BilanMigrationPar.6)
		
	}    else     {
		funout(get("msg",envir_stacomi)$BilanMigrationInterannuelle.5)
	}
}

# graphe affichant les migrations journalières
hgraphBilanMigrationInterAnnuelle2 = function(h,...)
{
	bilanMigrationInterAnnuelle = charge(bilanMigrationInterAnnuelle)
	
	# TODO traitement des poids
	dat=bilanMigrationInterAnnuelle@data
	dat=fundat(dat)
	dat=dat[dat$moyenne!=0,] # pour des raisons graphiques on ne garde pas les effectifs nuls generes par fundat
	newdat=dat[match(unique(as.character(dat$jour)),as.character(dat$jour)),]
	newdat=newdat[order(newdat$jour),] # pour avoir les range sur l'ensemble des valeurs dispo et pas seult l'annee en cours
	choix=select.list(choices=as.character(unique(dat$annee)[order(unique(dat$annee))]),
			preselect=as.character(max(dat$annee)),
			"choix annee",multiple=TRUE)
	amplitude=paste(min(as.numeric(as.character(dat$annee))),"-",max(as.numeric(as.character(dat$annee))),sep="")        
	if (length(choix)>0) { 
		# le layout pour l'affichage des graphiques
		vplayout <- function(x, y) { viewport(layout.pos.row = x, layout.pos.col = y)   }
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(length(choix),1,just="center")))   
		for(i in 1:length(choix))
		{
			
			tmp <- dat[as.numeric(as.character(dat$annee))==as.numeric(choix)[i],]
			tmp$annee=as.character(tmp$annee)
			g <- ggplot(newdat,aes(x=jour,y=valeur))
			g <- g+geom_ribbon(aes(ymin=mintab, ymax=maxtab,fill="amplitude"),color="black")+
					scale_x_datetime(name="date",major="months",minor="weeks", format="%d-%m")                           
			g<- g+ scale_fill_manual(name=eval(amplitude), values="grey80")  			
			# g<- g+ scale_colour_manual(name="Serie", values="red")
			g <- g+geom_bar(position="dodge",stat="identity",fill=I("orange"),size=1,alpha=0.9,data=tmp)
			g <- g+geom_point(aes(y=moyenne,col="Moyenne"),size=1,data=tmp)
			g <- g+geom_point(aes(col=annee),size=0.1,data=tmp)     # pour la légend                   
			g <- g+ scale_colour_manual(name="Series", values=c("black", "orange"))
			g <- g+opts(title=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,",",bilanMigrationInterAnnuelle@stades@data$std_libelle,unique(as.character(tmp$annee)),"/",amplitude))
			g <- g+scale_y_continuous(name="effectif")
			print(g, vp=vplayout(i,1)) 
			assign(paste("g",i,sep=""),g,envir_stacomi)
			funout(paste(get("msg",envir_stacomi)$BilanMigrationInterannuelle.10,"i=",paste(1:length(choix),collapse=","),"\n"))
		}  # end for
		
		
	} # end if
}  # end function 

##########################################################################
#'   @title statistics per time period
#'   function called for bilamMigrationInterannelle objects
#'   renames columnns replaces nulls,  
#'   and calculates reports with time period larger than day
#'   @param timesplit "week"  "2 week" "month" as provided to seq.POSIXT
#'   @return dat  a data frame with max mean min per time period
#####################################################################
fundat=function(dat,timesplit=NULL)
{
	
	if(nrow(dat)>0)
	{
		# ci dessous les calculs s'appliquent bien aux jours
		# remplacement des valeurs manquantes par des zeros par creation d'une sequence journaliere
		dat<-dat[dat$bjo_labelquantite=="Effectif_total",]
		dat<-chnames(dat,c("bjo_annee","bjo_jour","bjo_labelquantite","bjo_valeur"),    c("annee","jour","labelquantite","valeur"))
		dat=dat[,c("annee","jour","valeur")] 
		dat$jour=trunc.POSIXt(dat$jour, units='days')
		dat$jour = as.Date(strptime(strftime(dat$jour,'2000-%m-%d'),'%Y-%m-%d')) 
		
		
		# ci dessous calcul des sommes par semaine mois... Comme trunk.POSIXt ou floor ne prend pas 
		# la valeur week on est oblige de faire avec seq.POSIXt et calculer avec une boucle !
		if (!is.null(timesplit)){
			seq_timesplit= seq.POSIXt(from=strptime("2000-01-01",format='%Y-%m-%d'),
					to=strptime("2000-12-31",format='%Y-%m-%d'),
					by=getvalue(new("Refperiode"),timesplit))
			seq_timesplit<-as.Date(trunc(seq_timesplit, units='days'))
			# utilise la classe Refperiode pour avoir la correspondance entre le nom français et la variable utilisee par seq.POSIXt
			#datc=data.frame(rep(seq_timesplit,length(unique(dat$annee))),sort(rep(unique(dat$annee),length(seq_timesplit))))  # dataframe pour cumuls par periodes
			#colnames(datc)<-c(timesplit,"annee")
			# calcul des sommes par annee et par periode
			dat[,timesplit]<-dat$jour # pour avoir le format sinon renvoit un numérique
			# ci dessous on remplace une double boucle par un truc plus rapide
			for (j in 1:(length(seq_timesplit)-1)){
				dat[dat$jour>=seq_timesplit[j]&dat$jour<seq_timesplit[j+1],timesplit]<-seq_timesplit[j]
			}
			dat[dat$jour>=seq_timesplit[length(seq_timesplit)],timesplit]<-seq_timesplit[length(seq_timesplit)]
			dat[,"interv"]<-paste(dat[,"annee"],dat[,timesplit]) # on veut les valeurs uniques par annee et timesplit
			res<-tapply(dat$valeur,dat[,"interv"],sum,na.rm=TRUE)
			datc<-data.frame("annee"=substr(names(res),1,4),timesplit=substr(names(res),5,15),"valeur"=as.numeric(res))
			colnames(datc)[2]<-timesplit
			dat<-datc 
			rm(datc)
		} else {
			# si nul on remplace par jour pour generer le script en dessous
			timesplit="jour"
			jour2000=trunc.POSIXt(seq.POSIXt(from=strptime("2000-01-01",format='%Y-%m-%d'),
							to=strptime("2000-12-31",format='%Y-%m-%d'), by="day"), units='days')
			for (j in unique(dat$annee)){
				# les jours qui n'ont pas de bilan journalier pour ce jour sont rajoutés avec zéro
				jour2000restant<-jour2000[!jour2000 %in% dat[dat$annee==j,"jour"]]
				dat0=data.frame("jour"=jour2000restant,"annee"=j, "valeur"=0)
				dat=rbind(dat,dat0)
			} # end for
		}
		# calcul des valeurs min et max et moyenne en fonction de la coupure (jour, semaine,quinzaine, mois)
		
		maxdat<-tapply(dat$valeur,as.character(dat[,timesplit]),max,na.rm=TRUE)
		mindat<-tapply(dat$valeur,as.character(dat[,timesplit]),min,na.rm=TRUE)
		meandat<-tapply(dat$valeur,as.character(dat[,timesplit]),mean,na.rm=TRUE)
		datsummary<-data.frame("maxtab"=maxdat,"mintab"=mindat,"moyenne"=meandat)
		datsummary[,timesplit]<-names(maxdat)
		dat[,timesplit]<-as.character(dat[,timesplit])
		dat<-merge(dat,datsummary,by=timesplit)
		dat[,timesplit]<-as.POSIXct(strptime(dat[,timesplit],format='%Y-%m-%d')) # le format Posixct est nécessaire pour les ggplot
		rm(maxdat,mindat,meandat)
		dat<-dat[order(dat$annee,dat[,timesplit]),]
		# renvoit la premiere occurence qui correspond, pour n'importe quel jour min, max et moyenne sont OK
		return(dat)
	} else   {  # arret avec erreur
		funout(get("msg",envir_stacomi)$BilanMigrationInterannuelle.5,arret=TRUE)
	}    # end else
}
########################################
# fonction de creation des bilans cumules
############################################
hgraphBilanMigrationInterAnnuelle3 = function(h,...)
{ 
	bilanMigrationInterAnnuelle = charge(bilanMigrationInterAnnuelle)
	
	dat=bilanMigrationInterAnnuelle@data
	dat=fundat(dat)
	#dat=dat[order(dat$annee,dat$jour),]      
	choix=select.list(choices=as.character(unique(dat$annee)),preselect=as.character(max(dat$annee)),"choix annee",multiple=FALSE)
	amplitude=paste(min(as.numeric(as.character(dat$annee))),"-",max(as.numeric(as.character(dat$annee))),sep="")      
	#################
	# Calcul des cumsum
	###################
	for (an in as.character(unique(dat$annee))){
		# an=as.character(unique(dat$annee)) ;an<-an[1]
		dat[as.numeric(as.character(dat$annee))==an,"cumsum"]<-cumsum(dat[as.numeric(as.character(dat$annee))==an,"valeur"])
		dat[as.numeric(as.character(dat$annee))==an,"total_annuel"]<-max(dat[as.numeric(as.character(dat$annee))==an,"cumsum"])          
	}
	dat$cumsum=dat$cumsum/dat$total_annuel
	dat$jour=as.Date(dat$jour)
	dat$annee=as.factor(dat$annee)
	
	#################
	# Graphique
	###################
	
	g <- ggplot(dat,aes(x=jour,y=cumsum))
	tmp<-dat[as.numeric(as.character(dat$annee))==max(as.numeric(as.character(dat$annee))),]
	g <- g+geom_step(aes(col=annee,size=total_annuel))
	g <- g+geom_step(data=tmp,col="black",lty=2)
	g<-g+opts(title=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,",",bilanMigrationInterAnnuelle@stades@data$std_libelle,get("msg",envir_stacomi)$BilanMigrationInterannuelle.9,amplitude))
	g<-g+scale_y_continuous(name=get("msg",envir_stacomi)$BilanMigrationInterannuelle.8)
	g<-g+scale_x_date(name=get("msg",envir_stacomi)$BilanMigrationInterannuelle.7,major="months", minor="weeks", format="%b",lim=range(dat[dat$valeur>0&dat$cumsum!=1,"jour"]))# date 
	g<-g+scale_colour_manual(name=get("msg",envir_stacomi)$BilanMigrationInterannuelle.6,values=c(brewer.pal(9,"Set1"),brewer.pal(8,"Set2")))# annee
	print(g) 
	assign("g",g,envir_stacomi)
	funout(get("msg",envir_stacomi)$BilanMigrationPar.6)
}   



########################################
# Bilan migration comparant la migration hebdomadaire et la migration
# interannuelle hebdomadaire. fonctionne pour mensuelle et quinzaine et hebdomadaire
############################################
hgraphBilanMigrationInterAnnuelle4 = function(h,...)
{
	timesplit=h$action    # timesplit="quinzaine" timesplit="semaine" timesplit="mois"
	bilanMigrationInterAnnuelle = charge(bilanMigrationInterAnnuelle)
	dat=bilanMigrationInterAnnuelle@data
	dat=fundat(dat,timesplit)
	dat=dat[order(dat$annee,dat[,timesplit]),]
	dat$annee=as.factor(dat$annee)    
	#dat[,timesplit]<-as.Date(dat[,timesplit])
	# dat=dat[dat$moyenne!=0,] # pour des raisons graphiques on ne garde pas les effectifs nuls generes par fundat
	newdat=dat[match(as.character(unique(dat[,timesplit])),as.character(dat[,timesplit])),]
	newdat=newdat[order(newdat[,timesplit]),] # pour avoir les range sur l'ensemble des valeurs dispo et pas seult l'annee en cours
	thechoix=select.list(choices=as.character(unique(dat$annee)),preselect=as.character(max(as.numeric(as.character(dat$annee)))),"choix annee",multiple=TRUE)
	amplitude=paste(min(as.numeric(as.character(dat$annee))),"-",max(as.numeric(as.character(dat$annee))),sep="") 
	
	
	if (length(thechoix)>0) { 
		# le layout pour l'affichage des graphiques
		vplayout <- function(x, y) { viewport(layout.pos.row = x, layout.pos.col = y)   }
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(length(thechoix),1,just="center")))   
		for(i in 1:length(thechoix))  { 
			selection=as.numeric(as.character(dat$annee))==as.numeric(thechoix)[i] 
			tmp <- dat[selection,]
			tmp[tmp$valeur>=tmp$moyenne,"comp"]<-">=moy"
			tmp[tmp$valeur<tmp$moyenne,"comp"]<-"<moy"
			tmp[tmp$valeur==tmp$maxtab,"comp"]<-"max"
			tmp[tmp$valeur==tmp$mintab,"comp"]<-"min"
			tmp[tmp$moyenne==0,"comp"]<-"0"
			tmp$annee=as.factor(as.numeric(as.character(tmp$annee)))
			newdat$comp<-NA
			# for graphical reasons
			g <- ggplot(tmp,aes_string(x=timesplit,y="valeur"))
			g <- g+geom_crossbar(data=newdat,aes_string(x=timesplit, y="moyenne",ymin="mintab",ymax="maxtab"),fill="grey60",alpha=0.5,size=0.5)
			g <- g+geom_crossbar(stat="identity",aes_string(ymin="valeur",ymax="valeur",col="comp"),fatten=2)
			g <- g+scale_x_datetime(name=paste("mois"),major="month",minor=getvalue(new("Refperiode"),timesplit), format="%b",
					#lim=as.POSIXct(c(trunc((min(tmp[tmp$com!="0",timesplit])),"month")-delai,
					#				ceil((max(tmp[tmp$com!="0",timesplit])),"month")+delai))
			) 
			# pb the limit truncs the value
			g <- g+scale_y_continuous(name="effectif")
			cols <- c("max" = "blue","min" = "red",">=moy" = "darkgreen", "<moy" = "darkorange","0"="grey10")
			g <- g+scale_colour_manual(name=thechoix,value=cols)
			g<-g+opts(title=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,",",bilanMigrationInterAnnuelle@stades@data$std_libelle,", bilan par",timesplit,unique(as.character(tmp$annee)),"/",amplitude))
			print(g, vp=vplayout(i,1)) 
			assign(paste("g",i,sep=""),g,envir_stacomi)
			funout(paste(get("msg",envir_stacomi)$BilanMigrationInterannuelle.10,"i=",paste(1:length(thechoix),collapse=","),"\n"))
		}  # end for
	} # end if
}  # end function 


########################################
# Fonction similaire à la précédente mais pointrange et geom_bar
# interannuelle hebdomadaire. fonctionne pour mensuelle et quizaine et hebdomadaire
############################################
hgraphBilanMigrationInterAnnuelle5 = function(h,...)
{
	timesplit=h$action    # timesplit="quinzaine"
	bilanMigrationInterAnnuelle = charge(bilanMigrationInterAnnuelle)
	dat=bilanMigrationInterAnnuelle@data
	dat=fundat(dat,timesplit)
	dat=dat[order(dat$annee,dat[,timesplit]),]
	dat$annee=as.factor(dat$annee)    
	# dat=dat[dat$moyenne!=0,] # pour des raisons graphiques on ne garde pas les effectifs nuls generes par fundat
	newdat=dat[match(unique(dat[,timesplit]),dat[,timesplit]),]
	newdat=newdat[order(newdat[,timesplit]),] # pour avoir les range sur l'ensemble des valeurs dispo et pas seult l'annee en cours
	choix=select.list(choices=as.character(unique(dat$annee)),preselect=as.character(max(as.numeric(as.character(dat$annee)))),"choix annee",multiple=TRUE)
	amplitude=paste(min(as.numeric(as.character(dat$annee))),"-",max(as.numeric(as.character(dat$annee))),sep="") 
	
	
	if (length(choix)>0) { 
		# le layout pour l'affichage des graphiques
		vplayout <- function(x, y) { viewport(layout.pos.row = x, layout.pos.col = y)   }
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(length(choix),1,just="center")))   
		for(i in 1:length(choix))  { 
			selection=as.numeric(as.character(dat$annee))==as.numeric(choix)[i] 
			tmp <- dat[selection,]
			tmp[tmp$valeur>=tmp$moyenne,"comp"]<-">=moy"
			tmp[tmp$valeur<tmp$moyenne,"comp"]<-"<moy"
			tmp[tmp$valeur==tmp$maxtab,"comp"]<-"max"
			tmp[tmp$valeur==tmp$mintab,"comp"]<-"min"
			tmp[tmp$moyenne==0,"comp"]<-"0"
			tmp$annee=as.factor(as.numeric(as.character(tmp$annee)))
			newdat$comp<-NA
			g <- ggplot(tmp,aes_string(x=timesplit,y="valeur"))
			g<-g+geom_bar(stat="identity",aes_string(y="valeur",fill="comp"),alpha=0.5)
			g<-g+geom_pointrange(data=newdat,aes_string(x=timesplit, y="moyenne",ymin="mintab",ymax="maxtab"),alpha=1)
			g <- g+scale_x_datetime(name=paste("mois"),major="month",minor=getvalue(new("Refperiode"),timesplit), 
					format="%b",
					#lim=as.POSIXct(c(trunc((min(tmp[tmp$com!="0",timesplit])),"month"),
					#				ceil((max(tmp[tmp$com!="0",timesplit])),"month")))
			) 
			g <- g+scale_y_continuous(name="effectif")
			cols <- c("max" = "blue","min" = "red",">=moy" = "darkgreen", "<moy" = "darkorange","0"="grey10")
			g <- g+scale_fill_manual(name=choix,value=cols)
			g<-g+opts(title=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,",",bilanMigrationInterAnnuelle@stades@data$std_libelle,", bilan par",timesplit,unique(as.character(tmp$annee)),"/",amplitude))
			print(g, vp=vplayout(i,1)) 
			assign(paste("g",i,sep=""),g,envir_stacomi)
			funout(paste(get("msg",envir_stacomi)$BilanMigrationInterannuelle.10,"i=",paste(1:length(choix),collapse=","),"\n"))
		}  # end for
	} # end if
}  # end function 

# graphique des cumuls interannuels pour distinguer des tendances saisonnières, les données sont calculées par 
# quinzaine puis centrées réduites
hgraphBilanMigrationInterAnnuelle7 = function(h,...)
{
	bilanMigrationInterAnnuelle = charge(bilanMigrationInterAnnuelle)
	
	if(nrow(bilanMigrationInterAnnuelle@data)>0)
	{
		timesplit="quinzaine"
		dat=bilanMigrationInterAnnuelle@data
		dat=fundat(dat,timesplit)
		dat=dat[order(dat$annee,dat[,timesplit]),]
		dat$annee=as.factor(dat$annee)    
		sum_per_year<-tapply(dat$valeur,dat$annee,sum)
		sum_per_year<-data.frame(annee=names(sum_per_year),sum_per_year=sum_per_year)
		dat<-merge(dat,sum_per_year,by="annee")
		dat$std_valeur<-dat$valeur/dat$sum_per_year
		g <- ggplot(dat,aes_string(x=timesplit,y="std_valeur"))
		g<-g+geom_area(aes_string(y="std_valeur",fill="annee"),position="stack")
		g <- g+scale_x_datetime(name=paste("mois"),major="month",minor=getvalue(new("Refperiode"),timesplit),
				format="%b",lim=as.POSIXct(c(trunc((min(dat[dat$valeur!=0,timesplit])),"month"),ceil((max(dat[dat$valeur!="0",timesplit])),"month")))) 
		g <- g+scale_y_continuous(name="Somme des pourcentages annuels de migration par quinzaine")
		cols <- rainbow(length(levels(dat$annee)))
		g <- g+scale_fill_manual(name="annee",value=cols)
		g<-g+opts(title=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,",",bilanMigrationInterAnnuelle@stades@data$std_libelle,
						", saisonnalite de la migration")) 
		print(g)
		assign(paste("g",sep=""),g,envir_stacomi)
		funout(get("msg",envir_stacomi)$BilanMigrationPar.6)
		
	}    else     {
		funout(get("msg",envir_stacomi)$BilanMigrationInterannuelle.5)
	}
}
########################################
# ecriture de fichiers dans le datawd
############################################
htableBilanMigrationInterAnnuelle = function(h,...)
{
	# chargement des donnees
	bilanMigrationInterAnnuelle = charge(bilanMigrationInterAnnuelle)
	
	# TODO traitement des poids
	dat=bilanMigrationInterAnnuelle@data
	
	dat<-dat[dat$bjo_labelquantite=="Effectif_total",]
	dat<-chnames(dat,c("bjo_dis_identifiant","bjo_tax_code","bjo_std_code","bjo_annee","bjo_jour","bjo_labelquantite","bjo_valeur","bjo_horodateexport"),    c("DC","Taxon","Stade","Annee","Jour","Label_quantite","Nombre","Date d'export du bilan"))
	dat$Annee=as.factor(dat$Annee)
	dat = dat[,-1]
	
	tmp = dat$Jour
	
	DC = bilanMigrationInterAnnuelle@dc@dc_selectionne
	
	funtable(tableau=dat,duree=tmp,taxon=bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,stade=bilanMigrationInterAnnuelle@stades@data$std_libelle,DC,resum=NULL)
	
}