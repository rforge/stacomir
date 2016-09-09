#' Class "BilanMigrationConditionEnv"
#' 
#' Enables to compute an annual overview of fish migration and environmental
#' conditions in the same chart
#' 
#' 
#' @include RefAnnee.r
#' @section Objects from the Class: Objects can be created by calls of the form
#' 	\code{new("BilanMigrationConditionEnv",
#' 	bilanMigration=new("BilanMigration"),
#' 	bilanConditionEnv=new("BilanConditionEnv"))}.  
#' @slot dc Object of class \code{\link{RefDC-class}}, the counting device
#' @slot data Object of class \code{"data.frame"} data for bilan lot 
#' @slot taxons An object of class \code{\link{RefTaxon-class}}
#' @slot stades An object of class \code{\link{RefStades-class}}
#' @slot anneeDebut Object of class \code{\link{RefAnnee-class}}. refAnnee allows to choose year of beginning
#' @slot anneeFin Object of class \code{\link{RefAnnee-class}}
#' refAnnee allows to choose last year of the Bilan
#' 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other Bilan Class \code{\linkS4class{Bilan_carlot}}, 
#' 	\code{\linkS4class{Bilan_poids_moyen}}, 
#' 	\code{\linkS4class{Bilan_stades_pigm}}, 
#' 	\code{\linkS4class{Bilan_taille}}, 
#' 	\code{\linkS4class{BilanConditionEnv}}, 
#' 	\code{\linkS4class{BilanEspeces}}, 
#' 	\code{\linkS4class{BilanFonctionnementDC}}, 
#' 	\code{\linkS4class{BilanFonctionnementDF}}, 
#' 	\code{\linkS4class{BilanMigration}},  
#' 	\code{\linkS4class{BilanMigrationConditionEnv}}, 
#' 	\code{\linkS4class{BilanMigrationInterAnnuelle}}, 
#' 	\code{\linkS4class{BilanMigrationPar}}
#' @concept Bilan Object 
#' @export
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
#' @param object An object of class \link{BilanMigrationInterAnnuelle-class}
#' @return bilanMigrationInterannuelle an instantianted object with values filled with user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("connect",signature=signature("BilanMigrationInterAnnuelle"),
		definition=function(object)
		{ 
			# tableau contenant toutes les annees
			les_annees = (object@anneeDebut@annee_selectionnee):(object@anneeFin@annee_selectionnee)
			tax = object@taxons@data$tax_code
			std = object@stades@data$std_code
			dic= object@dc@dc_selectionne
			requete=new("RequeteODBCwhere")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@where=paste("WHERE bjo_annee IN ",vector_to_listsql(les_annees)," AND bjo_tax_code='",tax,"' AND bjo_std_code='",std,"' AND bjo_dis_identifiant=",dic,sep="")
			requete@select=paste("SELECT * FROM ",get("sch",envir=envir_stacomi),"t_bilanmigrationjournalier_bjo",sep="")
			requete@order_by=" ORDER BY bjo_jour "
			requete<-stacomirtools::connect(requete)
			
			# resultat de la requete
			object@data<- stacomirtools::killfactor(requete@query)
			
			# recuperation des indices des annees presentes dans la base
			index=unique(object@data$bjo_annee) %in% les_annees
			
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
			return(object)
		}
)

#' supprime method for BilanMigrationInterannuelle class
#' @param object An object of class \link{BilanMigrationInterAnnuelle-class}
#' @return nothing
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export

setMethod("supprime",signature=signature("BilanMigrationInterAnnuelle"),
		definition=function(object)
		{ 
			# recuperation des annees taxons et stades concernes
			les_annees = (object@anneeDebut@annee_selectionnee):(object@anneeFin@annee_selectionnee)
			tax = object@taxons@data$tax_code
			std = object@stades@data$std_code
			dic= object@dc@dc_selectionne
			requete=new("RequeteODBCwhere")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@select=stringr::str_c("DELETE from ",get("sch",envir=envir_stacomi),"t_bilanmigrationjournalier_bjo ")
			requete@where=paste("WHERE bjo_annee IN (",paste(les_annees,collapse=","),") AND bjo_tax_code='",tax,"' AND bjo_std_code='",std,"' AND bjo_dis_identifiant=",dic,sep="")
			requete<-stacomirtools::connect(requete)
			requete=new("RequeteODBCwhere")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@select=stringr::str_c("DELETE from ",get("sch",envir=envir_stacomi),"t_bilanmigrationmensuel_bme ")
			requete@where=paste("WHERE bme_annee IN (",paste(les_annees,collapse=","),") AND bme_tax_code='",tax,"' AND bme_std_code='",std,"' AND bme_dis_identifiant=",dic,sep="")
			requete<-stacomirtools::connect(requete)
			return(invisible(NULL))
		}
		
)

#' loading method for BilanMigrationInterannuelle class
#' @param object An object of class \link{BilanMigrationInterAnnuelle-class}
#' @return An object of class  \link{BilanMigrationInterAnnuelle-class}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("charge",signature=signature("BilanMigrationInterAnnuelle"),
		definition=function(object)
		{ 
			if (exists("refDC",envir_stacomi)) {
				object@dc<-get("refDC",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.1,arret=TRUE)
			}
			if (exists("refTaxons",envir_stacomi)) {
				object@taxons<-get("refTaxons",envir_stacomi)
			} else {      
				funout(get("msg",envir_stacomi)$ref.2,arret=TRUE)
			}
			if (exists("refStades",envir_stacomi)){
				object@stades<-get("refStades",envir_stacomi)
			} else 
			{
				funout(get("msg",envir_stacomi)$ref.3,arret=TRUE)
			}
			if (exists("anneeDebut",envir_stacomi)) {
				object@anneeDebut<-get("anneeDebut",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.10,arret=TRUE)
			}  	
			if (exists("anneeFin",envir_stacomi)) {
				object@anneeFin<-get("anneeFin",envir_stacomi)
			} else {
				funout(get("msg",envir_stacomi)$ref.11,arret=TRUE)
			}
			object<-connect(object)
			assign("bilanMigrationInterannuelle",object,envir_stacomi)
			funout(get("msg",envir_stacomi)$BilanMigrationInterannuelle.11)
			return(object)
		}
)
#' Plot of all interannual from top to bottom
#' @param h handler
#' @param ... additional parameters
hgraphBilanMigrationInterAnnuelle = function(h,...)
{
	bilanMigrationInterAnnuelle = charge(bilanMigrationInterAnnuelle)
	
	if(nrow(bilanMigrationInterAnnuelle@data)>0)
	{
		# TODO traitement des poids
		dat=bilanMigrationInterAnnuelle@data        
		dat<-dat[dat$bjo_labelquantite=="Effectif_total",]
		dat<-stacomirtools::chnames(dat,c("bjo_annee","bjo_jour","bjo_labelquantite","bjo_valeur"),    c("annee","jour","labelquantite","valeur"))
		# il faut un champ date, on ramene tout les monde e
		dat$jour = as.POSIXct(strptime(strftime(dat$jour,'2000-%m-%d %H:%M:%S'),format='%Y-%m-%d %H:%M:%S'),tz="GMT")
		dat$annee=as.factor(dat$annee)
		
		dat=stacomirtools::killfactor(dat)
		
		titre=paste(get("msg",envir_stacomi)$BilanMigrationInterannuelle.4,
				paste(min(dat$annee),max(dat$annee), collapse=":"),
				", ",
				bilanMigrationInterAnnuelle@dc@data$dis_commentaires[bilanMigrationInterAnnuelle@dc@data$dc==bilanMigrationInterAnnuelle@dc@dc_selectionne])
		soustitre=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin, ", ", bilanMigrationInterAnnuelle@stades@data$std_libelle, sep="")
		g<-ggplot(dat,aes(x=jour,y=valeur))
		g<-g+geom_line(aes(color=annee),position="dodge")+ labs(title=paste(titre, "\n", soustitre))+
				scale_x_datetime(name="date")
		print(g)
		assign("g",g,envir=envir_stacomi)
		funout(get("msg",envir_stacomi)$BilanMigrationPar.6)
		
	}    else     {
		funout(get("msg",envir_stacomi)$BilanMigrationInterannuelle.5)
	}
}

#'Plot of daily migrations
#' @param h handler
#' @param ... additional parameters
hgraphBilanMigrationInterAnnuelle2 = function(h,...)
{
	bilanMigrationInterAnnuelle = charge(bilanMigrationInterAnnuelle)
	
	# TODO traitement des poids
	dat=bilanMigrationInterAnnuelle@data
	dat=fundat(dat)
	dat=dat[dat$moyenne!=0,] # pour des raisons graphiques on ne garde pas les effectifs nuls generes par fundat
	newdat=dat[match(unique(as.character(dat$jour)),as.character(dat$jour)),]
	newdat=newdat[order(newdat$jour),] # pour avoir les range sur l'ensemble des valeurs dispo et pas seult l'annee en cours
	the_choice=select.list(choices=as.character(unique(dat$annee)[order(unique(dat$annee))]),
			preselect=as.character(max(dat$annee)),
			"choice annee",multiple=TRUE)
	amplitude=paste(min(as.numeric(as.character(dat$annee))),"-",max(as.numeric(as.character(dat$annee))),sep="")        
	if (length(the_choice)>0) { 
		# le layout pour l'affichage des graphiques
		vplayout <- function(x, y) { grid::viewport(layout.pos.row = x, layout.pos.col = y)   }
		grid::grid.newpage()
		grid::pushViewport(grid::viewport(layout = grid::grid.layout(length(the_choice),1,just="center")))   
		for(i in 1:length(the_choice))
		{
			amplitudechoice<-paste(the_choice[i],'/',amplitude)
			tmp <- dat[as.numeric(as.character(dat$annee))==as.numeric(the_choice)[i],]
			tmp$annee=as.character(tmp$annee)
			g <- ggplot(newdat,aes(x=jour,y=valeur))
			g <- g+geom_ribbon(aes(ymin=mintab, ymax=maxtab,fill="amplitude"),color="grey20")
			g <- g+geom_bar(aes(fill=I("orange")),position="dodge",stat="identity",alpha=0.9,data=tmp)
			g<- g+ scale_fill_manual(name=eval(amplitudechoice), values=c("grey80","orange"),
					labels = c("amplitude historique",the_choice[i]))
			g <- g+geom_point(aes(y=valeur,col=annee),data=tmp,size=0.5)  
			g <- g+	geom_line(aes(y=moyenne),col=I("brown"),data=tmp)
			g <- g+geom_point(aes(y=moyenne,col=I("red")),size=0.8,data=tmp)		           
			g <- g+ scale_colour_manual(name=amplitudechoice, values=c("orange3","red"),
							labels=c(the_choice[i],stringr::str_c("Moyenne interannuelle",amplitude)))+
					guides(fill = guide_legend(reverse=TRUE))
			g <- g+labs(title=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,",",bilanMigrationInterAnnuelle@stades@data$std_libelle,unique(as.character(tmp$annee)),"/",amplitude))
			g <- g+scale_x_datetime(name="effectif",breaks="months",minor_breaks="weeks", labels=date_format("%d-%m"))
			g<-g+theme_bw()
			print(g, vp=vplayout(i,1)) 
			assign(paste("g",i,sep=""),g,envir_stacomi)
			funout(paste(get("msg",envir_stacomi)$BilanMigrationInterannuelle.10,"i=",paste(1:length(choice),collapse=","),"\n"))
		}  # end for
		
		
	} # end if
}  # end function 






#' statistics per time period
#' 
#' function called for bilamMigrationInterannelle objects renames columns
#' replaces nulls, and calculates reports with time period larger than day
#' 
#' 
#' @param dat a data frame
#' @param timesplit "week" "2 week" "month" as provided to seq.POSIXT, default NULL
#' @return a data frame with mean, max, and min calculated for each timesplit
#' @seealso \code{\linkS4class{Bilan_poids_moyen}}
fundat=function(dat,timesplit=NULL)
{
	
	if(nrow(dat)>0)
	{
		# ci dessous les calculs s'appliquent bien aux jours
		# remplacement des valeurs manquantes par des zeros par creation d'une sequence journaliere
		dat<-dat[dat$bjo_labelquantite=="Effectif_total",]
		dat<-stacomirtools::chnames(dat,c("bjo_annee","bjo_jour","bjo_labelquantite","bjo_valeur"),    c("annee","jour","labelquantite","valeur"))
		dat=dat[,c("annee","jour","valeur")] 
		dat$jour=trunc.POSIXt(dat$jour, digits='days')
		dat$jour = as.Date(strptime(strftime(dat$jour,'2000-%m-%d'),'%Y-%m-%d')) 
		
		
		# ci dessous calcul des sommes par semaine mois... Comme trunk.POSIXt ou floor ne prend pas 
		# la valeur week on est oblige de faire avec seq.POSIXt et calculer avec une boucle !
		if (!is.null(timesplit)){
			seq_timesplit= seq.POSIXt(from=strptime("2000-01-01",format='%Y-%m-%d'),
					to=strptime("2000-12-31",format='%Y-%m-%d'),
					by=getvalue(new("Refperiode"),timesplit))
			seq_timesplit<-as.Date(trunc(seq_timesplit, digits='days'))
			# utilise la classe Refperiode pour avoir la correspondance entre le nom franeais et la variable utilisee par seq.POSIXt
			#datc=data.frame(rep(seq_timesplit,length(unique(dat$annee))),sort(rep(unique(dat$annee),length(seq_timesplit))))  # dataframe pour cumuls par periodes
			#colnames(datc)<-c(timesplit,"annee")
			# calcul des sommes par annee et par periode
			dat[,timesplit]<-dat$jour # pour avoir le format sinon renvoit un numerique
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
			jour2000=as.Date(Hmisc::trunc.POSIXt(seq.POSIXt(from=strptime("2000-01-01",format='%Y-%m-%d'),
							to=strptime("2000-12-31",format='%Y-%m-%d'), by="day"), digits='days'))
			for (j in unique(dat$annee)){
				# les jours qui n'ont pas de bilan journalier pour ce jour sont rajoutes avec zero
				jour2000restant<-jour2000[!jour2000 %in% dat[dat$annee==j,"jour"]]
				dat0=data.frame("jour"=jour2000restant,"annee"=j, "valeur"=NA)
				dat=rbind(dat,dat0)
			} # end for
		}
		# calcul des valeurs min et max et moyenne en fonction de la coupure (jour, semaine,quinzaine, mois)
		
		maxdat<-tapply(dat$valeur,as.character(dat[,timesplit]),max,na.rm=TRUE)
		mindat<-tapply(dat$valeur,as.character(dat[,timesplit]),min,na.rm=TRUE)
		meandat<-tapply(dat$valeur,as.character(dat[,timesplit]),mean,na.rm=TRUE)
		datsummary<-data.frame("maxtab"=maxdat,"mintab"=mindat,"moyenne"=meandat)
		datsummary<-datsummary[!is.infinite(datsummary$maxtab),]# the minimum and max of empty set are -Inf and Inf respectively
		datsummary[,timesplit]<-names(maxdat)[!is.infinite(maxdat)]
		dat[,timesplit]<-as.character(dat[,timesplit])
		dat<-merge(dat,datsummary,by=timesplit)
		dat[,timesplit]<-as.POSIXct(strptime(dat[,timesplit],format='%Y-%m-%d')) # le format Posixct est necessaire pour les ggplot
		rm(maxdat,mindat,meandat)
		dat<-dat[order(dat$annee,dat[,timesplit]),]
		# renvoit la premiere occurence qui correspond, pour n'importe quel jour min, max et moyenne sont OK
		return(dat)
	} else   {  # arret avec erreur
		funout(get("msg",envir_stacomi)$BilanMigrationInterannuelle.5,arret=TRUE)
	}    # end else
}

#' Step plot with different years displayed on the same graph. One year
#' can be highlighted against the others
#' @param h handler
#' @param ... additional parameters
hgraphBilanMigrationInterAnnuelle3 = function(h,...)
{ 
	bilanMigrationInterAnnuelle = charge(bilanMigrationInterAnnuelle)	
	dat=bilanMigrationInterAnnuelle@data
	dat=fundat(dat)
	#dat=dat[order(dat$annee,dat$jour),] 
	dat$valeur[is.na(dat$valeur)]<-0 # sinon si il ne reste qu'une ligne peut planter
	the_choice=select.list(choices=as.character(unique(dat$annee)),preselect=as.character(max(dat$annee)),"choice annee",multiple=FALSE)
	amplitude=paste(min(as.numeric(as.character(dat$annee))),"-",max(as.numeric(as.character(dat$annee))),sep="")      
	#################
	# Calcul des cumsum
	###################

	#dat$valeur[dat$valeur<0]<-0
	for (an in unique(dat$annee)){
		# an=as.character(unique(dat$annee)) ;an<-an[1]
		dat[dat$annee==an,"cumsum"]<-cumsum(dat[dat$annee==an,"valeur"])
		dat[dat$annee==an,"total_annuel"]<-max(dat[dat$annee==an,"cumsum"])          
	}
	dat$cumsum=dat$cumsum/dat$total_annuel
	dat$jour=as.Date(dat$jour)
	dat$annee=as.factor(dat$annee)
	# bug, enleve les annees avec seulement une ligne

	#################
	# Graphique
	###################
	
	g <- ggplot(dat,aes(x=jour,y=cumsum))
	tmp<-dat[as.numeric(as.character(dat$annee))==as.numeric(the_choice),]
	g <- g+geom_step(aes(col=annee,size=total_annuel))
	g <- g+geom_step(data=tmp,col="black",lty=2)
	g<-g+labs(title=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,",",bilanMigrationInterAnnuelle@stades@data$std_libelle,get("msg",envir_stacomi)$BilanMigrationInterannuelle.9,amplitude))
	g<-g+scale_y_continuous(name=get("msg",envir_stacomi)$BilanMigrationInterannuelle.8)
	g<-g+scale_x_date(name=get("msg",envir_stacomi)$BilanMigrationInterannuelle.7,breaks="months", 
			minor_breaks="weeks", 
			labels=date_format("%b"),
			limits=range(dat[dat$valeur>0&dat$cumsum!=1,"jour"]))# date 
	g<-g+scale_colour_hue(name=get("msg",envir_stacomi)$BilanMigrationInterannuelle.6,l=70, c=150)# annee
	print(g) 
	assign("g",g,envir_stacomi)
	funout(get("msg",envir_stacomi)$BilanMigrationPar.6)
}   



#' Plot comparing the migration  to the migration  
#' computed for all years available in the daily migration table.
#' This function plots comparisions for periods of 1 week, 2 weeks, month
#'  @param h A handler
#'  @param ... Additional parameters
hgraphBilanMigrationInterAnnuelle4 = function(h,...)
{
	timesplit=h$action    # timesplit="quinzaine" timesplit="semaine" timesplit="mois"
	bilanMigrationInterAnnuelle = charge(bilanMigrationInterAnnuelle)
	dat=bilanMigrationInterAnnuelle@data
	dat=fundat(dat,timesplit)
	dat=dat[order(dat$annee,dat[,timesplit]),]
	dat$annee=as.factor(dat$annee)
	dat$keeptimesplit<-dat[,timesplit]
	if(timesplit=="mois") {
		dat[,timesplit]<-strftime(dat[,timesplit],format="%m")
	} else if (timesplit=="quinzaine") {
		dat[,timesplit]<-strftime(dat[,timesplit],format="%m/%d")
	} else {
		dat[,timesplit]<-strftime(dat[,timesplit],format="%W")
	} 
	dat[,timesplit]<-as.factor(dat[,timesplit])
	
	#dat[,timesplit]<-as.Date(dat[,timesplit])
	# dat=dat[dat$moyenne!=0,] # pour des raisons graphiques on ne garde pas les effectifs nuls generes par fundat
	newdat=dat[match(unique(dat[,timesplit]),dat[,timesplit]),]
	newdat=newdat[order(newdat[,"keeptimesplit"]),] # pour avoir les range sur l'ensemble des valeurs dispo et pas seult l'annee en cours
	thechoice=select.list(choices=as.character(unique(dat$annee)),preselect=as.character(max(as.numeric(as.character(dat$annee)))),"choice annee",multiple=TRUE)
	amplitude=paste(min(as.numeric(as.character(dat$annee))),"-",max(as.numeric(as.character(dat$annee))),sep="") 
	# here change 12/2012 the geom_crossbar now needs a factor, label change according to timesplit

	newdat[,timesplit]<-as.factor(newdat[,timesplit])
	levels(newdat[,timesplit])<-newdat[,timesplit] # to have the factor in the right order from january to dec
	if (length(thechoice)>0) { 
		# le layout pour l'affichage des graphiques
		vplayout <- function(x, y) { grid::viewport(layout.pos.row = x, layout.pos.col = y)   }
		grid::grid.newpage()
		grid::pushViewport(grid::viewport(layout = grid::grid.layout(length(thechoice),1,just="center")))   
		for(i in 1:length(thechoice))  { 
			selection=as.numeric(as.character(dat$annee))==as.numeric(thechoice)[i] 
			tmp <- dat[selection,]
			tmp[tmp$valeur>=tmp$moyenne,"comp"]<-">=moy"
			tmp[tmp$valeur<tmp$moyenne,"comp"]<-"<moy"
			tmp[tmp$valeur==tmp$maxtab,"comp"]<-"max"
			tmp[tmp$valeur==tmp$mintab,"comp"]<-"min"
			tmp[tmp$moyenne==0,"comp"]<-"0"
			tmp$annee=as.factor(as.numeric(as.character(tmp$annee)))
			newdat$comp<-NA
			#newdatperyear<-newdat
			# for graphical reasons
			#newdatperyear<-newdatperyear[localnewdatperyear[,timesplit]%in%tmp[,timesplit],]
#TODO here bug ne s'affiche pas		
			
			g <- ggplot(tmp,aes_string(x=timesplit,y="valeur"))
			g <- g+geom_crossbar(data=newdat,aes_string(x=timesplit, 
							y="moyenne",
							ymin="mintab",ymax="maxtab"),fill="grey60",alpha=0.5,size=0.5)
			g <- g+geom_crossbar(stat="identity",aes_string(ymin="valeur",ymax="valeur",col="comp"),fatten=2)
			#g <- g+scale_x_date(name=paste("mois"),breaks="month",minor_breaks=getvalue(new("Refperiode"),label=date_format("%b"),timesplit))
			#lim=as.POSIXct(c(Hmisc::trunc.POSIXt((min(tmp[tmp$com!="0",timesplit])),"month")-delai,
			#				Hmisc::ceil.POSIXt((max(tmp[tmp$com!="0",timesplit])),"month")+delai)) 
			# pb the limit truncs the value
			g <- g+ylab("effectif")
			cols <- c("max" = "blue","min" = "red",">=moy" = "darkgreen", "<moy" = "darkorange","0"="grey10")
			g <- g+scale_colour_manual(name=thechoice,values=cols)
			g<-g+labs(title=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,",",bilanMigrationInterAnnuelle@stades@data$std_libelle,", bilan par",timesplit,unique(as.character(tmp$annee)),"/",amplitude))
			g<-g+ theme_bw()
			print(g, vp=vplayout(i,1)) 
			assign(paste("g",i,sep=""),g,envir_stacomi)
			funout(paste(get("msg",envir_stacomi)$BilanMigrationInterannuelle.10,"i=",paste(1:length(thechoice),collapse=","),"\n"))
		}  # end for
	} # end if
}  # end function 



#' Function displaying comparaison similar to \link{hgraphBilanMigrationInterAnnuelle4} but using pointrange and geom_bar
#' This function plots comparisions for periods of 1 week, 2 weeks, month
#'  @param h A handler
#'  @param ... Additional parameters
hgraphBilanMigrationInterAnnuelle5 = function(h,...)
{
	timesplit=h$action    # timesplit="quinzaine" # timesplit="mois"
	bilanMigrationInterAnnuelle = charge(bilanMigrationInterAnnuelle)
	dat=bilanMigrationInterAnnuelle@data
	dat=fundat(dat,timesplit)
	dat$annee=as.factor(dat$annee) 
	dat=dat[order(dat$annee,dat[,timesplit]),]
	dat$keeptimesplit<-dat[,timesplit]
	if(timesplit=="mois") {
		dat[,timesplit]<-strftime(dat[,timesplit],format="%m")
	} else if (timesplit=="quinzaine") {
		dat[,timesplit]<-strftime(dat[,timesplit],format="%m/%d")
	} else {
		dat[,timesplit]<-strftime(dat[,timesplit],format="%W")
	} 
	dat[,timesplit]<-as.factor(dat[,timesplit])

	# dat=dat[dat$moyenne!=0,] # pour des raisons graphiques on ne garde pas les effectifs nuls generes par fundat
	newdat=dat[match(unique(dat[,timesplit]),dat[,timesplit]),]
	newdat=newdat[order(newdat[,"keeptimesplit"]),] # il peut y avoir des annees pour le calcul de range qui s'ajoutent 
	# et viennent d'autres annees, il faut donc reordonner.
#	dat[,timesplit]<-gdata::reorder(dat[,timesplit], new.order=match(levels(dat[,timesplit]),newdat[,timesplit]))	
#	levels(dat[,timesplit])<-newdat[,timesplit]	
#	levels(newdat[,timesplit])<-newdat[,timesplit]	

	the_choice=select.list(choices=as.character(unique(dat$annee)),preselect=as.character(max(as.numeric(as.character(dat$annee)))),"choice annee",multiple=TRUE)
	amplitude=paste(min(as.numeric(as.character(dat$annee))),"-",max(as.numeric(as.character(dat$annee))),sep="") 
	
	
	if (length(the_choice)>0) { 
		# le layout pour l'affichage des graphiques
		vplayout <- function(x, y) { grid::viewport(layout.pos.row = x, layout.pos.col = y)   }
		grid::grid.newpage()
		grid::pushViewport(grid::viewport(layout = grid::grid.layout(length(the_choice),1,just="center")))   
		for(i in 1:length(the_choice))  { 
			selection=as.numeric(as.character(dat$annee))==as.numeric(the_choice)[i] 
			tmp <- dat[selection,]
			tmp[tmp$valeur>=tmp$moyenne,"comp"]<-">=moy"
			tmp[tmp$valeur<tmp$moyenne,"comp"]<-"<moy"
			tmp[tmp$valeur==tmp$maxtab,"comp"]<-"max"
			tmp[tmp$valeur==tmp$mintab,"comp"]<-"min"
			tmp[tmp$moyenne==0,"comp"]<-"0"
			tmp$annee=as.factor(as.numeric(as.character(tmp$annee)))
			newdat$comp<-NA
			g <- ggplot(tmp,aes_string(x=timesplit,y="valeur"))
			g<-g+geom_dotplot(aes_string(x=timesplit, y="valeur"),data=dat,stackdir = "center",binaxis = "y",position = "dodge",dotsize = 0.5,fill="wheat") #position = "dodge",dotsize = 0.4,alpha=0.5,binwidth = 1.5
			g<-g+geom_pointrange(data=newdat,aes_string(x=timesplit, y="moyenne",ymin="mintab",ymax="maxtab"),alpha=1,size=0.8)
			g<-g+geom_bar(stat="identity",aes_string(y="valeur",fill="comp"),alpha=0.7)			
			g <- g+scale_y_continuous(name="effectif")
			cols <- c("max" = "blue","min" = "red",">=moy" = "darkgreen", "<moy" = "darkorange","0"="grey10")
			g <- g+scale_fill_manual(name=the_choice,values=cols)
			g<-g+labs(title=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,",",bilanMigrationInterAnnuelle@stades@data$std_libelle,", bilan par",timesplit,unique(as.character(tmp$annee)),"/",amplitude))
			print(g, vp=vplayout(i,1)) 
			assign(paste("g",i,sep=""),g,envir_stacomi)
			funout(paste(get("msg",envir_stacomi)$BilanMigrationInterannuelle.10,"i=",paste(1:length(the_choice),collapse=","),"\n"))
		}  # end for
	} # end if
}  # end function 


#' This function creates a cumulated area plot to highlight seasonal trends in migration. Data are calculated by
#' 2 weeks period then centered and reduced
#'  @param h A handler
#'  @param ... Additional parameters
hgraphBilanMigrationInterAnnuelle7 = function(h,...)
{
	bilanMigrationInterAnnuelle = charge(bilanMigrationInterAnnuelle)
	
	if(nrow(bilanMigrationInterAnnuelle@data)>0)
	{
		timesplit="quinzaine"
		dat=bilanMigrationInterAnnuelle@data
		dat=fundat(dat,timesplit)
		dat$annee=as.factor(dat$annee)    
		sum_per_year<-tapply(dat$valeur,dat$annee,sum)
		sum_per_year<-data.frame(annee=names(sum_per_year),sum_per_year=sum_per_year)
		dat<-merge(dat,sum_per_year,by="annee")
		dat$std_valeur<-dat$valeur/dat$sum_per_year
		all_15<-unique(dat[,timesplit])
		# below I'm adding 0 instead of nothing for 15 days without value
		for (i in 1:length(unique(dat$annee))){#i=5
			annee<-unique(dat$annee)[i]
			this_year_15<-unique(dat[dat$annee==annee,timesplit])
			missing<-all_15[!all_15%in%this_year_15]
			if (length(missing>=1)){
				missingdat<-data.frame("annee"=annee,
						"quinzaine"=missing,
						"valeur"=0,
						"maxtab"=0,
						"mintab"=0,
						"moyenne"=0,
						"sum_per_year"=0,
						"std_valeur"=0)
				dat<-rbind(dat,missingdat)
			}
		}
		dat=dat[order(dat$annee,dat[,timesplit]),]
		g <- ggplot(dat,aes_string(x=timesplit,y="std_valeur"))
		g<-g+geom_area(aes_string(y="std_valeur",fill="annee"),position="stack")
		g <- g+scale_x_datetime(name=paste("mois"),breaks="month",
				minor_breaks=getvalue(new("Refperiode"),timesplit),
				labels=date_format("%b"),
				limits=as.POSIXct(c(Hmisc::trunc.POSIXt((min(dat[dat$valeur!=0,timesplit])),"month"),Hmisc::ceil.POSIXt((max(dat[dat$valeur!="0",timesplit])),"month")))) 
		g <- g+scale_y_continuous(name="Somme des pourcentages annuels de migration par quinzaine")
		cols <- grDevices::rainbow(length(levels(dat$annee)))
		g <- g+scale_fill_manual(name="annee",values=cols)
		g<-g+labs(title=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,",",bilanMigrationInterAnnuelle@stades@data$std_libelle,
						", saisonnalite de la migration")) 
		print(g)
		assign(paste("g",sep=""),g,envir_stacomi)
		funout(get("msg",envir_stacomi)$BilanMigrationPar.6)
		
	}    else     {
		funout(get("msg",envir_stacomi)$BilanMigrationInterannuelle.5)
	}
}
#'Writing of results in datawd
#'  @param h A handler
#'  @param ... Additional parameters
htableBilanMigrationInterAnnuelle = function(h,...)
{
	# chargement des donnees
	bilanMigrationInterAnnuelle = charge(bilanMigrationInterAnnuelle)
	# TODO traitement des poids
	dat=bilanMigrationInterAnnuelle@data
	dat<-dat[dat$bjo_labelquantite=="Effectif_total",]
	dat<-stacomirtools::chnames(dat,c("bjo_dis_identifiant","bjo_tax_code","bjo_std_code","bjo_annee","bjo_jour","bjo_labelquantite","bjo_valeur","bjo_horodateexport"),    c("DC","Taxon","Stade","Annee","Jour","Label_quantite","Nombre","Date d'export du bilan"))
	dat$Annee=as.factor(dat$Annee)
	dat = dat[,-1]
	tmp = dat$Jour
	DC = bilanMigrationInterAnnuelle@dc@dc_selectionne
	funtable(tableau=dat,
			time.sequence=tmp,
			taxon=bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,
			stade=bilanMigrationInterAnnuelle@stades@data$std_libelle,
			DC,
			resum=NULL)
}
