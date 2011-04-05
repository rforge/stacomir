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
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
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
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
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
						  les_annees[!index],get("msg",envir=envir_stacomi)$BilanMigrationInterannuelle.2))
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
# objet<-bil
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
      requete@select=paste("DELETE from t_bilanmigrationjournalier_bjo ")
      requete@where=paste("WHERE bjo_annee IN ",vector_to_listsql(les_annees)," AND bjo_tax_code='",tax,"' AND bjo_std_code='",std,"' AND bjo_dis_identifiant=",dic,";",sep="")
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
				 paste(unique(dat$annee), collapse=","),
				 ", ",
				 bilanMigrationInterAnnuelle@dc@data$dis_commentaires[bilanMigrationInterAnnuelle@dc@data$dc==bilanMigrationInterAnnuelle@dc@dc_selectionne])
         soustitre=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin, ", ", bilanMigrationInterAnnuelle@stades@data$std_libelle, sep="")
        g<-ggplot(dat,aes(x=jour,y=valeur))
        g<-g+geom_line(aes(col=annee),position="dodge")+ opts(title=paste(titre, "\n", soustitre))+scale_x_datetime(name="date",major="months",minor="weeks", format="%d-%m")
          print(g)
    }
    else
    {
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
    newdat=dat[match(unique(dat$jour),dat$jour),]
    newdat=newdat[order(newdat$jour),] # pour avoir les range sur l'ensemble des valeurs dispo et pas seult l'annee en cours
    choix=select.list(choices=as.character(unique(dat$annee)),preselect=as.character(unique(dat$annee))[1],"choix annee",multiple=TRUE)
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
              g <- ggplot(tmp,aes(x=jour,y=valeur))
              g <- g+geom_ribbon(aes(ymin=mintab, ymax=maxtab,fill="amplitude"), colour="black",data=newdat,alpha=0.5 )+scale_x_datetime(name="date",major="months",minor="weeks", format="%d-%m")                           
              g<- g+ scale_fill_manual(name=amplitude, values="grey20")             
               # g<- g+ scale_colour_manual(name="Serie", values="red")
              g <- g+geom_bar(position="dodge",stat="identity",fill=I("orange"),col="orange",size=1,alpha=0.5)
              g <- g+geom_point(data=newdat,aes(x=jour,y=moyenne,col="Moyenne"),size=1)
              g <- g+geom_point(aes(col=annee),size=0.1)                        
              g <- g+ scale_colour_manual(name="Series", values=c("red", "orange"))
              g <- g+opts(title=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,",",bilanMigrationInterAnnuelle@stades@data$std_libelle,", EPACS",unique(as.character(tmp$annee)),"/",amplitude))
              g <- g+scale_y_continuous(name="effectif")
              print(g, vp=vplayout(i,1)) 
			  assign(paste("g",i,sep=""),g,envir_stacomi)
			  funout(paste(get("msg",envir_stacomi)$BilanMigrationInterannuelle.10,"i=",paste(1:length(choix),collapse=","),"\n"))
          }  # end for
		  
		  
      } # end if
}  # end function 

##########################################################################
#   Fonction appellee pour traiter les donnees de migration interannuelle
#   Cette fonction renomme les colonnes, remplace les nuls, et 
#   Calcule les bilans pour des intervalles plus larges que le jour
#   @ param timesplit "week"  "2 week" "month" doit pouvoir etre interprete par seq.POSIXT
#   @ return dat= un data frame
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
          dat$jour = as.POSIXct(strptime(strftime(dat$jour,'2000-%m-%d %H:%M:%S'),'%Y-%m-%d %H:%M:%S',tz="GMT")) 
                    jour2000=seq.POSIXt(from=strptime("2000-01-01 00:00:00",format='%Y-%m-%d %H:%M:%S',tz="GMT"),
          to=strptime("2000-12-31 00:00:00",format='%Y-%m-%d %H:%M:%S',tz="GMT"),
          by="day")
          for (j in unique(dat$annee)){
           jour2000[!jour2000 %in% dat[dat$annee==j,"jour"]]# les jours qui n'ont pas de bilan journalier pour ce jour 
           dat0=data.frame("jour"=jour2000,"annee"=j, "valeur"=0)
           dat=rbind(dat,dat0)
         } # end for
         # ci dessous calcul des sommes par semaine mois... Comme trunk.POSIXt ou floor ne prend pas 
         # la valeur week on est oblige de faire avec seq.POSIXt et calculer avec une boucle !
          if (!is.null(timesplit)){
          seq_timesplit= seq.POSIXt(from=strptime("2000-01-01 00:00:00",format='%Y-%m-%d %H:%M:%S',tz="GMT"),
          to=strptime("2000-12-31 00:00:00",format='%Y-%m-%d %H:%M:%S',tz="GMT"),
          by=getvalue(new("Refperiode"),timesplit))
          # utilise la classe Refperiode pour avoir la correspondance entre le nom français et la variable utilisee par seq.POSIXt
          datc=data.frame(rep(seq_timesplit,length(unique(dat$annee))),sort(rep(unique(dat$annee),length(seq_timesplit))))  # dataframe pour cumuls par periodes
          colnames(datc)<-c(timesplit,"annee")
         # calcul des sommes par annee et par periode
          for (an in unique(dat$annee)){
           for (j in 1:(length(seq_timesplit)-1)){
            datc[datc$annee==an&datc[,timesplit]==seq_timesplit[j],"valeur"]<-sum(dat[dat$jour>=seq_timesplit[j]&dat$jour<seq_timesplit[j+1]&dat$annee==an,"valeur"])
            # somme pour par exemple la premiere semaine...
            }# end for j
           } # end for an 
           # je remplace le dat
           datc=datc[!is.na(datc$valeur),]# je vire les valeurs NA qui correspondent à la derniere periode
           dat<-datc 
          } else {
          # si nul on remplace par jour pour generer le script en dessous
          timesplit="jour"
          }
          # calcul des valeurs min et max et moyenne en fonction de la coupure (jour, semaine,quinzaine, mois)

          for(i in unique(dat[,timesplit]))
            {
			index<-dat[,timesplit]==i
			index[is.na(index)]<-FALSE # ce cas arrive....
            dat[index,"maxtab"] <- max(dat[index,"valeur"], na.rm = TRUE )
            dat[index,"mintab"] <-min(dat[index,"valeur"], na.rm = TRUE )
            dat[index,"moyenne"] = mean(dat[index,"valeur"], na.rm = TRUE )
          } # end for
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
    dat=dat[order(dat$annee,dat$jour),]      
    choix=select.list(choices=as.character(unique(dat$annee)),preselect=as.character(unique(dat$annee))[1],"choix annee",multiple=FALSE)
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
    timesplit=h$action    # timesplit="quinzaine"
    bilanMigrationInterAnnuelle = charge(bilanMigrationInterAnnuelle)
    dat=bilanMigrationInterAnnuelle@data
    dat=fundat(dat,timesplit)
    dat=dat[order(dat$annee,dat[,timesplit]),]
    dat$annee=as.factor(dat$annee)    
   # dat=dat[dat$moyenne!=0,] # pour des raisons graphiques on ne garde pas les effectifs nuls generes par fundat
    newdat=dat[match(unique(dat[,timesplit]),dat[,timesplit]),]
    newdat=newdat[order(newdat[,timesplit]),] # pour avoir les range sur l'ensemble des valeurs dispo et pas seult l'annee en cours
    choix=select.list(choices=as.character(unique(dat$annee)),preselect=as.character(unique(dat$annee))[1],"choix annee",multiple=TRUE)
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
              g <- g+geom_crossbar(data=newdat,aes_string(x=timesplit, y="moyenne",ymin="mintab",ymax="maxtab"),fill="grey40",alpha=0.5)
              g <- g+geom_crossbar(stat="identity",aes_string(ymin="valeur",ymax="valeur",col="comp"))
              g <- g+scale_x_datetime(name=paste("mois"),major="month",minor=getvalue(new("Refperiode"),timesplit), format="%b",lim=as.POSIXct(c(trunc((min(tmp[tmp$com!="0",timesplit])),"month"),ceil((max(tmp[tmp$com!="0",timesplit])),"month")))) 
              g <- g+scale_y_continuous(name="effectif")
              cols <- c("max" = "blue","min" = "red",">=moy" = "darkgreen", "<moy" = "darkorange","0"="grey10")
              g <- g+scale_colour_manual(name=choix,value=cols)
              g<-g+opts(title=paste(bilanMigrationInterAnnuelle@taxons@data$tax_nom_latin,",",bilanMigrationInterAnnuelle@stades@data$std_libelle,", bilan par",timesplit,unique(as.character(tmp$annee)),"/",amplitude))
             print(g, vp=vplayout(i,1)) 
			 assign(paste("g",i,sep=""),g,envir_stacomi)
			 funout(paste(get("msg",envir_stacomi)$BilanMigrationInterannuelle.10,"i=",paste(1:length(choix),collapse=","),"\n"))
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
	choix=select.list(choices=as.character(unique(dat$annee)),preselect=as.character(unique(dat$annee))[1],"choix annee",multiple=TRUE)
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
			g <- g+scale_x_datetime(name=paste("mois"),major="month",minor=getvalue(new("Refperiode"),timesplit), format="%b",lim=as.POSIXct(c(trunc((min(tmp[tmp$com!="0",timesplit])),"month"),ceil((max(tmp[tmp$com!="0",timesplit])),"month")))) 
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