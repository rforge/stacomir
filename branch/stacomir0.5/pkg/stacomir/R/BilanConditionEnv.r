# Nom fichier :        BilanConditionEnv    (classe)
# Date de creation :   24/06/2009 13:49:20
# constructeur d'un bilan des conditions environnementales
#' class BilanConditionEnv simple output of one or several environemental conditons
#' @slot horodate="RefHorodate"
#' @slot stationMesure=="RefStationMesure"
#' @slot data=data.frame
#' @method connect
#' @method charge
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
setClass(Class="BilanConditionEnv",
		representation=representation(
				horodate="RefHorodate",
				stationMesure="RefStationMesure",
				data="data.frame",
				datedebut="POSIXt",
				datefin="POSIXt"),
		prototype=prototype(
				horodate=new("RefHorodate"),
				stationMesure=new("RefStationMesure"),
				data=data.frame())
)


#' connect method for BilanConditionEnv class
#' @return an object of BilanConditionEnv class
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
setMethod("connect",signature=signature("BilanConditionEnv"),
		definition=function(objet,h) {
			#  construit une requete ODBCwheredate
			requete=new("RequeteODBCwheredate")
			requete@baseODBC=baseODBC
			requete@datedebut=strptime(objet@datedebut,format="%Y-%m-%d")
			requete@datefin=strptime(objet@datefin,format="%Y-%m-%d")
			requete@colonnedebut="env_date_debut"
			requete@colonnefin="env_date_fin"
			requete@select=paste("SELECT", 
							" env_date_debut,",
							" env_date_fin,",
							" env_methode_obtention,",
							" val_libelle as env_val_identifiant,",
							" env_valeur_quantitatif,",
							" env_stm_identifiant",
							" FROM ",sch,"tj_conditionenvironnementale_env",
							" LEFT JOIN ref.tr_valeurparametrequalitatif_val on env_val_identifiant=val_identifiant",sep="")
			requete@order_by<-"ORDER BY env_stm_identifiant, env_date_debut"			
			tmp<-vector_to_listsql(objet@stationMesure@data$stm_identifiant)
			requete@and=paste(" AND env_stm_identifiant IN ",tmp )			
			requete<-connect(requete)			
			objet@data<-killfactor(requete@query)
			funout(get("msg",envir=envir_stacomi)$BilanCondtionEnv.1)
			return(objet)
		}
)

#' charge method for BilanCondtionEnv class
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
setMethod("charge",signature=signature("BilanConditionEnv"),definition=function(objet,h) {
			
			if (exists("refStationMesure",envir_stacomi)) {
				objet@stationMesure<-get("refStationMesure",envir_stacomi)
			} else {
				funout(get("msg",envir=envir_stacomi)$BilanCondtionEnv.2,arret=TRUE)
			}     
			
			if (exists("bilanConditionEnv_date_debut",envir_stacomi)) {
				objet@datedebut<-get("bilanConditionEnv_date_debut",envir_stacomi)@horodate
			} else {
				funout(get("msg",envir=envir_stacomi)$ref.5,arret=TRUE)
			}
			
			if (exists("bilanConditionEnv_date_fin",envir_stacomi))  {
				objet@datefin<-get("bilanConditionEnv_date_fin",envir_stacomi)@horodate
			}else {
				funout(get("msg",envir=envir_stacomi)$ref.6,arret=TRUE)
			}      		
			objet<-connect(objet)
			return(objet)
		})

# affiche un graphe si des CE sont dans la base pendant la periode selectionnee
#' hbilanConditionEnvgraph function called by handler which displays a graphe if environmental conditons are in the database during the selected period
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
hbilanConditionEnvgraph = function(h,...) 
{
	# chargement des conditions environnementales
	bilanConditionEnv=charge(bilanConditionEnv)
	
	# le dataframe contenant le res de la requete
	dat<-bilanConditionEnv@data
	
	if(length(unique(dat$env_stm_identifiant))!=0)
	{
		# le layout pour l'affichage des graphiques
		vplayout <- function(x, y) { viewport(layout.pos.row = x, layout.pos.col = y)   }
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(length(unique(dat$env_stm_identifiant)),1,just="center")))
		# la liste des graphes calcules
		lesGraphes=list()
		if(length(unique(dat$env_stm_identifiant))!= nrow(bilanConditionEnv@stationMesure@data))
		{
			funout(get("msg",envir=envir_stacomi)$BilanCondtionEnv.3)
		}
		
		# pour toutes les stations de mesure selectionnees
		for (i in 1:length(unique(dat$env_stm_identifiant)))
		{
			# l'identifiant de la station de mesure courante
			stmidentifiant <- unique(dat$env_stm_identifiant)[i]
			
			# la ligne de bilanConditionEnv@stationMesure en cours de traitement
			stm <- bilanConditionEnv@stationMesure@data[bilanConditionEnv@stationMesure@data$stm_identifiant==stmidentifiant,]
			
			# toutes les mesures pour la station de mesure selectionnee
			nameColonne <- as.character(stm$stm_libelle)
			datstm <- chnames(dat,"env_valeur_quantitatif", nameColonne) 
			datstm <- datstm[datstm$env_stm_identifiant==stmidentifiant,]
			
			#AES<-structure(as.list(c("x"=as.name(datstm$env_date_debut),"y"=as.name(eval(nameColonne)))),class="uneval")
			
			# creation du graphe
			g<-ggplot(datstm,aes_string(x="env_date_debut",y=nameColonne))  
			g<-g+geom_line(aes_string(colour=nameColonne))+scale_y_continuous(stm$stm_libelle)+ scale_x_datetime(name="date",major="months", minor="weeks", format="%b-%y")
			
			# affichage du graphe à l'ecran
			print(g, vp=vplayout(i,1))
			
			# ajout du graphe dans la liste      
			lesGraphes[stm$stm_libelle] = g
		} 
	}
	else
	{
		funout(get("msg",envir=envir_stacomi)$BilanCondtionEnv.4,arret=TRUE);
	}	
	return (lesGraphes)
}   

# affichage de stats
hbilanConditionEnvstat = function(h,...) 
{
	bilanConditionEnv=charge(bilanConditionEnv)
	
	# le dataframe contenant le res de la requete
	dat<-bilanConditionEnv@data
	dat<-chnames(dat,"env_stm_identifiant","stm_identifiant")
	dat<-merge(dat,bilanConditionEnv@stationMesure@data,by="stm_identifiant")
	funout(get("msg",envir=envir_stacomi)$BilanCondtionEnv.5)
	liste = tapply(dat$env_valeur_quantitatif,dat$stm_libelle,summary)
	for (i in names(liste)){
		funout(paste(" station",i,":\nMin  ; 1st Qu.;  Median  ;    Mean   ; 3rd Qu.  ;     Max   ;    Na's  ) = \n",paste(liste[[i]],collapse="   ;   "),"\n"))
	}
}
