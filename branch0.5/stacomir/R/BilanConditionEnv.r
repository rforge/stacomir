# Nom fichier :        BilanConditionEnv    (classe)
# Date de creation :   24/06/2009 13:49:20
# constructeur d'un bilan des conditions environnementales
#' class BilanConditionEnv simple output of one or several environmental
#' conditions...
#' 
#' Annual overview of environmental conditions. Enables to draw charts and
#' write files.
#' 
#' @include RefHorodate.r 
#' @include RefStationMesure.r
#' @include create_generic.r
#' @include utilitaires.r
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanConditionEnv", horodate=new("Horodate"),
#' stationMesure=new("RefStationMesure"), data=data.frame(),
#' requete=new("RequeteODBCwheredate"))}.
#' @author cedric.briand"at"eptb-vilaine.fr
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
#' @concept Bilan Object 
#' @examples
#' 
#' showClass("BilanConditionEnv")
#' 
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
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("connect",signature=signature("BilanConditionEnv"),
		definition=function(object,h,..) {
			#  construit une requete ODBCwheredate
			requete=new("RequeteODBCwheredate")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@datedebut=strptime(object@datedebut,format="%Y-%m-%d")
			requete@datefin=strptime(object@datefin,format="%Y-%m-%d")
			requete@colonnedebut="env_date_debut"
			requete@colonnefin="env_date_fin"
			requete@select=paste("SELECT", 
							" env_date_debut,",
							" env_date_fin,",
							" env_methode_obtention,",
							" val_libelle as env_val_identifiant,",
							" env_valeur_quantitatif,",
							" env_stm_identifiant",
							" FROM ",get("sch",envir=envir_stacomi),"tj_conditionenvironnementale_env",
							" LEFT JOIN ref.tr_valeurparametrequalitatif_val on env_val_identifiant=val_identifiant",sep="")
			requete@order_by<-"ORDER BY env_stm_identifiant, env_date_debut"			
			tmp<-vector_to_listsql(object@stationMesure@data$stm_identifiant)
			requete@and=paste(" AND env_stm_identifiant IN ",tmp )			
			requete<-stacomirtools::connect(requete)			
			object@data<-stacomirtools::killfactor(requete@query)
			funout(get("msg",envir=envir_stacomi)$BilanCondtionEnv.1)
			return(object)
		}
)

#' charge method for BilanCondtionEnv class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("charge",signature=signature("BilanConditionEnv"),definition=function(object,h) {
			
			if (exists("refStationMesure",envir_stacomi)) {
				object@stationMesure<-get("refStationMesure",envir_stacomi)
			} else {
				funout(get("msg",envir=envir_stacomi)$BilanCondtionEnv.2,arret=TRUE)
			}     
			
			if (exists("bilanConditionEnv_date_debut",envir_stacomi)) {
				object@datedebut<-get("bilanConditionEnv_date_debut",envir_stacomi)@horodate
			} else {
				funout(get("msg",envir=envir_stacomi)$ref.5,arret=TRUE)
			}
			
			if (exists("bilanConditionEnv_date_fin",envir_stacomi))  {
				object@datefin<-get("bilanConditionEnv_date_fin",envir_stacomi)@horodate
			}else {
				funout(get("msg",envir=envir_stacomi)$ref.6,arret=TRUE)
			}      		
			object<-connect(object)
			return(object)
		})

# affiche un graphe si des CE sont dans la base pendant la periode selectionnee
#' hbilanConditionEnvgraph function called by handler which displays a graphe if environmental conditons are in the database during the selected period
#' @param h 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
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
		vplayout <- function(x, y) { grid::viewport(layout.pos.row = x, layout.pos.col = y)   }
		grid::grid.newpage()
		grid::pushViewport(grid::viewport(layout = grid::grid.layout(length(unique(dat$env_stm_identifiant)),1,just="center")))
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
			datstm <- stacomirtools::chnames(dat,"env_valeur_quantitatif", nameColonne) 
			datstm <- datstm[datstm$env_stm_identifiant==stmidentifiant,]
			
			#AES<-structure(as.list(c("x"=as.name(datstm$env_date_debut),"y"=as.name(eval(nameColonne)))),class="uneval")
			
			# creation du graphe
			g<-ggplot(datstm,aes_string(x="env_date_debut",y=nameColonne))  
			g<-g+geom_line(aes_string(colour=nameColonne))+scale_y_continuous(stm$stm_libelle)+
					scale_x_datetime(name="date")
			
			# affichage du graphe a  l'ecran
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
	dat<-stacomirtools::chnames(dat,"env_stm_identifiant","stm_identifiant")
	dat<-merge(dat,bilanConditionEnv@stationMesure@data,by="stm_identifiant")
	funout(get("msg",envir=envir_stacomi)$BilanCondtionEnv.5)
	liste = tapply(dat$env_valeur_quantitatif,dat$stm_libelle,summary)
	for (i in names(liste)){
		funout(paste(" station",i,":\nMin  ; 1st Qu.;  Median  ;    Mean   ; 3rd Qu.  ;     Max   ;    Na's  ) = \n",paste(liste[[i]],collapse="   ;   "),"\n"))
	}
	path=file.path(path.expand(get("datawd",envir=envir_stacomi)),paste("env_cond.csv",sep=""),fsep ="\\")
	write.table(dat,path,sep=';',row.names=FALSE)
	funout(paste(get("msg",envir=envir_stacomi)$funtable.1,path,"\n"))
}
