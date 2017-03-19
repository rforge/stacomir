#' class BilanConditionEnv simple output of one or several environmental
#' conditions...
#' 
#' Annual overview of environmental conditions. Enables to draw some plot. Mostly used to build
#' joined graphs for BilanMigration in class BilanMigrationConditionEnv
#' 
#' @include RefHorodate.r 
#' @include RefStationMesure.r
#' @include create_generic.r
#' @include utilitaires.r
#' @slot horodate \link{RefHorodate-class}
#' @slot stationMesure \link{RefStationMesure-class}
#' @slot data \code{data.frame}
#' @slot datedebut A \link[base]{-.POSIXt} value
#' @slot datefin A \link[base]{-.POSIXt} value 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("BilanConditionEnv", horodate=new("Horodate"),
#' stationMesure=new("RefStationMesure"), data=data.frame(),
#' requete=new("RequeteODBCwheredate"))}.
#' @author cedric.briand"at"eptb-vilaine.fr
#' @family Bilan Objects
#' @keywords classes
#' @aliases BilanConditionEnv bilanConditionEnv
#' @keywords classes
#' @example inst/examples/bilanConditionEnv_example.R
#' @export 
setClass(Class="BilanConditionEnv",
		representation=representation(			
				stationMesure="RefStationMesure",
				horodatedebut="RefHorodate",
				horodatefin="RefHorodate",
				data="data.frame"
		),
		prototype=prototype(
				horodatedebut=new("RefHorodate"),
				horodatefin=new("RefHorodate"),
				stationMesure=new("RefStationMesure"),
				data=data.frame())
)


#' connect method for BilanConditionEnv class
#' @param object An object of class \link{BilanConditionEnv-class}
#' @return an object of BilanConditionEnv class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("connect",signature=signature("BilanConditionEnv"),definition=function(object) {
			#object<-bil_CE
			requete=new("RequeteODBCwheredate")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@datedebut=strptime(object@horodatedebut@horodate,format="%Y-%m-%d")
			requete@datefin=strptime(object@horodatefin@horodate,format="%Y-%m-%d")
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
			funout(gettext("Environmental conditions loading query completed\n",domain="R-stacomiR"))
			return(object)
		}
)
#' command line interface for BilanConditionEnv class
#' @param object An object of class \link{BilanConditionEnv-class}
#' @param stationmesure A character, the code of the monitoring station, which records environmental parameters \link{choice_c,RefStationMesure-method}
#' @param datedebut The starting date as a character, formats like \code{\%Y-\%m-\%d} or \code{\%d-\%m-\%Y} can be used as input
#' @param datefin The finishing date of the Bilan, for this class this will be used to calculate the number of daily steps.
#' @param silent Boolean default FALSE, if TRUE information messages not displayed.
#' @return An object of class \link{BilanConditionEnv-class}
#' The choice_c method fills in the data slot for RefStationMesure and  and then 
#' uses the choice_c methods of these object to select the data.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("choice_c",signature=signature("BilanConditionEnv"),definition=function(object,stationMesure,datedebut,datefin,silent=FALSE){
			# code for debug using bM_Arzal example
			#stationmesure=c("temp_gabion","coef_maree");datedebut="2008-01-01";datefin="2008-12-31";silent=FALSE
			bil_CE<-object
			bil_CE@stationMesure=charge(bil_CE@stationMesure)
			# loads and verifies the stationmesure (selects the relevant lines in the table
			bil_CE@stationMesure<-choice_c(object=bil_CE@stationMesure,stationMesure)
			bil_CE@horodatedebut<-choice_c(object=bil_CE@horodatedebut,
					nomassign="bilanConditionEnv_date_debut",
					funoutlabel=gettext("Beginning date has been chosen\n",domain="R-stacomiR"),
					horodate=datedebut, 
					silent=silent)
			bil_CE@horodatefin<-choice_c(bil_CE@horodatefin,
					nomassign="bilanConditionEnv_date_fin",
					funoutlabel=gettext("Ending date has been chosen\n",domain="R-stacomiR"),
					horodate=datefin,
					silent=silent)
			return(bil_CE)
		})
#' charge method for BilanCondtionEnv class
#' @param object An object of class \link{BilanConditionEnv-class}
#' @param h A handler
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("charge",signature=signature("BilanConditionEnv"),definition=function(object,h) {
			
			if (exists("refStationMesure",envir_stacomi)) {
				object@stationMesure<-get("refStationMesure",envir_stacomi)
			} else {
				funout(gettext("You need to choose a monitoring station, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
			}     
			
			if (exists("bilanConditionEnv_date_debut",envir_stacomi)) {
				object@horodatedebut@horodate<-get("bilanConditionEnv_date_debut",envir_stacomi)
			} else {
				funout(gettext("You need to choose the starting date\n",domain="R-stacomiR"),arret=TRUE)
			}
			
			if (exists("bilanConditionEnv_date_fin",envir_stacomi))  {
				object@horodatefin@horodate<-get("bilanConditionEnv_date_fin",envir_stacomi)
			}else {
				funout(gettext("You need to choose the ending date\n",domain="R-stacomiR"),arret=TRUE)
			}      		
			return(object)
		})


#' hbilanConditionEnvgraph Internal method
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
hbilanConditionEnvgraph = function(h,...) 
{	
	bilanConditionEnv<-get("bilanConditionEnv",envir=envir_stacomi)
	bilanConditionEnv=charge(bilanConditionEnv)
	bilanConditionEnv=connect(bilanConditionEnv)
	plot(bilanConditionEnv)
}	
#' Plot method for BilanConditionEnv
#' @param x An object of class Bilan_carlot
#' @param silent Stops displaying the messages.
#' @param ... Additional arguments, see \code{plot}, \code{plot.default} and \code{par}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases plot.BilanConditionEnv plot.bilanConditionEnv plot.bilanconditionenv
#' @export
setMethod("plot", signature(x = "BilanConditionEnv", y = "missing"), definition=function(x,  silent=FALSE){ 
			# le dataframe contenant le res de la requete
			bil_CE<-x
			dat<-bil_CE@data	
			if(length(unique(dat$env_stm_identifiant))!=0){
				# le layout pour l'affichage des graphiques
				vplayout <- function(x, y) { grid::viewport(layout.pos.row = x, layout.pos.col = y)   }
				grid::grid.newpage()
				grid::pushViewport(grid::viewport(layout = grid::grid.layout(length(unique(dat$env_stm_identifiant)),1,just="center")))
				# la liste des graphes calcules
				lesGraphes=list()
				if(length(unique(dat$env_stm_identifiant))!= nrow(bil_CE@stationMesure@data))
				{
					funout(gettext("Some monitoring stations lack associated values (no environmental data)\n",domain="R-stacomiR"))
				}
				
				# for all stationmesure selected
				for (i in 1:length(unique(dat$env_stm_identifiant)))
				{
					# the identifier of the current station
					stmidentifiant <- unique(dat$env_stm_identifiant)[i]
					
					# the line of bilanConditionEnv@stationMesure currently processed in the loop
					stm <- bil_CE@stationMesure@data[bil_CE@stationMesure@data$stm_identifiant==stmidentifiant,]
					
					# all measures for the selected station
					nameColonne <- as.character(stm$stm_libelle)
					datstm <- stacomirtools::chnames(dat,"env_valeur_quantitatif", nameColonne) 
					datstm <- datstm[datstm$env_stm_identifiant==stmidentifiant,]
					
					# creating the plot
					g<-ggplot(datstm,aes_string(x="env_date_debut",y=nameColonne))  
					g<-g+geom_line(aes_string(colour=nameColonne))+scale_y_continuous(stm$stm_libelle)+
							scale_x_datetime(name="date")
					
					# printing plot on screen
					print(g, vp=vplayout(i,1))
				} 
			} else {
				funout(gettext("No environmental conditions values for selected monitoring stations (BilanConditionEnv.r)\n",domain="R-stacomiR"),arret=TRUE)
			}	
			
		})   
