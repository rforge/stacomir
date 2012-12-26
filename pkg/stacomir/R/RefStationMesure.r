# Nom fichier :        RefStationMesure   (classe)
# Date de creation :   02/01/2009 15:02:40

#' @title Refstades referential class to load the measures stations
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @slot data="data.frame"
#' @expamples objet=new("RefStationMesure")
setClass (  Class="RefStationMesure", 
   representation=representation(data="data.frame"),
   prototype=prototype(data=data.frame())
)

#' Loading method for RefStationMesure referential object
#' @returnType S4 object
#' @return An S4 object of class RefStationMesure
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @expamples 
#'  objet=new("RefStationMesure")
#'  charge(objet)
setMethod("charge",
          signature=signature("RefStationMesure"),     
          definition=function(objet) 
          {
        			requete=new("RequeteODBC")
        			objet@baseODBC<-get("baseODBC",envir=envir_stacomi)
        			requete@sql= paste("SELECT stm_identifiant, stm_libelle, stm_sta_code, stm_par_code, stm_description",
                    " FROM ",get("sch",envir=envir_stacomi),"tj_stationmesure_stm", 
					" ORDER BY stm_identifiant;",sep="")
        			requete@silent = TRUE;
        			requete<-connect(requete)    
        			objet@data<-requete@query
        			return(objet)
          }
)
#' Choice method for RefStationMesure referential object
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @expamples  
#' objet=new("RefStationMesure")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' objet<-charge(objet)
#' choix(objet)
setMethod("choix",signature=signature("RefStationMesure"),definition=function(objet,is.enabled=TRUE,title=get("msg",envir=envir_stacomi)$RefStationMesure.3) {
			if (nrow(objet@data) > 0){
				hSTM=function(h,...){
          stationMesure=svalue(choix,index=TRUE)
          if(length(stationMesure)==0)
          {
             funout(get("msg",envir=envir_stacomi)$RefStationMesure.1,arret=TRUE)
          }
          else
          {
            objet@data<-objet@data[stationMesure,]
            assign("refStationMesure",objet,envir_stacomi)
  					funout(get("msg",envir=envir_stacomi)$RefStationMesure.2)
          }
				}
				frame_stationMesure<<-gexpandgroup(title)
				add(group,frame_stationMesure)
				stm_libelle=fun_char_spe(objet@data$stm_libelle)
				choix=gcheckboxgroup(stm_libelle,container=frame_stationMesure)
				enabled(frame_stationMesure)<-is.enabled
				gbutton("OK", container=frame_stationMesure,handler=hSTM)
			} 
      else funout(get("msg",envir=envir_stacomi)$RefStationMesure.4,arret=TRUE)
		})
