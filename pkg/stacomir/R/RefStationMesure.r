# Nom fichier :        RefStationMesure   (classe)
# Date de creation :   02/01/2009 15:02:40

#' Class "RefStationMesure"
#' 
#' Enables to load measure stations and to select one of them
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefStationMesure", ...)}. 
#' @slot dataframe Data concerning the
#' measure station
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso Other referential classes 
#' \code{\linkS4class{RefAnnee}}
#' \code{\linkS4class{RefCheckBox}} 
#' \code{\linkS4class{RefChoix}}
#' \code{\linkS4class{RefCoe}} 
#' \code{\linkS4class{RefDC}}
#' \code{\linkS4class{RefDF}} 
#' \code{\linkS4class{RefListe}}
#' \code{\linkS4class{Refpar}} 
#' \code{\linkS4class{Refparqual}}
#' \code{\linkS4class{Refparquan}} 
#' \code{\linkS4class{RefPoidsMoyenPeche}}
#' \code{\linkS4class{RefStades}} 
#' \code{\linkS4class{RefStationMesure}}
#' \code{\linkS4class{RefTaxon}}
#' @keywords classes
#' @examples
#' 
#' showClass("RefStationMesure")
setClass (Class="RefStationMesure", 
   representation=representation(data="data.frame"),
   prototype=prototype(data=data.frame())
)

#' Loading method for RefStationMesure referential object
#' @return An S4 object of class RefStationMesure
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#'  object=new("RefStationMesure")
#'  charge(object)
setMethod("charge",
          signature=signature("RefStationMesure"),     
          definition=function(object) 
          {
        			requete=new("RequeteODBC")
        			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
        			requete@sql= paste("SELECT stm_identifiant, stm_libelle, stm_sta_code, stm_par_code, stm_description",
                    " FROM ",get("sch",envir=envir_stacomi),"tj_stationmesure_stm", 
					" ORDER BY stm_identifiant;",sep="")
        			requete@silent = TRUE;
        			requete<-stacomirtools::connect(requete)    
        			object@data<-requete@query
        			return(object)
          }
)
#' Choice method for RefStationMesure referential object
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples  
#' object=new("RefStationMesure")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object<-charge(object)
#' choice(object)
setMethod("choice",signature=signature("RefStationMesure"),definition=function(object,is.enabled=TRUE,title=get("msg",envir=envir_stacomi)$RefStationMesure.3) {
			if (nrow(object@data) > 0){
				hSTM=function(h,...){
          stationMesure=svalue(choice,index=TRUE)
          if(length(stationMesure)==0)
          {
             funout(get("msg",envir=envir_stacomi)$RefStationMesure.1,arret=TRUE)
          }
          else
          {
            object@data<-object@data[stationMesure,]
            assign("refStationMesure",object,envir_stacomi)
  					funout(get("msg",envir=envir_stacomi)$RefStationMesure.2)
          }
				}
				frame_stationMesure<<-gexpandgroup(title)
				add(group,frame_stationMesure)
				stm_libelle=fun_char_spe(object@data$stm_libelle)
				choice=gcheckboxgroup(stm_libelle,container=frame_stationMesure)
				enabled(frame_stationMesure)<-is.enabled
				gbutton("OK", container=frame_stationMesure,handler=hSTM)
			} 
      else funout(get("msg",envir=envir_stacomi)$RefStationMesure.4,arret=TRUE)
		})
