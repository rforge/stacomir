UNE_SECONDE     = as.difftime(c("0:0:1")) ;
UNE_MINUTE      = 60 * UNE_SECONDE ;
DIX_MINUTES     = 10 * UNE_MINUTE ;
QUINZE_MINUTES  = 15 * UNE_MINUTE ;
TRENTE_MINUTES  = 30 * UNE_MINUTE ;
UNE_HEURE       = 60 * UNE_MINUTE ;
DOUZE_HEURES    = 12 * UNE_HEURE ;
UN_JOUR         = 24 * UNE_HEURE ;
UNE_SEMAINE     = 7 * UN_JOUR ;
DEUX_SEMAINES   = 2 * UNE_SEMAINE ;
UN_MOIS         = 30 * UN_JOUR ;
TROIS_MOIS      = 91 * UN_JOUR ;
SIX_MOIS        = 182 * UN_JOUR ;
UN_AN           = 365 * UN_JOUR ;

ValeurPasDeTemps=c(UNE_SECONDE,UNE_MINUTE,DIX_MINUTES,QUINZE_MINUTES,TRENTE_MINUTES,UNE_HEURE,DOUZE_HEURES,
		UN_JOUR,UNE_SEMAINE,DEUX_SEMAINES,UN_MOIS,TROIS_MOIS,SIX_MOIS,UN_AN)
LabelPasDeTemps=c(
		"1 sec",
		"1 min",
		"10 min" ,
		"15 min" ,
		"30 min",
		"1 h"   ,
		"12 h"  ,
		"1 jour"   ,
		"1 sem" ,
		"2 sem"  ,
		"1 mois" ,
		"3 mois" ,
		"6 mois" ,
		"1 an"   )
LesPasDeTemps=data.frame("ValeurPasDeTemps"=ValeurPasDeTemps)
LesPasDeTemps[,"LabelPasDeTemps"]=LabelPasDeTemps
rownames(LesPasDeTemps)=
		c("UNE_SECONDE","UNE_MINUTE","DIX_MINUTES","QUINZE_MINUTES","TRENTE_MINUTES","UNE_HEURE","DOUZE_HEURES",
				"UN_JOUR","UNE_SEMAINE","DEUX_SEMAINES","UN_MOIS","TROIS_MOIS","SIX_MOIS","UN_AN")
rm(UNE_SECONDE,UNE_MINUTE,DIX_MINUTES,QUINZE_MINUTES,TRENTE_MINUTES,UNE_HEURE,DOUZE_HEURES,
		UN_JOUR,UNE_SEMAINE,DEUX_SEMAINES,UN_MOIS,TROIS_MOIS,SIX_MOIS,UN_AN,LabelPasDeTemps)


validite_PasDeTemps=function(object)
{
  retValue=NULL
	rep1= class(object@dateDebut)[1]=="POSIXlt"
	if (!rep1) retValue="object@dateDebut is not of class POSIXlt"  
	rep2=length(object@stepDuration)==1
	if (!rep2) retValue=paste(retValue,"length(object@stepDuration) !=1") 
	rep3=length(object@nbStep)==1
	if (!rep3) retValue=paste(retValue,"length(object@nbStep) !=1") 
	rep4=length(object@noPasCourant)==1
	if (!rep4) retValue=paste(retValue,"length(object@noPasCourant) !=1")
	return(ifelse(rep1 & rep2 & rep3 & rep4,TRUE,retValue))
}

#' Class "PasDeTemps"
#' 
#' Describes a time step
#' 
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("PasDeTemps",
#' dateDebut="POSIXt",stepDuration=numeric(),nbStep=numeric(),noPasCourant=integer())}.
#' \describe{ 
#' \item{list("dateDebut")}{Object of class \code{"POSIXt"} Starting
#' date }
#' \item{:}{Object of class \code{"POSIXt"} Starting date }
#' \item{list("stepDuration")}{Object of class \code{"numeric"} Step length
#' }\item{:}{Object of class \code{"numeric"} Step length }
#' \item{list("nbStep")}{Object of class \code{"numeric"} Number of steps
#' }\item{:}{Object of class \code{"numeric"} Number of steps }
#' \item{list("noPasCourant")}{Object of class \code{"integer"} Number of the
#' current step }\item{:}{Object of class \code{"integer"} Number of the
#' current step } }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso \code{\linkS4class{PasDeTempsJournalier}}
#' @concept Bilan Object
setClass(Class="PasDeTemps",representation=
				representation(dateDebut="POSIXlt",stepDuration="numeric",nbStep="numeric",noPasCourant="integer"),
		validity=validite_PasDeTemps,
		prototype=prototype(dateDebut=as.POSIXlt(Hmisc::trunc.POSIXt(Sys.time(),"year")),
				stepDuration=as.numeric(86400),
				nbStep=as.numeric(1),
				noPasCourant=as.integer(0) ) )
# pasDeTemps= new("PasDeTemps")


validite_PasDeTempsChar=function(object)
{
	rep1= class(object@dateDebut)[1]=="POSIXlt"
	rep2=length(object@stepDuration)==1
	rep3=length(object@nbStep)==1
	rep4=length(object@noPasCourant)==1
	rep5= object@stepDuration%in%LesPasDeTemps[,"LabelPasDeTemps"]
	return(ifelse(rep1 & rep2 & rep3 & rep4 & rep5,TRUE,c(1:5)[!c(rep1, rep2, rep3, rep4,rep5)]))
}
#' Class "PasDeTempsChar"
#' 
#' Character to represent a PasDeTemps
#' 
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("PasDeTempsChar", \dots{})}
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso \code{\linkS4class{PasDeTemps}}
#' @keywords classes
#' @examples
#' 
#' showClass("PasDeTempsChar")
#' 
setClass(Class="PasDeTempsChar",representation=
				representation(dateDebut="POSIXlt",stepDuration="character",nbStep="numeric",noPasCourant="integer"),
		validity=validite_PasDeTempsChar,
		prototype=prototype(dateDebut=as.POSIXlt(strptime("2008-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S"),tz="GMT"),
				stepDuration=as.character("1 jour"),
				nbStep=as.numeric(1),
				noPasCourant=as.integer(0) ))

setAs("PasDeTempsChar","PasDeTemps",   # from to
		function(from,to){
			index=LesPasDeTemps[,"LabelPasDeTemps"]%in%from@stepDuration
			newstepDuration=LesPasDeTemps[index,"ValeurPasDeTemps"]
			new("PasDeTemps",dateDebut=from@dateDebut,
					stepDuration=newstepDuration,
					nbStep=from@nbStep,
					noPasCourant=from@noPasCourant)})
# pasDeTemps=as(pasDeTempsChar,"PasDeTemps")

#' Generic method to get current time step
#' @param object An object
#' @param ... Additional parameters passed to the method
setGeneric("getnoPasCourant",def=function(object,...) standardGeneric("getnoPasCourant"))
#' Gets the current time step of an object of class \link{PasDeTemps-class}
#' @param object An object of class \link{PasDeTemps-class}
#' @return the current time step of the object
#' @keywords internal
setMethod("getnoPasCourant",signature=signature("PasDeTemps"),definition=function(object) object@noPasCourant)

#' Generic method for getting the final date
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric("DateFin",def=function(object,...) standardGeneric("DateFin"))
#' Gets the final horodate for an object of class \link{PasDeTemps-class}
#' @param object An object of class \link{PasDeTemps-class}
#' @return DateFin, The final date corresponding to nbStep*time duration + initial date
#' @export
#' @keywords internal
setMethod("DateFin",signature=signature("PasDeTemps"),definition=function(object){
			DateFin=object@dateDebut+ object@stepDuration*(object@nbStep)
			# pour les pb de changement d'heure
			
			return(DateFin)
		})

#' Generic method for getting the beginning date for current time step
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric("currentDateDebut",def=function(object,...) standardGeneric("currentDateDebut"))
#' Gets the starting date of a time step for an object of class \link{PasDeTemps-class}
#' @param object An object of class \link{PasDeTemps-class}
#' @return CurrentDateDebut, The starting date for the current timestep
#' @keywords internal
setMethod("currentDateDebut",signature=signature("PasDeTemps"),definition=function(object){
			CurrentDateDebut=object@dateDebut+ object@stepDuration*object@noPasCourant
			# bug cht heure
			if (object@stepDuration==86400) {
				CurrentDateDebut=Hmisc::round.POSIXt(CurrentDateDebut,"days")
			}			
			return(CurrentDateDebut)
		})

#' Generic method for getting the ending date for current time step
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric("currentDateFin",def=function(object,...) standardGeneric("currentDateFin"))
#' Gets the ending date of a time step for an object of class \link{PasDeTemps-class}
#' @param object An object of class \link{PasDeTemps-class}
#' @return CurrentDateFin, The ending date for the current timestep
setMethod("currentDateFin",signature=signature("PasDeTemps"),definition=function(object){
			CurrentDateFin=object@dateDebut+ object@stepDuration*(object@noPasCourant+as.integer(1))
			if (object@stepDuration==86400) {
				CurrentDateFin=Hmisc::round.POSIXt(CurrentDateFin,"days")
			}
			return(CurrentDateFin)
		})

#' Generic method next
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric("suivant",def=function(object,...) standardGeneric("suivant"))
#' Gets the next time step 
#' @param object An object of class \link{PasDeTemps-class}
#' @return An object of class \link{PasDeTemps-class} with current time step set
#' @keywords internal
setMethod("suivant",signature=signature("PasDeTemps"),definition=function(object){
			object@noPasCourant =object@noPasCourant+as.integer(1)
			if (currentDateFin(object)>DateFin(object)) {
				object@noPasCourant = as.integer(-1)
			}
			return (object)
		})

#' Generic method the get starting date
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric("getdateDebut",def=function(object,...) standardGeneric("getdateDebut"))
#' Returns the starting date as character
#' @param object An object of class \link{PasDeTemps-class}
#' @param ... Additional parameters passed to the method
#' @keywords internal
setMethod("getdateDebut",signature=signature("PasDeTemps"),definition=function(object){
			return ( strftime(as.POSIXlt(object@dateDebut),format="%Y-%m-%d %H:%M:%S") )
		})


#' Generic method to set the starting date
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric("setdateDebut",def=function(object,...) standardGeneric("setdateDebut"))
#' Sets starting date from a character
#' 
#' 
#' @param object An object of class \link{PasDeTemps-class}
#' @param string Character string of type"\%Y-\%m-\%d \%H:\%M:\%S" or "\%Y-\%m-\%d".
#' this allows to use either horodate or date
#' @return An object of class \link{PasDeTemps-class}
#' @keywords internal
setMethod("setdateDebut",signature=signature("PasDeTemps"),definition=function(object,string){
			object@dateDebut=if (!is.na(strptime(string,format="%Y-%m-%d %H:%M:%S"))) strptime(string,format="%Y-%m-%d %H:%M:%S") else
      strptime(string,format="%Y-%m-%d") 
			return(object) 
		})

#' Generic method to get the string value of time step
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric("getLibellesPas",def=function(object,...) standardGeneric("getLibellesPas"))



#' Gets the string value of time step
#' 
#' @param object An object of class \link{PasDeTemps-class}
#' @return A string corresponding to the value of current time step
#' @keywords internal
setMethod("getLibellesPas",signature=signature("PasDeTemps"),definition=function(object){
			ret=paste(LesPasDeTemps$LabelPasDeTemps)
			return (ret )
		})

#' Generic method to get the years 
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric("getAnnees",def=function(object,...) standardGeneric("getAnnees"))

#' Gets the year or a vector of years corresponding to the timestep ("PasDeTemps") object
#' @param object An object of class \link{PasDeTemps-class}
#' @keywords internal
setMethod("getAnnees",signature=signature("PasDeTemps"),definition=function(object){
			 dateFin=DateFin(object)
			 dateDebut=object@dateDebut
			 seq=seq.POSIXt(from=dateDebut,to=dateFin,by="day")
			 seq=seq[-length(seq)]
			 annees=unique(strftime(seq,"%Y"))
  		return (as.numeric(annees))
		})

#' Method to select timesteps from the graphical interface
#' @param object An object of class \link{PasDeTemps-class}
#' @keywords internal
setMethod("choice",signature=signature("PasDeTemps"),definition=function(object) {
			if (length(LesPasDeTemps$LabelPasDeTemps) > 0){
				hwinpa=function(h,...){
					pas=svalue(choicepas)
					nbStep=as.numeric(svalue(choicenbStep)) 
					object@nbStep<-nbStep
					object@stepDuration<-as.numeric(LesPasDeTemps$ValeurPasDeTemps[LesPasDeTemps$LabelPasDeTemps%in%pas])
					object=setdateDebut(object,svalue(datedeb))
					assign("pasDeTemps",object,envir_stacomi)					
				}
				hchoicepas=function(h,...){
					pas=svalue(choicepas)
					nbStep=as.numeric(svalue(choicenbStep))
					object@stepDuration<-as.numeric(LesPasDeTemps$ValeurPasDeTemps[LesPasDeTemps$LabelPasDeTemps%in%pas])
					object@nbStep<-nbStep 
					object=setdateDebut(object,svalue(datedeb))
					add(datedefin,strftime(as.POSIXlt(DateFin(object)),format="%Y-%m-%d %H:%M:%S"),
							font.attr=c(foreground.colors="red") )
					hwinpa(h)
				}
				group<-get("group",envir=envir_stacomi)
				winpa=gframe(gettext("Time steps choice",domain="R-stacomiR"),container=group,horizontal=FALSE)
				pg<-ggroup(horizontal=FALSE,container=winpa)
				glabel(gettext("Starting date",domain="R-stacomiR"),container=pg)
				datedeb<-gedit(getdateDebut(object),
						container=pg,handler=hchoicepas,width=15)
				datedebut2=as.character(strftime(object@dateDebut,"%Y-%m-%d"))
				datedeb2<-gcalendar(datedebut2,container=pg,handler=function(h,...){
							svalue(datedeb)<-as.character(strftime(
											strptime(svalue(datedeb2),"%Y-%m-%d"),
											"%Y-%m-%d %H:%M:%S"))
							hchoicepas(h)				
						} )
				glabel(gettext("Time steps choice",domain="R-stacomiR"),container=winpa)
				pas_libelle=fun_char_spe(LesPasDeTemps$LabelPasDeTemps)
				choicepas=gdroplist(pas_libelle,selected = 8,container=winpa,handler=hchoicepas) 
				glabel(gettext("Number of time step choice",domain="R-stacomiR"),container=winpa)
				choicenbStep=gedit("365",container=winpa,coerce.with=as.numeric,handler=hchoicepas,width=15)
				datedefin<-gtext(gettext("End date",domain="R-stacomiR"),height=50,container=winpa) # Date de fin
				gbutton("OK", container=winpa,handler=hwinpa,icon="execute")
			} else funout(gettext("Internal error : no entry in time steps table\n",domain="R-stacomiR"), arret=TRUE)
		})

		
#' Graphical interface for multiple choice method for PasdeTemps (used in BilanMigrationMult)
#' @param object An object of class \link{PasDeTemps-class}
#' @note this method differs from choice as it is called within a notebook,
#' it does not allow for multiple choice to be made
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
		setMethod("choicemult",signature=signature("PasDeTemps"),definition=function(object) {
					if (length(LesPasDeTemps$LabelPasDeTemps) > 0){
						hwinpa=function(h,...){
							pas=svalue(choicepas)
							nbStep=as.numeric(svalue(choicenbStep)) 
							object@nbStep<<-nbStep
							object@stepDuration<<-as.numeric(LesPasDeTemps$ValeurPasDeTemps[LesPasDeTemps$LabelPasDeTemps%in%pas])
							object=setdateDebut(object,svalue(datedeb))						
							assign("pasDeTemps",object,envir_stacomi)
							funout(gettext("Timesteps loaded\n",domain="R-stacomiR"))
							# charge le deuxieme onglet du notebook
							svalue(notebook)<-2
						}
						hchoicepas=function(h,...){
							#browser()
							pas=svalue(choicepas)
							nbStep=as.numeric(svalue(choicenbStep))
							object@stepDuration<-as.numeric(LesPasDeTemps$ValeurPasDeTemps[LesPasDeTemps$LabelPasDeTemps%in%pas])
							object@nbStep<-nbStep 
							object=setdateDebut(object,svalue(datedeb))  
							add(datedefin,strftime(as.POSIXlt(DateFin(object)),format="%Y-%m-%d %H:%M:%S"),
									font.attr=c(foreground.colors="red") )
							hwinpa(h)
						}
						hchoicedatedebut=function(h,...){
							# TODO to develop
						}
						notebook<-get("notebook",envir=envir_stacomi)
						groupdate<-ggroup(container=notebook, label="periode")   ## "add" called by constructor this is a tab of the notebook
						assign("groupdate",groupdate,envir=envir_stacomi)
						winpa=gframe(gettext("Time steps choice",domain="R-stacomiR"),container=groupdate,horizontal=FALSE)
						pg<-ggroup(horizontal=FALSE,container=winpa)
						glabel(gettext("Starting date",domain="R-stacomiR"),container=pg)
						datedeb<-gedit(getdateDebut(object),container=pg,handler=hchoicepas,width=15)
						datedebut2=as.character(strftime(object@dateDebut,"%Y-%m-%d"))
						datedeb2<-gcalendar(datedebut2,container=pg,handler=function(h,...){
									svalue(datedeb)<-as.character(strftime(
													strptime(svalue(datedeb2),"%Y-%m-%d"),
													"%Y-%m-%d %H:%M:%S"))
									hchoicepas(h)				
								} )
						glabel(gettext("Time steps choice",domain="R-stacomiR"),container=winpa)
						pas_libelle=fun_char_spe(LesPasDeTemps$LabelPasDeTemps)
						choicepas=gdroplist(pas_libelle,selected = 8,container=winpa,handler=hchoicepas) 
						glabel(gettext("Number of time steps choice",domain="R-stacomiR"),container=winpa)
						choicenbStep=gedit("365",container=winpa,coerce.with=as.numeric,handler=hchoicepas,width=15)
						datedefin<-gtext(gettext("Ending date",domain="R-stacomiR"),height=50,container=winpa) # Date de fin)
						gbutton("OK", container=winpa,handler=hwinpa,icon="execute")
					} else funout(gettext("Internal error : no entry in time steps table\n",domain="R-stacomiR"), arret=TRUE)
				})
		


