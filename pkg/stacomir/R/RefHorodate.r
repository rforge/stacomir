#' Validity check for RefHorodate
#' 
#' @param object A refHorodate object
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
validite_RefHorodate=function(object)
{
	rep1= class(object@dateDebut)[1]=="POSIXt"
	
	return(ifelse(rep1,TRUE,FALSE))
}


#' Class RefHorodate
#' 
#' choice of date with method to show current and previous year
#' 
#' 
#' @slot horodate a "POSIXt"
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefHorodate", \dots{})}.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @seealso Other referential classes \code{\linkS4class{RefAnnee}}
#' \code{\linkS4class{RefCheckBox}} \code{\linkS4class{RefChoix}}
#' \code{\linkS4class{RefCoe}} \code{\linkS4class{RefDC}}
#' \code{\linkS4class{RefDF}} \code{\linkS4class{RefListe}}
#' \code{\linkS4class{Refpar}} \code{\linkS4class{Refparqual}}
#' \code{\linkS4class{Refparquan}} \code{\linkS4class{RefPoidsMoyenPeche}}
#' \code{\linkS4class{RefStades}} \code{\linkS4class{RefStationMesure}}
#' \code{\linkS4class{RefTaxon}}
#' @family Referential objects
setClass(Class="RefHorodate",representation=
				representation(horodate="POSIXt"),
		validity=validite_RefHorodate,
		prototype=prototype(horodate=Hmisc::round.POSIXt(Sys.time(),"years")))
# date= new("Horodate")
#retourne la date en format character
setGeneric("getRefHorodate",def=function(object,...) standardGeneric("getRefHorodate"))
setMethod("getRefHorodate",signature=signature("RefHorodate"),definition=function(object){
			return ( strftime(as.POSIXlt(object@horodate),format="%Y-%m-%d %H:%M:%S") )
		})

#Fixe la date de debut e partir d'un champ charactere de type "%Y-%m-%d %H:%M:%S"
setGeneric("setRefHorodate",def=function(object,...) standardGeneric("setRefHorodate"))




#' Method to set the Horodate
#' 
#' @return An Object of class "RefHorodate" 
#' @author cedric.briand
#' @docType methods
#' @export
setMethod("setRefHorodate",signature=signature("RefHorodate"),definition=function(object,string){
			object@horodate=strptime(string,format="%Y-%m-%d %H:%M:%S")
			return(object) 
		})
# retourne l'annee d'avant l'annee en cours
setGeneric("getanneeprec",def=function(object,...) standardGeneric("getanneeprec"))
setMethod("getanneeprec",signature=signature("RefHorodate"),definition=function(object,decal){
			anneeprec=as.numeric(strftime(object@horodate,"%Y"))+decal
			object@horodate<-strptime(paste(anneeprec,"-01-01",sep=""),format="%Y-%m-%d")
			return (object)
		})


#' choice method for RefHorodate
#' 
#' @return Selects the date in the graphical interface
#' 
#' @author cedric.briand
#' @docType methods
setMethod("choice",signature=signature("RefHorodate"),definition=function(object,label="date",nomassign="horodate",funoutlabel="nous avons le choice dans la date\n",decal=0,affichecal=TRUE) {
			hwinhor=function(h,...){
				object=setRefHorodate(object,svalue(horodate))
				if (affichecal){
			    # bug dans horocal
				#	svalue(horocal)<-as.character(strftime(object@horodate,"%Y-%m-%d"))
				}
				assign(nomassign,object,envir_stacomi)
				funout(funoutlabel)
				#print(object)
				#dispose(winpa)
			}
			if (decal!=0){
				object<-getanneeprec(object,decal)
			}
			winhor=gframe(label,container=group,horizontal=!affichecal)
			pg<-ggroup(horizontal=FALSE,container=winhor)
			horodate<-gedit(getRefHorodate(object),container=pg,handler=hwinhor,width=20)
			horodate2=as.character(strftime(object@horodate,"%Y-%m-%d"))
			if (affichecal) {
#				horocal<-gcalendar(horodate2,container=pg,handler=function(h,...){
#							svalue(horodate)<-as.character(strftime(
#											strptime(svalue(horocal),"%Y-%m-%d"),
#											"%Y-%m-%d %H:%M:%S"))
#						} ) 
			}
			gbutton("OK", container=winhor,handler=hwinhor,icon="execute")
		})

# showClass("PasDeTemps")
# validObject( pasDeTemps)
# showMethods("suivant")

