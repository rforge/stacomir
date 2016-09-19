#' Validity check for RefHorodate
#' 
#' @param object A refHorodate object
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
validite_RefHorodate=function(object)
{
	rep1= class(object@horodate)[2]=="POSIXt"
	
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
#' @param object An object of class \link{RefHorodate-class}
#' @param string A string representing an horodate in the format "\%Y-\%m-\%d \%H:\%M:\%S"
#' @return An Object of class "RefHorodate" 
#' @author cedric.briand
setMethod("setRefHorodate",signature=signature("RefHorodate"),definition=function(object,string){
			object@horodate=strptime(string,format="%Y-%m-%d %H:%M:%S")
			return(object) 
		})
# retourne l'annee d'avant l'annee en cours






#' Graphical interface
#' @param object An object of class \link{RefHorodate-class}
#' @param label Label for the gframe
#' @param nomassing The name assigned in environment envir_stacomi
#' @param funoutlabel, text displayed by the interface
#' @param decale Default 0, number of years to shift forward or backward 
#' @return Selects the date in the graphical interface
setMethod("choice",signature=signature("RefHorodate"),definition=function(object,label="date",
				nomassign="horodate",
				funoutlabel="nous avons le choix dans la date\n",
				decal=0) {
			hwinhor=function(h,...){
				object=setRefHorodate(object,svalue(horodate))
				assign(nomassign,object,envir_stacomi)
				funout(funoutlabel)
				#print(object)
				#dispose(winpa)
			}
			if (decal!=0){
				# Returns the first horodate of a year shifted by decal
				# @param horodate The horodate to shift (class POSIXt)
				# @param decal number of year to shift
				# @return A POSIXt
				shiftyear<-function(horodate,decal){
					anneeprec=as.numeric(strftime(horodate,"%Y"))+decal
					return(strptime(paste(anneeprec,"-01-01",sep=""),format="%Y-%m-%d"))
				}
				object@horodate<-shiftyear(object@horodate,decal)
			}
			winhor=gframe(label,container=group,horizontal=FALSE)
			pg<-ggroup(horizontal=FALSE,container=winhor)
			horodate<-gedit(getRefHorodate(object),container=pg,handler=hwinhor,width=20)
			horodate2=as.character(strftime(object@horodate,"%Y-%m-%d"))
			gbutton("OK", container=winhor,handler=hwinhor,icon="execute")
		})



#' Command line
#' @param object An object of class \link{RefHorodate-class}
#' @param label Label for the gframe
#' @param nomassing The name assigned in environment envir_stacomi
#' @param funoutlabel, text displayed by the interface
#' @param affichecal Default TRUE, should the calendar be displayed
#' @param silent Default FALSE, should messages be displayed
#' @param horodate The horodate to set, formats "\%d/\%m/\%Y \%H:\%M:\%s", "\%d/\%m/\%y \%H:\%M:\%s", "\%Y-\%m-\%d  \%H:\%M:\%s" formats
#' can also be passed with the date set to the minute \%d/\%m/\%Y \%H:\%M or the day  \%d/\%m/\%Y
#' \dots are accepted
#' @return An object of class \link{RefHorodate-class} with slot \emph{horodate} set
setMethod("choice_c",signature=signature("RefHorodate"),definition=function(object,
				nomassign="horodate",
				funoutlabel="nous avons le choix dans la date\n",
				#decal=0,
				horodate,
				silent=FALSE
		) {
			# horodate="2013-01-01"
			# parse the horohorodate
			if (length(horodate)>1) stop("horodate should be a vector of length 1")
			if (is.null(horodate)) stop("horodate should not be null")
			if (class(horodate)=="character") {
				if (grepl("/",horodate)){
					.horodate=strptime(horodate, format="%d/%m/%Y %H:%M:%s")
					if (is.na(.horodate)){
						.horodate=strptime(horodate, format="%d/%m/%y %H:%M:%s")				
					}
					if (is.na(.horodate)){
						.horodate=strptime(horodate, format="%d/%m/%y %H:%M")				
					}
					if (is.na(.horodate)){
						.horodate=strptime(horodate, format="%d/%m/%Y %H:%M")				
					}
					if (is.na(.horodate)){
						.horodate=strptime(horodate, format="%d/%m/%y")				
					}
					if (is.na(.horodate)){
						.horodate=strptime(horodate, format="%d/%m/%Y")				
					}
				} else if (grepl("-",horodate)){
					.horodate=strptime(horodate, format="%Y-%m-%d  %H:%M:%s")
					if (is.na(.horodate)){
						.horodate=strptime(horodate, format="%d-%m-%Y  %H:%M:%s")				
					}
					if (is.na(.horodate)){
						.horodate=strptime(horodate, format="%Y-%m-%d  %H:%M")				
					}
					if (is.na(.horodate)){
						.horodate=strptime(horodate, format="%d-%m-%Y  %H:%M")				
					}
					if (is.na(.horodate)){
						.horodate=strptime(horodate, format="%Y-%m-%d")				
					}
					if (is.na(.horodate)){
						.horodate=strptime(horodate, format="%d-%m-%Y")				
					}
				}
		
			} else if (class(horodate)=="Date"){
				.horodate<-as.POSIXlt(horodate)
			} else if (class(horodate)[2]=="POSIXt"){
				.horodate=horodate
			}
			if (is.na(.horodate)) stop("Formatting problem, the character vector you are trying to pass as horodate could not
be parsed. Check example or documentation")
			object@horodate=.horodate	
			validObject(object)				
			assign(nomassign,object,envir_stacomi)
			funout(funoutlabel)	
			return(object)
		})

