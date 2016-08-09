# Nom fichier :        RefHorodate (classe)

#fonction pour valider les objets de classe Horodate
validite_RefHorodate=function(object)
{
	rep1= class(object@dateDebut)[1]=="POSIXt"
	
	return(ifelse(rep1,TRUE,FALSE))
}


#' Class "RefHorodate"
#' 
#' choice of date with method to show current and previous year
#' 
#' 
#' @name RefHorodate-class
#' @aliases RefHorodate-class RefHorodate validite_RefHorodate
#' getanneeprec,RefHorodate-method getanneeprec
#' getRefHorodate,RefHorodate-method getRefHorodate
#' setRefHorodate,RefHorodate-method setRefHorodate

#'@slot horodate="POSIXt"
#'@method getRefHorodate
#'@method setRefHorodate
#'@method getanneeprec
#'@method choix
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
#' @keywords classes
#' @examples
#' @family Referential objects
#' 
#' showClass("RefHorodate")
#' 
setClass(Class="RefHorodate",representation=
				representation(horodate="POSIXt"),
		validity=validite_RefHorodate,
		prototype=prototype(horodate=Hmisc::round.POSIXt(Sys.time(),"years")))
# date= new("Horodate")
#retourne la date en format character
setGeneric("getRefHorodate",def=function(objet,...) standardGeneric("getRefHorodate"))
setMethod("getRefHorodate",signature=signature("RefHorodate"),definition=function(objet){
			return ( strftime(as.POSIXlt(objet@horodate),format="%Y-%m-%d %H:%M:%S") )
		})

#Fixe la date de debut à partir d'un champ charactere de type "%Y-%m-%d %H:%M:%S"
setGeneric("setRefHorodate",def=function(objet,...) standardGeneric("setRefHorodate"))
setMethod("setRefHorodate",signature=signature("RefHorodate"),definition=function(objet,string){
			objet@horodate=strptime(string,format="%Y-%m-%d %H:%M:%S")
			return(objet) 
		})
# retourne l'annee d'avant l'annee en cours
setGeneric("getanneeprec",def=function(objet,...) standardGeneric("getanneeprec"))
setMethod("getanneeprec",signature=signature("RefHorodate"),definition=function(objet,decal){
			anneeprec=as.numeric(strftime(objet@horodate,"%Y"))+decal
			objet@horodate<-strptime(paste(anneeprec,"-01-01",sep=""),format="%Y-%m-%d")
			return (objet)
		})


setMethod("choix",signature=signature("RefHorodate"),definition=function(objet,label="date",nomassign="horodate",funoutlabel="nous avons le choix dans la date\n",decal=0,affichecal=TRUE) {
			hwinhor=function(h,...){
				objet=setRefHorodate(objet,svalue(horodate))
				if (affichecal){
			    # bug dans horocal
				#	svalue(horocal)<-as.character(strftime(objet@horodate,"%Y-%m-%d"))
				}
				assign(nomassign,objet,envir_stacomi)
				funout(funoutlabel)
				#print(objet)
				#dispose(winpa)
			}
			if (decal!=0){
				objet<-getanneeprec(objet,decal)
			}
			winhor=gframe(label,container=group,horizontal=!affichecal)
			pg<-ggroup(horizontal=FALSE,cont=winhor)
			horodate<-gedit(getRefHorodate(objet),cont=pg,handler=hwinhor,width=20)
			horodate2=as.character(strftime(objet@horodate,"%Y-%m-%d"))
			if (affichecal) {
#				horocal<-gcalendar(horodate2,cont=pg,handler=function(h,...){
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

