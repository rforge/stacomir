# Nom fichier :        PasdeTemps (classe)
# Projet :             controle migrateur
# Organisme :          IAV
# Auteur :             Cedric Briand
# Contact :            cedric.briand"at"eptb-vilaine.fr
# Date de creation :  31/03/2008 17:21:25
# Compatibilite :
# Etat :               developpement
# Description          calcul et affichage des pas de temps (classe object)
#**********************************************************************
#*

################################################################
# Declarations de classe
################################################################


#' Class "PasDeTempsJournalier"
#' 
#' Representation of a PasDeTemps object with a step length equal to one day.
#' It receives an heritance from PasDeTemps
#' 
#' validite_PasDeTempsJournalier
#' @include PasdeTemps.r
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("PasDeTempsJournalier",
#' dateDebut="POSIXt",dureePas=numeric(),nbPas=numeric(),noPasCourant=integer())}.
#' \describe{ \item{list("dateDebut")}{Object of class \code{"POSIXt"} Starting
#' date }\item{:}{Object of class \code{"POSIXt"} Starting date }
#' \item{list("dureePas")}{Object of class \code{"numeric"} Step length
#' }\item{:}{Object of class \code{"numeric"} Step length }
#' \item{list("nbPas")}{Object of class \code{"numeric"} Number of steps
#' }\item{:}{Object of class \code{"numeric"} Number of steps }
#' \item{list("noPasCourant")}{Object of class \code{"integer"} Number of the
#' current step }\item{:}{Object of class \code{"integer"} Number of the
#' current step } }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso \code{\linkS4class{PasDeTemps}}
#' @keywords classes
#' @examples
#' 
#' showClass("PasDeTempsJournalier")
#' 
setClass(Class="PasDeTempsJournalier",contains="PasDeTemps",
		prototype=(dureePas=86400) 
)



setValidity(Class="PasDeTempsJournalier",function(object)
		{
			retValue<-NULL
			rep1 = validite_PasDeTemps(object)
			if (!is.logical(rep1)) retValue<-rep1
			rep2 = (object@dureePas==86400)
			if (!rep2) retValue=paste(retValue,get("msg",envir=envir_stacomi)$PasdeTempsJournalier.1)
			rep3 = length(getAnnees(object))==1
			if (!rep3) retValue=paste(retValue,get("msg",envir=envir_stacomi)$PasdeTempsJournalier.2)
			return(ifelse( rep1 & rep2 & rep3 ,TRUE,retValue)   )
		})	
# pour test #object=new("PasDeTempsJournalier")
setMethod("choix",signature=signature("PasDeTempsJournalier"),definition=function(object) {
			if (length(LesPasDeTemps$LabelPasDeTemps) > 0){
				hwinpa=function(h,...){
					pas=svalue(choixpas)
					nbpas=as.numeric(svalue(choixnbpas)) 
					object@nbPas<-nbpas
					object@dureePas<-as.numeric(LesPasDeTemps$ValeurPasDeTemps[LesPasDeTemps$LabelPasDeTemps%in%pas])
					object=setdateDebut(object,as.POSIXlt(svalue(datedeb)))
					svalue(datedefin)<-as.Date(DateFin(object))
					assign("pasDeTemps",object,envir_stacomi)
					funout(get("msg",envir=envir_stacomi)$PasdeTempsJournalier.8)
					#dispose(winpa)
				}
				winpa=gframe(get("msg",envir=envir_stacomi)$PasdeTempsJournalier.3,container=group,horizontal=FALSE)
				pg<-glayout(cont=winpa)
				pg[1,1]<-glabel(get("msg",envir=envir_stacomi)$PasdeTempsJournalier.4)
				datedeb<-gedit(as.Date(getdateDebut(object)),handler=hwinpa,width=10)
				pg[2,1]<-datedeb
				pg[3,1]<-glabel(get("msg",envir=envir_stacomi)$PasdeTempsJournalier.5)
				pas_libelle=fun_char_spe(LesPasDeTemps$LabelPasDeTemps)
				choixpas=gdroplist(pas_libelle,selected = 8,handler=hwinpa)
				pg[4,1]<-choixpas 
				enabled(choixpas)=FALSE
				pg[3,2]<-glabel(get("msg",envir=envir_stacomi)$PasdeTempsJournalier.6)
				choixnbpas=gedit("365",coerce.with=as.numeric,handler=hwinpa,width=5)
				pg[4,2]<-choixnbpas
				pg[1,2]<-glabel(get("msg",envir=envir_stacomi)$PasdeTempsJournalier.7,container=pg)
				datedefin<-gedit("...",width=10) # heigth=30
				enabled(datedefin)<-FALSE
				pg[2,2]<-datedefin			
				pg[3,4:4]<-	gbutton("OK", handler=hwinpa,icon="execute")
			} else stop("internal error length(LesPasDeTemps$LabelPasDeTemps) == 0")
		})

# showClass("PasDeTemps")
# validObject( pasDeTemps)
# showMethods("suivant")


#' load method for class PasDeTempsJournalier
#' 
#' the load method is intented to have the same behaviour as choix (which creates a
#' widget in the graphical interface) but from the command line.  
#' @param datedebut a character (format \code{"15/01/1996"} or \code{"1996-01-15"} or \code{"15-01-1996"}), or POSIXct object
#' @param datefin a character of POSIXct object
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples
#' \dontrun{
#'object=new("RefDC")
#'object<-charge(object)
#' load(object=object,datedebut="2012-01-01",datefin="2013-01-01")}
setMethod("load",signature=signature("PasDeTempsJournalier"),definition=function(object,datedebut,datefin) {
			if (class(datedebut)=="character") {
				if (grepl("/",datedebut)){
					datedebut=strptime(datedebut, format="%d/%m/%Y")
					if (is.na(datedebut)){
						datedebut=strptime(datedebut, format="%d/%m/%y")				
					}
				} else if (grepl("-",datedebut)){
					datedebut=strptime(datedebut, format="%Y-%m-%d")
					if (is.na(datedebut)){
						datedebut=strptime(datedebut, format="%d-%m-%Y")				
					}
				}
				if (is.na(datedebut)){
					stop ("datedebut not parsed to datetime try format like '01/01/2017'")
				}
			}
				
				# the datedebut can have a POSIXct format
				if (class(datefin)=="character") {
					if (grepl("/",datefin)){
						datefin=strptime(datefin, format="%d/%m/%Y")
						if (is.na(datefin)){
							datefin=strptime(datefin, format="%d/%m/%y")				
						}
					} else if (grepl("-",datefin)){
						datefin=strptime(datefin, format="%Y-%m-%d")
						if (is.na(datefin)){
							datefin=strptime(datefin, format="%d-%m-%Y")				
						}
					}
					if (is.na(datefin)){
						stop ("datefin not parsed to datetime try format like '01/01/2017'")
					}	
				}
					object@dateDebut<-as.POSIXlt(datedebut)
					object@nbPas=as.numeric(difftime(datefin,datedebut,unit="days"))
					validObject(object) 			
					return(object)
				})
