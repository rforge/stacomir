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

################################################################
# Declarations de classe
################################################################
#fonction pour valider les objects de classe Pas deTemps
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
		prototype=prototype(dateDebut=as.POSIXlt(trunc.POSIXt(Sys.time(),"year")),
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
#pasDeTempsChar=new("PasDeTempsChar")
# pasDeTempsChar@stepDuration= "1 jour"
# pasDeTempsChar@nbStep=as.integer(10)


################################################################
# Conversion de classe
################################################################

#Conversion depuis la classe charactere  la classe numerique
setAs("PasDeTempsChar","PasDeTemps",   # from to
		function(from,to){
			index=LesPasDeTemps[,"LabelPasDeTemps"]%in%from@stepDuration
			newstepDuration=LesPasDeTemps[index,"ValeurPasDeTemps"]
			new("PasDeTemps",dateDebut=from@dateDebut,
					stepDuration=newstepDuration,
					nbStep=from@nbStep,
					noPasCourant=from@noPasCourant)})
# pasDeTemps=as(pasDeTempsChar,"PasDeTemps")

################################################################
# methodes standard
################################################################
# retourne le pas courant
setGeneric("getnoPasCourant",def=function(object,...) standardGeneric("getnoPasCourant"))
setMethod("getnoPasCourant",signature=signature("PasDeTemps"),definition=function(object) object@noPasCourant)

# retourne la date de fin
setGeneric("DateFin",def=function(object,...) standardGeneric("DateFin"))
setMethod("DateFin",signature=signature("PasDeTemps"),definition=function(object){
			DateFin=object@dateDebut+ object@stepDuration*(object@nbStep)
			# pour les pb de changement d'heure
			
			return(DateFin)
		})

#getnoPasCourant(EssaiPasdeTemps)


#Retourne la date de debut correspondant au no de pas courant
setGeneric("currentDateDebut",def=function(object,...) standardGeneric("currentDateDebut"))
setMethod("currentDateDebut",signature=signature("PasDeTemps"),definition=function(object){
			CurrentDateDebut=object@dateDebut+ object@stepDuration*object@noPasCourant
			# bug cht heure
			if (object@stepDuration==86400) {
				CurrentDateDebut=round.POSIXt(CurrentDateDebut,"days")
			}			
			return(CurrentDateDebut)
		})

# pasDeTemps@noPasCourant=as.integer(2)
# currentDateDebut(pasDeTemps)

# Retourne la date de fin correspondant au no de pas courant
setGeneric("currentDateFin",def=function(object,...) standardGeneric("currentDateFin"))
setMethod("currentDateFin",signature=signature("PasDeTemps"),definition=function(object){
			CurrentDateFin=object@dateDebut+ object@stepDuration*(object@noPasCourant+as.integer(1))
			# bug cht heure 
			if (object@stepDuration==86400) {
				CurrentDateFin=round.POSIXt(CurrentDateFin,"days")
			}
			return(CurrentDateFin)
		})

#Avance au pas de temps suivant et retourne lengthno du nouveau pas de temps
setGeneric("suivant",def=function(object,...) standardGeneric("suivant"))
setMethod("suivant",signature=signature("PasDeTemps"),definition=function(object){
			object@noPasCourant =object@noPasCourant+as.integer(1)
			if (currentDateFin(object)>DateFin(object)) {
				object@noPasCourant = as.integer(-1)
			}
			return (object)
		})
# retourne le libelle complet comme champ charactere
setGeneric("getdateDebut",def=function(object,...) standardGeneric("getdateDebut"))
setMethod("getdateDebut",signature=signature("PasDeTemps"),definition=function(object){
			return ( strftime(as.POSIXlt(object@dateDebut),format="%Y-%m-%d %H:%M:%S") )
		})

#Fixe la date de debut e partir d'un champ charactere de type "%Y-%m-%d %H:%M:%S" ou "%Y-%m-%d" (classe pas de temps journalier)
setGeneric("setdateDebut",def=function(object,...) standardGeneric("setdateDebut"))
setMethod("setdateDebut",signature=signature("PasDeTemps"),definition=function(object,string){
			object@dateDebut=if (!is.na(strptime(string,format="%Y-%m-%d %H:%M:%S"))) strptime(string,format="%Y-%m-%d %H:%M:%S") else
      strptime(string,format="%Y-%m-%d") 
			return(object) 
		})
# object=setdateDebut(object,"2008-05-01 00:00:00")
# getdateDebut(object)

setGeneric("getLibellesPas",def=function(object,...) standardGeneric("getLibellesPas"))
setMethod("getLibellesPas",signature=signature("PasDeTemps"),definition=function(object){
			ret=paste(LesPasDeTemps$LabelPasDeTemps)
			return (ret )
		})
setGeneric("getAnnees",def=function(object,...) standardGeneric("getAnnees"))
setMethod("getAnnees",signature=signature("PasDeTemps"),definition=function(object){
			 dateFin=DateFin(object)
			 dateDebut=object@dateDebut
			 seq=seq.POSIXt(from=dateDebut,to=dateFin,by="day")
			 seq=seq[-length(seq)]# dans le bilan Migration la derniere valeur n'est pas prise en compte
			 annees=unique(strftime(seq,"%Y"))
  		return (as.numeric(annees))
		})
# pour test #object=new("PasDeTemps")    
setMethod("choice",signature=signature("PasDeTemps"),definition=function(object) {
			if (length(LesPasDeTemps$LabelPasDeTemps) > 0){
				hwinpa=function(h,...){
					pas=svalue(choicepas)
					nbStep=as.numeric(svalue(choicenbStep)) 
					object@nbStep<-nbStep
					object@stepDuration<-as.numeric(LesPasDeTemps$ValeurPasDeTemps[LesPasDeTemps$LabelPasDeTemps%in%pas])
					object=setdateDebut(object,svalue(datedeb))
					#object@nbStep<<-nbStep
					#object@stepDuration<<-LesPasDeTemps$ValeurPasDeTemps[LesPasDeTemps$LabelPasDeTemps%in%pas]
					assign("pasDeTemps",object,envir_stacomi)
					#funout("Les pas de temps ont ete charges\n")
					#print(object)
					# pour l'instant pour une raison inexpliquee, je n'arrive pas e traduire
					# object@nbStep dans l'environnement principal alors que  object@stepDuration
					# est remplace dans object => ???????  je ne comprens pas....
					# Je le remplace par un assign du meme object mais ce n'est pas propre car cela
					# suppose que je ne peux avoir que lepas e passer e cette fonction
					
					#dispose(winpa)
				}
				hchoicepas=function(h,...){
					pas=svalue(choicepas)
					nbStep=as.numeric(svalue(choicenbStep))
					object@stepDuration<-as.numeric(LesPasDeTemps$ValeurPasDeTemps[LesPasDeTemps$LabelPasDeTemps%in%pas])
					object@nbStep<-nbStep 
					object=setdateDebut(object,svalue(datedeb))
					#print(object@dateDebut)
					
					#assign("date",svalue(datedeb),envir = .GlobalEnv)     
					add(datedefin,strftime(as.POSIXlt(DateFin(object)),format="%Y-%m-%d %H:%M:%S"),
							font.attr=c(foreground.colors="red") )
					hwinpa(h)
				}
				hchoicedatedebut=function(h,...){
					# TODO a developper
				}
				winpa=gframe(get("msg",envir=envir_stacomi)$PasdeTemps.1,container=group,horizontal=FALSE)
				pg<-ggroup(horizontal=FALSE,container=winpa)
				glabel("Date de debut",container=pg)
				datedeb<-gedit(getdateDebut(object),
						container=pg,handler=hchoicepas,width=15)
				datedebut2=as.character(strftime(object@dateDebut,"%Y-%m-%d"))
				datedeb2<-gcalendar(datedebut2,container=pg,handler=function(h,...){
							svalue(datedeb)<-as.character(strftime(
											strptime(svalue(datedeb2),"%Y-%m-%d"),
											"%Y-%m-%d %H:%M:%S"))
							hchoicepas(h)				
						} )
				glabel(get("msg",envir=envir_stacomi)$PasdeTemps.1,container=winpa)
				pas_libelle=fun_char_spe(LesPasDeTemps$LabelPasDeTemps)
				choicepas=gdroplist(pas_libelle,selected = 8,container=winpa,handler=hchoicepas) 
				glabel(get("msg",envir=envir_stacomi)$PasdeTemps.2,container=winpa)
				choicenbStep=gedit("365",container=winpa,coerce.with=as.numeric,handler=hchoicepas,width=15)
				datedefin<-gtext(get("msg",envir=envir_stacomi)$PasdeTemps.4,height=50,container=winpa) # Date de fin
				gbutton("OK", container=winpa,handler=hwinpa,icon="execute")
			} else funout(get("msg",envir=envir_stacomi)$PasdeTemps.3, arret=TRUE)
		})

		
#' choice method for PasdeTemps
#' this method differs from choice as it is called within a notebook,
#' it does not allow for multiple choice to be made
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}

		setMethod("choicemult",signature=signature("PasDeTemps"),definition=function(object) {
					if (length(LesPasDeTemps$LabelPasDeTemps) > 0){
						hwinpa=function(h,...){
							pas=svalue(choicepas)
							nbStep=as.numeric(svalue(choicenbStep)) 
							object@nbStep<<-nbStep
							object@stepDuration<<-as.numeric(LesPasDeTemps$ValeurPasDeTemps[LesPasDeTemps$LabelPasDeTemps%in%pas])
							object=setdateDebut(object,svalue(datedeb))						
							assign("pasDeTemps",object,envir_stacomi)
							funout("Les pas de temps ont ete charges\n")
							# charge le deuxieme onglet du notebook
							#svalue(notebook)<-2
						}
						hchoicepas=function(h,...){
							#browser()
							pas=svalue(choicepas)
							nbStep=as.numeric(svalue(choicenbStep))
							object@stepDuration<-as.numeric(LesPasDeTemps$ValeurPasDeTemps[LesPasDeTemps$LabelPasDeTemps%in%pas])
							object@nbStep<-nbStep 
							object=setdateDebut(object,svalue(datedeb))
							#print(object@dateDebut)							
							#assign("date",svalue(datedeb),envir = .GlobalEnv)     
							add(datedefin,strftime(as.POSIXlt(DateFin(object)),format="%Y-%m-%d %H:%M:%S"),
									font.attr=c(foreground.colors="red") )
							hwinpa(h)
						}
						hchoicedatedebut=function(h,...){
							# TODO a developper
						}
						groupdate<<-ggroup(container=notebook, label="periode")   ## "add" called by constructor this is a tab of the notebook
						winpa=gframe(get("msg",envir=envir_stacomi)$PasdeTemps.1,container=groupdate,horizontal=FALSE)
						pg<-ggroup(horizontal=FALSE,container=winpa)
						glabel("Date de debut",container=pg)
						datedeb<-gedit(getdateDebut(object),container=pg,handler=hchoicepas,width=15)
						datedebut2=as.character(strftime(object@dateDebut,"%Y-%m-%d"))
						datedeb2<-gcalendar(datedebut2,container=pg,handler=function(h,...){
									svalue(datedeb)<-as.character(strftime(
													strptime(svalue(datedeb2),"%Y-%m-%d"),
													"%Y-%m-%d %H:%M:%S"))
									hchoicepas(h)				
								} )
						glabel(get("msg",envir=envir_stacomi)$PasdeTemps.1,container=winpa)
						pas_libelle=fun_char_spe(LesPasDeTemps$LabelPasDeTemps)
						choicepas=gdroplist(pas_libelle,selected = 8,container=winpa,handler=hchoicepas) 
						glabel(get("msg",envir=envir_stacomi)$PasdeTemps.2,container=winpa)
						choicenbStep=gedit("365",container=winpa,coerce.with=as.numeric,handler=hchoicepas,width=15)
						datedefin<-gtext(get("msg",envir=envir_stacomi)$PasdeTemps.4,height=50,container=winpa) # Date de fin
						gbutton("OK", container=winpa,handler=hwinpa,icon="execute")
					} else funout(get("msg",envir=envir_stacomi)$PasdeTemps.3, arret=TRUE)
				})
		
# showClass("PasDeTemps")
# validObject( pasDeTemps)
# showMethods("suivant")

