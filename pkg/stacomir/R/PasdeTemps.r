# Nom fichier :        PasdeTemps (classe)
# Projet :             controle migrateur
# Organisme :          IAV
# Auteur :             Cedric Briand
# Contact :            cedric.briand00@gmail.com
# Date de creation :  31/03/2008 17:21:25
# Compatibilite :
# Etat :               developpement
# Description          calcul et affichage des pas de temps (classe objet)
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
#fonction pour valider les objets de classe Pas deTemps
validite_PasDeTemps=function(object)
{
  retValue=NULL
	rep1= class(object@dateDebut)[1]=="POSIXt"
	if (!rep1) retValue="object@dateDebut is not of class POSIXt"  
	rep2=length(object@dureePas)==1
	if (!rep2) retValue=paste(retValue,"length(object@dureePas) !=1") 
	rep3=length(object@nbPas)==1
	if (!rep3) retValue=paste(retValue,"length(object@nbPas) !=1") 
	rep4=length(object@noPasCourant)==1
	if (!rep4) retValue=paste(retValue,"length(object@noPasCourant) !=1")
	return(ifelse(rep1 & rep2 & rep3 & rep4,TRUE,retValue))
}
#definition de la classe

setClass(Class="PasDeTemps",representation=
				representation(dateDebut="POSIXt",dureePas="numeric",nbPas="numeric",noPasCourant="integer"),
		validity=validite_PasDeTemps,
		prototype=prototype(dateDebut=as.POSIXlt(trunc.POSIXt(Sys.time(),"year")),
				dureePas=as.numeric(86400),
				nbPas=as.numeric(1),
				noPasCourant=as.integer(0) ) )
# pasDeTemps= new("PasDeTemps")


validite_PasDeTempsChar=function(object)
{
	rep1= class(object@dateDebut)[1]=="POSIXt"
	rep2=length(object@dureePas)==1
	rep3=length(object@nbPas)==1
	rep4=length(object@noPasCourant)==1
	rep5= object@dureePas%in%LesPasDeTemps[,"LabelPasDeTemps"]
	return(ifelse(rep1 & rep2 & rep3 & rep4 & rep5,TRUE,c(1:5)[!c(rep1, rep2, rep3, rep4,rep5)]))
}

setClass(Class="PasDeTempsChar",representation=
				representation(dateDebut="POSIXt",dureePas="character",nbPas="numeric",noPasCourant="integer"),
		validity=validite_PasDeTempsChar,
		prototype=prototype(dateDebut=as.POSIXlt(strptime("2008-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S"),tz="GMT"),
				dureePas=as.character("1 jour"),
				nbPas=as.numeric(1),
				noPasCourant=as.integer(0) ))
#pasDeTempsChar=new("PasDeTempsChar")
# pasDeTempsChar@dureePas= "1 jour"
# pasDeTempsChar@nbPas=as.integer(10)


################################################################
# Conversion de classe
################################################################

#Conversion depuis la classe charactere à la classe numerique
setAs("PasDeTempsChar","PasDeTemps",   # from to
		function(from,to){
			index=LesPasDeTemps[,"LabelPasDeTemps"]%in%from@dureePas
			newdureePas=LesPasDeTemps[index,"ValeurPasDeTemps"]
			new("PasDeTemps",dateDebut=from@dateDebut,
					dureePas=newdureePas,
					nbPas=from@nbPas,
					noPasCourant=from@noPasCourant)})
# pasDeTemps=as(pasDeTempsChar,"PasDeTemps")

################################################################
# methodes standard
################################################################
# retourne le pas courant
setGeneric("getnoPasCourant",def=function(objet,...) standardGeneric("getnoPasCourant"))
setMethod("getnoPasCourant",signature=signature("PasDeTemps"),definition=function(objet) objet@noPasCourant)

# retourne la date de fin
setGeneric("DateFin",def=function(objet,...) standardGeneric("DateFin"))
setMethod("DateFin",signature=signature("PasDeTemps"),definition=function(objet){
			DateFin=objet@dateDebut+ objet@dureePas*(objet@nbPas)
			# pour les pb de changement d'heure
			
			return(DateFin)
		})

#getnoPasCourant(EssaiPasdeTemps)


#Retourne la date de debut correspondant au no de pas courant
setGeneric("currentDateDebut",def=function(objet,...) standardGeneric("currentDateDebut"))
setMethod("currentDateDebut",signature=signature("PasDeTemps"),definition=function(objet){
			CurrentDateDebut=objet@dateDebut+ objet@dureePas*objet@noPasCourant
			# bug cht heure
			if (objet@dureePas==86400) {
				CurrentDateDebut=round.POSIXt(CurrentDateDebut,"days")
			}
			
			return(CurrentDateDebut)
		})

# pasDeTemps@noPasCourant=as.integer(2)
# currentDateDebut(pasDeTemps)

# Retourne la date de fin correspondant au no de pas courant
setGeneric("currentDateFin",def=function(objet,...) standardGeneric("currentDateFin"))
setMethod("currentDateFin",signature=signature("PasDeTemps"),definition=function(objet){
			CurrentDateFin=objet@dateDebut+ objet@dureePas*(objet@noPasCourant+as.integer(1))
			# bug cht heure 
			if (objet@dureePas==86400) {
				CurrentDateFin=round.POSIXt(CurrentDateFin,"days")
			}
			return(CurrentDateFin)
		})

#Avance au pas de temps suivant et retourne lengthno du nouveau pas de temps
setGeneric("suivant",def=function(objet,...) standardGeneric("suivant"))
setMethod("suivant",signature=signature("PasDeTemps"),definition=function(objet){
			objet@noPasCourant =objet@noPasCourant+as.integer(1)
			if (currentDateFin(objet)>DateFin(objet)) {
				objet@noPasCourant = as.integer(-1)
			}
			return (objet)
		})
# retourne le libelle complet comme champ charactere
setGeneric("getdateDebut",def=function(objet,...) standardGeneric("getdateDebut"))
setMethod("getdateDebut",signature=signature("PasDeTemps"),definition=function(objet){
			return ( strftime(as.POSIXlt(objet@dateDebut),format="%Y-%m-%d %H:%M:%S") )
		})

#Fixe la date de debut à partir d'un champ charactere de type "%Y-%m-%d %H:%M:%S" ou "%Y-%m-%d" (classe pas de temps journalier)
setGeneric("setdateDebut",def=function(objet,...) standardGeneric("setdateDebut"))
setMethod("setdateDebut",signature=signature("PasDeTemps"),definition=function(objet,string){
			objet@dateDebut=if (!is.na(strptime(string,format="%Y-%m-%d %H:%M:%S"))) strptime(string,format="%Y-%m-%d %H:%M:%S") else
      strptime(string,format="%Y-%m-%d") 
			return(objet) 
		})
# objet=setdateDebut(objet,"2008-05-01 00:00:00")
# getdateDebut(objet)

setGeneric("getLibellesPas",def=function(objet,...) standardGeneric("getLibellesPas"))
setMethod("getLibellesPas",signature=signature("PasDeTemps"),definition=function(objet){
			ret=paste(LesPasDeTemps$LabelPasDeTemps)
			return (ret )
		})
setGeneric("getAnnees",def=function(objet,...) standardGeneric("getAnnees"))
setMethod("getAnnees",signature=signature("PasDeTemps"),definition=function(objet){
			 dateFin=DateFin(objet)
			 dateDebut=objet@dateDebut
			 seq=seq.POSIXt(from=dateDebut,to=dateFin,by="day")
			 seq=seq[-length(seq)]# dans le bilan Migration la derniere valeur n'est pas prise en compte
			 annees=unique(strftime(seq,"%Y"))
  		return (as.numeric(annees))
		})
# pour test #objet=new("PasDeTemps")    
setMethod("choix",signature=signature("PasDeTemps"),definition=function(objet) {
			if (length(LesPasDeTemps$LabelPasDeTemps) > 0){
				hwinpa=function(h,...){
					pas=svalue(choixpas)
					nbpas=as.numeric(svalue(choixnbpas)) 
					objet@nbPas<-nbpas
					objet@dureePas<-LesPasDeTemps$ValeurPasDeTemps[LesPasDeTemps$LabelPasDeTemps%in%pas]
					objet=setdateDebut(objet,svalue(datedeb))
					#objet@nbPas<<-nbpas
					#objet@dureePas<<-LesPasDeTemps$ValeurPasDeTemps[LesPasDeTemps$LabelPasDeTemps%in%pas]
					assign("pasDeTemps",objet,envir_stacomi)
					#funout("Les pas de temps ont ete charges\n")
					#print(objet)
					# pour l'instant pour une raison inexpliquee, je n'arrive pas à traduire
					# objet@nbPas dans l'environnement principal alors que  objet@dureePas
					# est remplace dans objet => ???????  je ne comprens pas....
					# Je le remplace par un assign du meme objet mais ce n'est pas propre car cela
					# suppose que je ne peux avoir que lepas à passer à cette fonction
					
					#dispose(winpa)
				}
				hchoixpas=function(h,...){
					pas=svalue(choixpas)
					nbPas=as.numeric(svalue(choixnbpas))
					objet@dureePas<-LesPasDeTemps$ValeurPasDeTemps[LesPasDeTemps$LabelPasDeTemps%in%pas]
					objet@nbPas<-nbPas 
					objet=setdateDebut(objet,svalue(datedeb))
					#print(objet@dateDebut)
					
					#assign("date",svalue(datedeb),envir = .GlobalEnv)     
					add(datedefin,strftime(as.POSIXlt(DateFin(objet)),format="%Y-%m-%d %H:%M:%S"),
							font.attr=c(foreground.colors="red") )
					hwinpa(h)
				}
				hchoixdatedebut=function(h,...){
					# TODO a developper
				}
				winpa=gframe(get("msg",envir=envir_stacomi)$PasdeTemps.1,container=group,horizontal=FALSE)
				pg<-ggroup(horizontal=FALSE,cont=winpa)
				glabel("Date de debut",container=pg)
				datedeb<-gedit(getdateDebut(objet),cont=pg,handler=hchoixpas,width=15)
				datedebut2=as.character(strftime(objet@dateDebut,"%Y-%m-%d"))
				datedeb2<-gcalendar(datedebut2,cont=pg,handler=function(h,...){
							svalue(datedeb)<-as.character(strftime(
											strptime(svalue(datedeb2),"%Y-%m-%d"),
											"%Y-%m-%d %H:%M:%S"))
							hchoixpas(h)				
						} )
				glabel(get("msg",envir=envir_stacomi)$PasdeTemps.1,container=winpa)
				pas_libelle=fun_char_spe(LesPasDeTemps$LabelPasDeTemps)
				choixpas=gdroplist(pas_libelle,selected = 8,container=winpa,handler=hchoixpas) 
				glabel(get("msg",envir=envir_stacomi)$PasdeTemps.2,container=winpa)
				choixnbpas=gedit("365",container=winpa,coerce.with=as.numeric,handler=hchoixpas,width=15)
				datedefin<-gtext(get("msg",envir=envir_stacomi)$PasdeTemps.4,height=50,container=winpa) # Date de fin
				gbutton("OK", container=winpa,handler=hwinpa,icon="execute")
			} else funout(get("msg",envir=envir_stacomi)$PasdeTemps.3, arret=TRUE)
		})

# showClass("PasDeTemps")
# validObject( pasDeTemps)
# showMethods("suivant")

