# Nom fichier :        PasdeTemps (classe)
# Projet :             controle migrateur
# Organisme :          IAV
# Auteur :             Cedric Briand
# Contact :            cedric.briand@lavilaine.com
# Date de creation :  31/03/2008 17:21:25
# Compatibilite :
# Etat :               developpement
# Description          calcul et affichage des pas de temps (classe objet)
#**********************************************************************
#*

################################################################
# Declarations de classe
################################################################
#fonction pour valider les objets de classe PasDeTempsJournalier
validite_PasDeTempsJournalier=function(object)
{
	retValue<-NULL
	rep1 = validite_PasDeTemps(object)
	if (!is.logical(rep1)) retValue<-rep1
	rep2 = (object@dureePas==86400)
	if (!rep2) retValue=paste(retValue,get("msg",envir=envir_stacomi)$PasdeTempsJournalier.1)
	rep3 = length(getAnnees(object))==1
	if (!rep3) retValue=paste(retValue,get("msg",envir=envir_stacomi)$PasdeTempsJournalier.2)
	return(ifelse( rep1 & rep2 & rep3 ,TRUE,retValue)   )
}	
#definition de la classe

setClass(Class="PasDeTempsJournalier",contains="PasDeTemps",
		prototype=(dureePas=86400) ,
		validity=validite_PasDeTempsJournalier
)

# pour test #objet=new("PasDeTempsJournalier")
setMethod("choix",signature=signature("PasDeTempsJournalier"),definition=function(objet) {
			if (length(LesPasDeTemps$LabelPasDeTemps) > 0){
				hwinpa=function(h,...){
					pas=svalue(choixpas)
					nbpas=as.numeric(svalue(choixnbpas)) 
					objet@nbPas<-nbpas
					objet@dureePas<-LesPasDeTemps$ValeurPasDeTemps[LesPasDeTemps$LabelPasDeTemps%in%pas]
					objet=setdateDebut(objet,as.POSIXlt(svalue(datedeb)))
					svalue(datedefin)<-as.Date(DateFin(objet))
					assign("pasDeTemps",objet,envir_stacomi)
					funout(get("msg",envir=envir_stacomi)$PasdeTempsJournalier.8)
					#dispose(winpa)
				}
				winpa=gframe(get("msg",envir=envir_stacomi)$PasdeTempsJournalier.3,container=group,horizontal=FALSE)
				pg<-glayout(cont=winpa)
				pg[1,1]<-glabel(get("msg",envir=envir_stacomi)$PasdeTempsJournalier.4)
				datedeb<-gedit(as.Date(getdateDebut(objet)),handler=hwinpa,width=10)
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

