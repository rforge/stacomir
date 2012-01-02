# Nom fichier :        ConnexionODBC (classe)
# Projet :             controle migrateur / traitement
# Organisme :          IAV
# Auteur :             Cedric Briand
# Contact :            cedric.briand00@gmail.com
# Date de creation :   06/02/2007 10:58:37
# Compatibilite :      R 2.7.0
# Etat :               OK
# Description          Classe de connexion à la base de donnee
#**********************************************************************

#fonction pour valider les acces ODBC
validite_ODBC=function(object)
{
	rep1= class(object@baseODBC[1])=="Character"
	rep2=class(object@baseODBC[2])=="Character"
	rep3=class(object@baseODBC[3])=="ANY"
	rep4=length(object@baseODBC)==3
	return(ifelse(rep1 & rep2 & rep3 & rep4,TRUE,c(1:4)[!c(rep1, rep2, rep3, rep4)]))
}

#' @title ConnexionODBC class 
#' @note Mother class for connection, opens the connection but does not shut it
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @slot baseODBC="vector" (of length 3, character)
#' @slot silent="logical"
#' @slot etat="ANY" # can be -1 or string
#' @slot connexion="ANY" # could be both string or S3
#' @slot sql="character"
#' @slot query="data.frame"
#' @return connexionODBC an S4 object of class connexionODBC
#' @expamples 
#' objet=new("ConnexionODBC")
#' objet@baseODBC=baseODBC"
#' objet@silent=FALSE
#' objet<-connect(objet)
#' # odbcCloseAll()
#' odbcClose(objet@connexion)
setClass(Class="ConnexionODBC",
		representation= representation(baseODBC="vector",silent="logical",etat="ANY",connexion="ANY"),
		prototype = list(silent=TRUE),
		validity=validite_ODBC)


#' connect method for ConnexionODBC class
#' @returnType ConnectionODBC S4 object
#' @return a connexion with slot filled
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @expamples objet=new("ConnexionODBC")
#' objet@baseODBC=baseODBC
#' connect(objet)
setMethod("connect",signature=signature("ConnexionODBC"),definition=function(objet) {     
		      if (length(objet@baseODBC)!=3)  {
       if (exists("baseODBC",envir=.GlobalEnv)){ 
        objet@baseODBC<-get("baseODBC",envir=.GlobalEnv) 
        } else {
          funout(get("msg",envir_stacomi)$ConnexionODBC.1,arret=TRUE)
        }
      }
      e=expression(channel <-odbcConnect(objet@baseODBC[1],
							uid = objet@baseODBC[2],
							pwd = objet@baseODBC[3],
							case = "tolower",
							believeNRows = FALSE))
			if (!exists("odbcConnect")) funout(get("msg",envir_stacomi)$ConnexionODBC.2,arret=TRUE)
			if (!objet@silent) print(paste(get("msg",envir_stacomi)$ConnexionODBC.3,objet@baseODBC[1]))
			# renvoit du resultat d'un try catch expression dans
			#l'ojet Connexion courante, soit un vecteur caractere
			connexionCourante<-tryCatch(eval(e), error=paste(get("msg",envir_stacomi)$ConnexionODBC.4,objet@baseODBC[1])) 
			if (class(connexionCourante)=="RODBC") {
				if (!objet@silent) print(get("msg",envir_stacomi)$ConnexionODBC.5)
				objet@connexion=connexionCourante  # un objet S3 RODBC
				objet@etat=get("msg",envir_stacomi)$ConnexionODBC.6
			} else {
				funout(connexionCourante)
				objet@etat=connexionCourante # report de l'erreur
			}
			return(objet)
		})
