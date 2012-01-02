# Nom fichier :        RequeteODBC.R 
#' @title RequeteODBC class 
#' @note Inherits from ConnexionODBC
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @slot baseODBC="vector" (inherited from ConnexionODBC)
#' @slot silent="logical" (inherited from ConnexionODBC)
#' @slot etat="character" (inherited from ConnexionODBC)
#' @slot connexion="ANY" (inherited from ConnexionODBC)
#' @slot sql="character"
#' @slot query="data.frame"
#' @slot open=logical is the connexion left open after the request ?
#' @expamples objet=new("RequeteODBC")
setClass(Class="RequeteODBC",
		representation= representation(sql="character",query="data.frame",open="logical"),
		prototype = list(silent=TRUE,open=FALSE),
		contains="ConnexionODBC")

#' connect method loads a request to the database and returns either an error or a data.frame
#' @note assign("showmerequest",1,envir=envir_stacomi) permet d'afficher toutes les requetes passant par la classe connect
#' @returnType S4 object
#' @return An object of class RequeteODBC
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @expamples 
#' objet=new("RequeteODBC")
#' objet@open=TRUE
#' objet@baseODBC=baseODBC
#' objet@sql= "select * from t_lot_lot limit 100"
#' objet<-connect(objet)
#' odbcClose(objet@connexion)
#' odbcCloseAll()
setMethod("connect",signature=signature("RequeteODBC"),definition=function(objet) {     
			 # la connexion peut deja etre ouverte, on veut eviter de passer par la !
			if (is.null(objet@connexion)){ 
					if (length(objet@baseODBC)!=3)  {
						if (exists("baseODBC",envir=.GlobalEnv)) {
							objet@baseODBC<-get("baseODBC",envir=.GlobalEnv)  
						} else {
							funout(get("msg",envir=envir_stacomi)$RequeteODBC.1,arret=TRUE)
						}
					}
					# ouverture de la connection ODBC
					e=expression(channel <-odbcConnect(objet@baseODBC[1],
									uid = objet@baseODBC[2],
									pwd = objet@baseODBC[3],
									case = "tolower",
									believeNRows = FALSE))
					if (!objet@silent) funout(paste(get("msg",envir=envir_stacomi)$RequeteODBC.2,objet@baseODBC[1],"\n"))
					# renvoit du resultat d'un try catch expression dans
					#l'objet Connexion courante, soit un vecteur caractere
					objet@connexion<-tryCatch(eval(e), error=paste("connexion impossible ",objet@baseODBC)) 
					# un objet S3 RODBC
					if (class(objet@connexion)=="RODBC") {
						if (!objet@silent)funout(get("msg",envir=envir_stacomi)$RequeteODBC.4)
						objet@etat=get("msg",envir=envir_stacomi)$RequeteODBC.4# success
					} else {
						objet@etat<-objet@connexion # report de l'erreur
						objet@connexion<-NULL
						funout("connexion ODBC impossible",arret=TRUE)
					}
					# Envoi de la requete  
				} 
			if (!objet@silent) funout(get("msg",envir=envir_stacomi)$RequeteODBC.5) # essai de la requete
			if (exists("showmerequest",envir=envir_stacomi)) print(objet@sql)
			e=expression(query<-sqlQuery(objet@connexion,objet@sql,errors=TRUE))
			if (objet@open) {
				# si on veut laisser la connexion ouverte on ne passe pas de clause finally
				resultatRequete<-tryCatch(eval(e),error = function(e) e)
			} else {
				# sinon la connexion est fermée à la fin de la requète
				resultatRequete<-tryCatch(eval(e),error = function(e) e,finally=odbcClose(objet@connexion))
			}
			if ((class(resultatRequete)=="data.frame")[1]) {
				if (!objet@silent) funout(get("msg",envir=envir_stacomi)$RequeteODBC.6)
				objet@query=killfactor(query)     # au lieu de query 11/08/2009 11:55:20
				objet@etat=get("msg",envir=envir_stacomi)$RequeteODBC.6
			} else {
				if (!objet@silent) print(resultatRequete)
				objet@etat=as.character(resultatRequete)
				print(objet@etat)
			}
			return(objet)
		})