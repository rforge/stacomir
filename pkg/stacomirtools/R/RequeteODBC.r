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
			# the function is intended to work with stacomiR package but will work outside hence the workanyway function
			
				msg1<-"ODBC error =>you have to define a vector baseODBC with the ODBC link name, user and password"
				msg2<-"connexion trial :"
				msg3<-"connexion impossible"
				msg4<-"connexion successfull"
				msg5<-"request trial"
				msg6<-"success"
				funout<-function(text,arret=FALSE){
					if(arret) stop(text) else print(text)
					return(NULL)
				}	
				killfactor=function(df){
					for (i in 1:ncol(df))
					{
						if(is.factor(df[,i])) df[,i]=as.character(df[,i])
					}
					return(df)
				}
			
			# The connection might already be opened, we will avoid to go through there !
			if (is.null(objet@connexion)){ 
				if (exists("envir_stacomi")){
					if (exists("msg",envir_stacomi)){
						msg1<-get("msg",envir=envir_stacomi)$RequeteODBC.1
						msg2<-get("msg",envir=envir_stacomi)$RequeteODBC.2
						msg3<-get("msg",envir=envir_stacomi)$RequeteODBC.3
						msg4<-get("msg",envir=envir_stacomi)$RequeteODBC.4
						msg5<-get("msg",envir=envir_stacomi)$RequeteODBC.5
						msg6<-get("msg",envir=envir_stacomi)$RequeteODBC.6
						verbose<-exists("showmerequest",envir=envir_stacomi)
					} else {
					# msg not changed loaded at the beginning
					verbose<-exists("showmerequest",envir=envir_stacomi)
					}
				} else {
					# msg not changed loaded at the beginning
					verbose<-FALSE # cannot exist in envir_stacomi as envir_stacomi does not exists
				}
				if (length(objet@baseODBC)!=3)  {
					if (exists("baseODBC",envir=.GlobalEnv)) {
						objet@baseODBC<-get("baseODBC",envir=.GlobalEnv)  
					} else {
						funout(msg1,arret=TRUE)
					}
				}
				# ouverture de la connection ODBC
				e=expression(channel <-odbcConnect(objet@baseODBC[1],
								uid = objet@baseODBC[2],
								pwd = objet@baseODBC[3],
								case = "tolower",
								believeNRows = FALSE))
				if (!objet@silent) funout(paste(msg2,objet@baseODBC[1],"\n"))
				# renvoit du resultat d'un try catch expression dans
				#l'objet Connexion courante, soit un vecteur caractere
				objet@connexion<-tryCatch(eval(e), error=paste(msg3 ,objet@baseODBC)) 
				# un objet S3 RODBC
				if (class(objet@connexion)=="RODBC") {
					if (!objet@silent)funout(msg4)
					objet@etat=msg4# success
				} else {
					objet@etat<-objet@connexion # report of the error
					objet@connexion<-NULL
					funout(msg3,arret=TRUE)
				}
				# sending the query
			} 
			if (!objet@silent) funout(msg5) # query trial
			if (verbose) print(objet@sql)
			e=expression(query<-sqlQuery(objet@connexion,objet@sql,errors=TRUE))
			if (objet@open) {
				# If we want to leave the connexion open no finally clause
				resultatRequete<-tryCatch(eval(e),error = function(e) e)
			} else {
				# otherwise the connexion is closed while ending the request
				resultatRequete<-tryCatch(eval(e),error = function(e) e,finally=odbcClose(objet@connexion))
			}
			if ((class(resultatRequete)=="data.frame")[1]) {
				if (!objet@silent) funout(msg6)
				objet@query=killfactor(query)     # instead of query 11/08/2009 11:55:20
				objet@etat=msg6
			} else {
				if (!objet@silent) print(resultatRequete)
				objet@etat=as.character(resultatRequete)
				print(objet@etat)
			}
			return(objet)
		})