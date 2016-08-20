# Nom fichier :        RequeteODBC.R 
#' @title RequeteODBC class 
#' @note Inherits from ConnectionODBC
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @slot baseODBC="vector" (inherited from ConnectionODBC)
#' @slot silent="logical" (inherited from ConnectionODBC)
#' @slot etat="character" (inherited from ConnectionODBC)
#' @slot connection="ANY" (inherited from ConnectionODBC)
#' @slot sql="character"
#' @slot query="data.frame"
#' @slot open=logical is the connection left open after the request ?
#' @expamples object=new("RequeteODBC")
setClass(Class="RequeteODBC",
		representation= representation(sql="character",query="data.frame",open="logical"),
		prototype = list(silent=TRUE,open=FALSE),
		contains="ConnectionODBC")

#' connect method loads a request to the database and returns either an error or a data.frame
#' @note assign("showmerequest",1,envir=envir_stacomi) permet d'afficher toutes les requetes passant par la classe connect
#' @return An object of class RequeteODBC
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @expamples 
#' object=new("RequeteODBC")
#' object@open=TRUE
#' object@baseODBC=baseODBC
#' object@sql= "select * from t_lot_lot limit 100"
#' object<-connect(object)
#' odbcClose(object@connection)
#' odbcCloseAll()
setMethod("connect",signature=signature("RequeteODBC"),definition=function(object) {     
			# the function is intended to work with stacomiR package but will work outside hence the workanyway function
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
					msg1<-"ODBC error =>you have to define a vector baseODBC with the ODBC link name, user and password"
					msg2<-"connection trial :"
					msg3<-"connection impossible"
					msg4<-"connection successfull"
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
				}
			} else {
				# msg not changed loaded at the beginning
				verbose<-FALSE # cannot exist in envir_stacomi as envir_stacomi does not exists
				msg1<-"ODBC error =>you have to define a vector baseODBC with the ODBC link name, user and password"
				msg2<-"connection trial :"
				msg3<-"connection impossible"
				msg4<-"connection successfull"
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
			}

			
			# The connection might already be opened, we will avoid to go through there !
			if (is.null(object@connection)){ 				
				if (length(object@baseODBC)!=3)  {
					if (exists("baseODBC",envir=.GlobalEnv)) {
						object@baseODBC<-get("baseODBC",envir=.GlobalEnv)  
					} else {
						funout(msg1,arret=TRUE)
					}
				}
				# opening of ODBC connection
				e=expression(channel <-odbcConnect(object@baseODBC[1],
								uid = object@baseODBC[2],
								pwd = object@baseODBC[3],
								case = "tolower",
								believeNRows = FALSE))
				if (!object@silent) funout(paste(msg2,object@baseODBC[1],"\n"))
				# send the result of a try catch expression in
				#the Currentconnection object ie a character vector
				object@connection<-tryCatch(eval(e), error=paste(msg3 ,object@baseODBC)) 
				# un object S3 RODBC
				if (class(object@connection)=="RODBC") {
					if (!object@silent)funout(msg4)
					object@etat=msg4# success
				} else {
					object@etat<-object@connection # report of the error
					object@connection<-NULL
					funout(msg3,arret=TRUE)
				}
				# sending the query
			} 
			if (!object@silent) funout(msg5) # query trial
			if (verbose) print(object@sql)
			query<-data.frame() # otherwise, query called in the later expression is evaluated as a global variable by RCheck
			e=expression(query<-sqlQuery(object@connection,object@sql,errors=TRUE))
			if (object@open) {
				# If we want to leave the connection open no finally clause
				resultatRequete<-tryCatch(eval(e),error = function(e) e)
			} else {
				# otherwise the connection is closed while ending the request
				resultatRequete<-tryCatch(eval(e),error = function(e) e,finally=RODBC::odbcClose(object@connection))
			}
			if ((class(resultatRequete)=="data.frame")[1]) {
				if (!object@silent) funout(msg6)
				object@query=killfactor(query)     # instead of query 11/08/2009 11:55:20
				object@etat=msg6
			} else {
				if (!object@silent) print(resultatRequete)
				object@etat=as.character(resultatRequete)
				print(object@etat)
			}
			return(object)
		})