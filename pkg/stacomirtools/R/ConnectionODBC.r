# Nom fichier :        ConnectionODBC (classe)
# Organisme :          IAV
# Auteur :             Cedric Briand
# Contact :            cedric.briand"at"eptb-vilaine.fr
# Date de creation :   06/02/2007 10:58:37
# Etat :               OK
# Description          Classe de connexion a la base de donnee
#**********************************************************************

#validation function
validite_ODBC=function(object)
{
	rep1= class(object@baseODBC[1])=="Character"
	rep2=class(object@baseODBC[2])=="Character"
	rep3=class(object@baseODBC[3])=="ANY"
	rep4=length(object@baseODBC)==3
	return(ifelse(rep1 & rep2 & rep3 & rep4,TRUE,c(1:4)[!c(rep1, rep2, rep3, rep4)]))
}

#' @title ConnectionODBC class 
#' @note Mother class for connection, opens the connection but does not shut it
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @slot baseODBC="vector" (of length 3, character)
#' @slot silent="logical"
#' @slot etat="ANY" # can be -1 or string
#' @slot connection="ANY" # could be both string or S3
#' @slot sql="character"
#' @slot query="data.frame"
#' @return connectionODBC an S4 object of class connectionODBC
#' @examples 
#' object=new("ConnectionODBC")
#' object@baseODBC=c("myODBCconnection","myusername","mypassword")
#' object@silent=FALSE
#' object<-connect(object)
#' odbcClose(object@connection)
setClass(Class="ConnectionODBC",
		representation= representation(baseODBC="vector",silent="logical",etat="ANY",connection="ANY"),
		prototype = list(silent=TRUE),
		validity=validite_ODBC)
setGeneric("connect",def=function(object,...) standardGeneric("connect"))

#' connect method for ConnectionODBC class
#' @return a connection with slot filled
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples object=new("ConnectionODBC")
#' object@baseODBC=baseODBC
#' connect(object)
setMethod("connect",signature=signature("ConnectionODBC"),definition=function(object) {     
			if (length(object@baseODBC)!=3)  {
				if (exists("baseODBC",envir=.GlobalEnv)){ 
					object@baseODBC<-get("baseODBC",envir=.GlobalEnv) 
				} else {
					if(exists("envir_stacomi")){# the program is called within stacomiR
						funout(get("msg",envir_stacomi)$ConnectionODBC.1,arret=TRUE)
					} else	  {
						stop("you need to define a vector baseODBC with the ODBC link, user and password")
					}
				}
			}
			e=expression(channel <-odbcConnect(object@baseODBC[1],
							uid = object@baseODBC[2],
							pwd = object@baseODBC[3],
							case = "tolower",
							believeNRows = FALSE))
			if (!exists("odbcConnect")) {
				if(exists("envir_stacomi")){
					funout("The RODBC library is necessary, please load the package",arret=TRUE)
				} else	  {
					stop("the RODBC library is necessary, please load the package")
				}
			}
			if (!object@silent) {
				if(exists("envir_stacomi")){
					print(paste("connection trial, warning this class should only be used for test: ",object@baseODBC[1]))
				} else {
					print(paste("connection trial, warning this class should only be used for test: ",object@baseODBC[1]))
				}
			}	
			# sends the result of a trycatch connection in the
			#l'object (current connection), e.g. a character vector
			connection_error<-if(exists("envir_stacomi")){
						error=paste(get("msg",envir_stacomi)$ConnectionODBC.4,object@baseODBC[1])
					} else {
						error="impossible connection"
					}
			currentConnection<-tryCatch(eval(e), error=connection_error) 
			if (class(currentConnection)=="RODBC") {
				if (!object@silent){
					if(exists("envir_stacomi")){
						print(get("msg",envir_stacomi)$ConnectionODBC.5)
					} else {
						print("connection successful")
					}
				} 
				object@connection=currentConnection  # an object S3 RODBC
				if(exists("envir_stacomi")){
					state<-get("msg",envir_stacomi)$ConnectionODBC.6
				} else {
					state<-"connected"
				}
				object@etat=state
			} else {
				funout(currentConnection)
				object@etat=currentConnection # reporting error
			}
			return(object)
		})
