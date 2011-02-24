# Nom fichier :        RequeteODBCwheredate (classe)
# Projet :             controle migrateur / traitement
# Organisme :          IAV
# Auteur :             Cedric Briand
# Contact :            cedric.briand@lavilaine.com
# Date de creation :   10/01/2009 22:49:33
# Compatibilite :      R 2.7.0
# Etat :               OK
# Description          Classe de connexion à la base de donnee, envoie une requete sql avec une clause where concernant une date 
#                       avec une commande overlaps
#                     Utilisee par la classe BilanFonctionnementDC
#**********************************************************************

#' @title RequeteODBCwhere class 
#' @note Inherits from RequeteODBCwhere and uses its connect method with a new SetAs
#' @slot datedebut="POSIXlt"
#' @slot datefin="POSIXlt"
#' @slot colonnedebut="character" # name of the column containing datedebut
#' @slot colonnefin="character"  # name of the column containing datefin
#' @slot select="character"		(inherited from ConnexionODBCwhere) 
#' @slot where="character"		(inherited from ConnexionODBCwhere) 
#' @slot and="vector"			(inherited from ConnexionODBCwhere) 
#' @slot order_by="character"	(inherited from ConnexionODBCwhere) 
#' @slot baseODBC="vector" 		(inherited from ConnexionODBC)
#' @slot silent="logical" 		(inherited from ConnexionODBC)
#' @slot etat="character" 		(inherited from ConnexionODBC)
#' @slot connexion="ANY" 		(inherited from ConnexionODBC)
#' @slot sql="character" 		(inherited from RequeteODBC)
#' @slot query="data.frame"		(inherited from RequeteODBC)
#' @slot open="logical" 		(inherited from RequeteODBC)
#' @example objet=new("RequeteODBCwhere")
setClass(Class="RequeteODBCwheredate",
		representation= representation(datedebut="POSIXlt",datefin="POSIXlt",colonnedebut="character",colonnefin="character"),
		prototype = list(silent=TRUE,open=FALSE),contains="RequeteODBCwhere")

#' transformation method from RequeteODBCwheredate to RequeteODBC
#' @returnType S4 object
#' @return An object of class RequeteODBC
setAs("RequeteODBCwheredate","RequeteODBCwhere",function(from,to){
			requeteODBCwhere=new("RequeteODBCwhere")
			requeteODBCwhere@where=paste("WHERE (",from@colonnedebut,
					", ",from@colonnefin,
					") overlaps (DATE '",
					from@datedebut,"',DATE '",
					from@datefin,"') ")
			requeteODBCwhere@and=paste(from@and,sep=" ") # concatenation du vecteur
			requeteODBCwhere@select=from@select
			requeteODBCwhere@order_by=from@order_by
			requeteODBCwhere@baseODBC=from@baseODBC
			requeteODBCwhere@silent=from@silent
			# other slots will be filled in by connect	
			return(requeteODBCwhere)
		})
#' connect method loads a request to the database and returns either an error or a data.frame
#' @note method modified from v0.2.1240 to use the connect method of the mother class which in turn will use the method of the mother class
#' @returnType S4 object
#' @return An object of class RequeteODBCwheredate
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @example 
#' objet<-new("RequeteODBCwheredate")
#' objet@baseODBC<-baseODBC
#' objet@select<- "select * from t_operation_ope"
#' objet@datedebut=strptime("1996-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S")
#' objet@datefin=strptime("2000-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S")
#' objet@colonnedebut="ope_date_debut"
#' objet@colonnefin="ope_date_fin"
#' objet@and<-c("AND ope_dic_identifiant=1","AND ope_dic_identifiant=2")
#' objet@order_by<-"ORDER BY ope_identifiant"
#' objet@silent=FALSE
#' objet<-connect(objet)
setMethod("connect",signature=signature("RequeteODBCwheredate"),definition=function(objet) {
			requeteODBCwhere=as(objet,"RequeteODBCwhere")
			requeteODBCwhere=connect(requeteODBCwhere) # utilise la méthode de la classe mère
			# récupère au sein de l'objet les éléments de requeteODBC
			objet@where=requeteODBCwhere@where
			objet@connexion=requeteODBCwhere@connexion
			objet@query=requeteODBCwhere@query
			objet@etat=requeteODBCwhere@etat
			objet@sql=requeteODBCwhere@sql
			return(objet)
		})

