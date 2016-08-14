# Nom fichier :        RefPoidsMoyenPeche(classe)
# Projet :             controle migrateur
# Organisme :          IAV
# Auteur :             Cedric Briand
# Contact :            cedric.briand"at"eptb-vilaine.fr
# Date de creation :  31/03/2008 17:21:25 / modif10/01/2009 22:50:42
# Compatibilite :
# Etat :               
# Description          Classe appellee pour sa methode charge qui permet le chargement des lots de la base MORTCIV des peches experimentales
#                      entre une date de debut et une date de fin 
#                      Utilise dans Bilan_poids_moyen uniquement pour Arzal
#                      

#' Class "RefPoidsMoyenPeche"
#' 
#' Class which enables to load bunches of elver of experimental fishings
#' 
#' 
#' @name RefPoidsMoyenPeche
#' @aliases RefPoidsMoyenPeche-class RefPoidsMoyenPeche

#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefPoidsMoyenPeche",
#' data=data.frame(),datedebut="POSIXlt",datefin="POSIXlt")}.  \describe{
#' \item{list("data")}{Object of class \code{"data.frame"} ~
#' Datas}\item{:}{Object of class \code{"data.frame"} ~ Datas}
#' \item{list("datedebut")}{Object of class \code{"POSIXlt"} ~ Starting
#' year}\item{:}{Object of class \code{"POSIXlt"} ~ Starting year}
#' \item{list("datefin")}{Object of class \code{"POSIXlt"} ~ Ending
#' year}\item{:}{Object of class \code{"POSIXlt"} ~ Ending year} }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso Other referential classes \code{\linkS4class{RefAnnee}}
#' \code{\linkS4class{RefCheckBox}} \code{\linkS4class{RefChoix}}
#' \code{\linkS4class{RefCoe}} \code{\linkS4class{RefDC}}
#' \code{\linkS4class{RefDF}} \code{\linkS4class{RefListe}}
#' \code{\linkS4class{Refpar}} \code{\linkS4class{Refparqual}}
#' \code{\linkS4class{Refparquan}} \code{\linkS4class{RefPoidsMoyenPeche}}
#' \code{\linkS4class{RefStades}} \code{\linkS4class{RefStationMesure}}
#' \code{\linkS4class{RefTaxon}}
#' @keywords classes
#' @family Referential objects
#' @examples
#' 
#' showClass("RefPoidsMoyenPeche")
#' 
setClass(Class="RefPoidsMoyenPeche",representation=
				representation(data="data.frame",datedebut="POSIXlt",datefin="POSIXlt"),
		prototype=prototype(data=data.frame()))
# pour test  
# object= new("RefPoidsMoyenPeche")

#retourne la liste des annees presentes dans la base
setMethod("charge",signature=signature("RefPoidsMoyenPeche"),definition=function(object){
			requete=new("RequeteODBCwheredate")
			requete@datedebut=object@datedebut
			requete@datefin=object@datefin
			requete@colonnedebut="datedebutpeche"
			requete@colonnefin="datefinpeche"
			requete@baseODBC<-baseODBCmortciv
			requete@select="SELECT lot_identifiant,
					datedebutpeche AS ope_date_debut ,
					datefinpeche AS ope_date_fin,
					lot_effectif,
					car_valeur_quantitatif as poids,
					(car_valeur_quantitatif/lot_effectif) AS poids_moyen, 
					(datefinpeche-datedebutpeche)/2 AS duree,
					datedebutpeche+(datefinpeche-datedebutpeche)/2 AS datemoy,
					date_part('year', datedebutpeche) AS annee,
					date_part('month',datedebutpeche) AS mois  
					FROM vue_ope_lot_poids"
			requete@and=""
			requete=connect(requete)  
			object@data<-requete@query
			return(object)
		})
# pas de methode choix, le choix est dej� fait dans l'annee de l'interface
#charge(refPoidsMoyenPeche)