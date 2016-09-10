#' Class "RefPoidsMoyenPeche"
#' 
#' Class which enables to load bunches of elver of experimental fishings
#' 
#' 
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
#' @seealso Other referential classes 
#' \code{\linkS4class{RefAnnee}}
#' \code{\linkS4class{RefCheckBox}} 
#' \code{\linkS4class{RefChoix}}
#' \code{\linkS4class{RefCoe}} 
#' \code{\linkS4class{RefDC}}
#' \code{\linkS4class{RefDF}} 
#' \code{\linkS4class{RefListe}}
#' \code{\linkS4class{Refpar}} 
#' \code{\linkS4class{Refparqual}}
#' \code{\linkS4class{Refparquan}} 
#' \code{\linkS4class{RefPoidsMoyenPeche}}
#' \code{\linkS4class{RefStades}} 
#' \code{\linkS4class{RefStationMesure}}
#' \code{\linkS4class{RefTaxon}}
#' @keywords classes
#' @family Referential objects
setClass(Class="RefPoidsMoyenPeche",representation=
				representation(data="data.frame",datedebut="POSIXlt",datefin="POSIXlt"),
		prototype=prototype(data=data.frame()))
# pour test  
# object= new("RefPoidsMoyenPeche")



#' Charge method for RefPoidsMoyenPeche
#' 
#' @return An object of class \code{\linkS4class{RefPoidsMoyenPeche}}
#' 
#' @author cedric.briand
#' @export
setMethod("charge",signature=signature("RefPoidsMoyenPeche"),definition=function(object){
			baseODBCmortciv<-get("baseODBCmortciv",envir=envir_stacomi)
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
					(datefinpeche-datedebutpeche)/2 AS time.sequence,
					datedebutpeche+(datefinpeche-datedebutpeche)/2 AS datemoy,
					date_part('year', datedebutpeche) AS annee,
					date_part('month',datedebutpeche) AS mois  
					FROM vue_ope_lot_poids"
			requete@and=""
			requete<-stacomirtools::connect(requete)  
			object@data<-requete@query
			return(object)
		})
