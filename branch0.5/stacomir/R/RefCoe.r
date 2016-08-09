# Nom fichier :        RefCoe(classe)

#' Class "RefCoe"
#' 
#' Enables to load conversion coefficients quantity-number
#' 
#' 
#' @name RefCoe-class
#' @aliases RefCoe RefCoe-class

#' @note Class loading coefficient of conversion between quantity (weights or
#' volumes of glass eel) and numbers between a starting and finishing date
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefCoe")}.
#' @slot data="data.frame"
#' @slot datedebut="POSIXlt"
#' @slot datefin="POSIXlt"
#' @method charge
#' @author cedric.briand"at"eptb-vilaine.fr
#' @family Referential objects
#' @seealso Other referential classes \code{\linkS4class{RefAnnee}}
#' \code{\linkS4class{RefCheckBox}} \code{\linkS4class{RefChoix}}
#' \code{\linkS4class{RefCoe}} \code{\linkS4class{RefDC}}
#' \code{\linkS4class{RefDF}} \code{\linkS4class{RefListe}}
#' \code{\linkS4class{Refpar}} \code{\linkS4class{Refparqual}}
#' \code{\linkS4class{Refparquan}} \code{\linkS4class{RefPoidsMoyenPeche}}
#' \code{\linkS4class{RefStades}} \code{\linkS4class{RefStationMesure}}
#' \code{\linkS4class{RefTaxon}}
#' @keywords classes
#' @examples
#' 
#' showClass("RefCoe")
#' 
setClass(Class="RefCoe",representation=
				representation(data="data.frame",datedebut="POSIXlt",datefin="POSIXlt"),
		prototype=prototype(data=data.frame()))

#' loads the coefficients for the period defined in class
#' The slots datedebut and datefin have to be filled before using charge
#' @returnType Object of class RefCoe
#' @return Object of class RefCoe
#' @note Bien que je ne comprenne pourquoi, la connection fonctionne si on ne pointe pas le sch�ma
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @examples objet<- new("RefCoe")
#' objet@datedebut<-strptime("01/01/1996",format="%d/%m/%Y")
#' objet@datefin<-strptime("01/01/1997",format="%d/%m/%Y")
#' charge(objet) # objet
setMethod("charge",signature=signature("RefCoe"),definition=function(objet){
			requete=new("RequeteODBCwheredate")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@datedebut=objet@datedebut
			requete@datefin=objet@datefin
			requete@colonnedebut="coe_date_debut"
			requete@colonnefin="coe_date_fin"
			requete@select=str_c("select * from ",
					get("sch",envir=envir_stacomi),
					"tj_coefficientconversion_coe")
			requete@and=" and  coe_tax_code='2038' and coe_std_code='CIV' and coe_qte_code='1'"
			requete=connect(requete)  
			objet@data<-requete@query
			return(objet)
		})
# pas de methode choix, le choix est dej� fait dans l'annee de l'interface