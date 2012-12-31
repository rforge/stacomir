# Nom fichier :        RefCoe(classe)

#' @title RefCoe referential class 
#' @note Class loading coefficient of conversion between quantity (weights or volumes of glass eel) and numbers 
#' between a starting and finishing date
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @slot data="data.frame"
#' @slot datedebut="POSIXlt"
#' @slot datefin="POSIXlt"
#' @method charge
#' @expamples coe= new("RefCoe")
setClass(Class="RefCoe",representation=
				representation(data="data.frame",datedebut="POSIXlt",datefin="POSIXlt"),
		prototype=prototype(data=data.frame()))

#' loads the coefficients for the period defined in class
#' The slots datedebut and datefin have to be filled before using charge
#' @returnType Object of class RefCoe
#' @return Object of class RefCoe
#' @note Bien que je ne comprenne pourquoi, la connexion fonctionne si on ne pointe pas le sch�ma
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @expamples objet<- new("RefCoe")
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