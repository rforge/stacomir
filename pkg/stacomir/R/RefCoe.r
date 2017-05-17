# Nom fichier :        RefCoe(classe)

#' Class "RefCoe"
#' 
#' Enables to load conversion coefficients quantity-number. This class only exists to load
#' the data with its method charge. It is not used directly as component of the graphical interface,
#' as the year is already loaded in the different Bilan objects
#' 
#' 
#' @note Class loading coefficient of conversion between quantity (weights or
#' volumes of glass eel) and numbers between a starting and finishing date
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RefCoe")}.
#' @slot data A \code{data.frame}
#' @slot datedebut A "POSIXlt"
#' @slot datefin A "POSIXlt"
#' @author cedric.briand"at"eptb-vilaine.fr
#' @family Referential objects
#' @keywords classes
setClass(Class="RefCoe",representation=
				representation(data="data.frame",datedebut="POSIXlt",datefin="POSIXlt"),
		prototype=prototype(data=data.frame()))

#' loads the coefficients for the period defined in class
#' 
#' 
#' The slots datedebut and datefin have to be filled before using charge
#' @param object An object of class \link{RefCoe-class}
#' @return Object of class RefCoe
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#' object<- new("RefCoe")
#' object@datedebut<-strptime("01/01/1996",format="%d/%m/%Y")
#' object@datefin<-strptime("01/01/1997",format="%d/%m/%Y")
#' charge(object) 
#' }
setMethod("charge",signature=signature("RefCoe"),definition=function(object){
			requete=new("RequeteODBCwheredate")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@datedebut=object@datedebut
			requete@datefin=object@datefin
			requete@colonnedebut="coe_date_debut"
			requete@colonnefin="coe_date_fin"
			requete@datefin=as.POSIXlt(object@datefin)		
			requete@select=stringr::str_c("select * from ",
					get("sch",envir=envir_stacomi),
					"tj_coefficientconversion_coe")
			requete@and=" and  coe_tax_code='2038' and coe_std_code='CIV' and coe_qte_code='1'"
			requete<-connect(requete)  
			object@data<-requete@query
			return(object)
		})


#' supprime method for "RefCoe" class
#' @param object An object of class \link{RefCoe-class}
#' @param tax '2038=Anguilla anguilla'
#' @param std 'CIV=civelle'
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @return nothing
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export

setMethod("supprime",signature=signature("RefCoe"),
		definition=function(object,tax,std,silent=FALSE)
		{ 
			#object<-bilPM@coe;tax=2038;std="CIV"
			# getting the data to import
		
			# here I assume that dc_selectionne will be unique (no bilan with several dc)
			#
			requete=new("RequeteODBCwheredate")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@datedebut<-object@datedebut
			requete@datefin<-object@datefin
			requete@colonnedebut<-"coe_date_debut"
			requete@colonnefin<-"coe_date_fin"
			requete@select=stringr::str_c("DELETE from ",get("sch",envir=envir_stacomi),"tj_coefficientconversion_coe ")
			requete@and=str_c(" and  coe_tax_code='",tax,"' and coe_std_code='",std,"' and coe_qte_code='1'")
			requete<-stacomirtools::connect(requete)
			if (!silent) funout(gettextf("%s rows deleted from table tj_coefficientconversion_coe",nrow(object@data),domain="R-stacomiR"))
			return(invisible(NULL))
		})

