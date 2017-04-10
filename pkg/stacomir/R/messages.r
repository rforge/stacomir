# messages.r
# last changes 3 april 2011
# Author: cedric
###############################################################################







#' Function used at the beginning of the program to display the first messages
#' collected by method RODBC Attention all msgs are starting with a letter "l"
#' numbers are wrong when used as msgs within a list
#' 
#' Function used at the beginning of the program to display the first messages
#' collected by method RODBC Attention all msgs are starting with a letter "l"
#' numbers are wrong when used as msgs within a list
#' 
#' 
#' @param lang one of "French","Spanish","English"
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
# exported for test_that
messages=function(){
	msg=list()
# RequeteODBC
	msg$RequeteODBC.1<-gettext("Error ODBC =>Define a vector baseODBC with the name of the ODBC link, user and password")
	msg$RequeteODBC.2<-gettext("Connection trial :")
	msg$RequeteODBC.3<-gettext("Connection failure :")
	msg$RequeteODBC.4<-gettext("Connection successful")
	msg$RequeteODBC.5<-gettext("Trying query")
	msg$RequeteODBC.6<-gettext("Query succeded")
# RequeteODBCwhere
# RequeteODBCwheredate

	assign("msg",msg,envir=envir_stacomi)
}
