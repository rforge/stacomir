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
messages=function(lang="French"){
	msg=list()
# RequeteODBC
	msg$RequeteODBC.1<-"Error ODBC =>Define a vector baseODBC with the name of the ODBC link, user and password \n"
	msg$RequeteODBC.2<-"Connection trial :"
	msg$RequeteODBC.3<-"Connection failure :"
	msg$RequeteODBC.4<-"Connection successful \n"
	msg$RequeteODBC.5<-"Trying query \n"
	msg$RequeteODBC.6<-"Query succeded \n"
# RequeteODBCwhere
# RequeteODBCwheredate

	assign("msg",msg,envir=envir_stacomi)
}
