#' RefMsg referential class to load message according to the language chosen
#' 
#' @slot data A data.frame
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setClass(Class="RefMsg",representation= representation(messager="data.frame",messagerlang="data.frame" ))


#' Loading method for RefMsg referential objects
#' 
#' loads the common table ts_messager_msr
#' @return An S4 object of class RefMsg
#' @family Referential objects
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("RefMsg")
#'  charge(object)
#' }
setMethod("charge",signature=signature("RefMsg"),definition=function(object) {
			req=new("RequeteODBC")
			req@baseODBC<-get("baseODBC",envir=envir_stacomi)
			req@sql="SELECT * from ref.ts_messager_msr  ORDER BY msr_id ASC ;"
			req<-stacomirtools::connect(req)  # appel de la methode connect de l'object requeteODBC
			object@messager<-req@query
			return(object)
		})

#' Loading method for RefMsg referential objects
#' 
#'  searching ref.ts_messagerlang_mrl for the lines corresponding to lang
#' @return An S4 object of class RefMsg
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("RefMsg")
#'  charge_avec_filtre(object,lang='French')
#' }
setMethod("charge_avec_filtre",signature=signature("RefMsg"),definition=function(object,lang) {
			requete=new("RequeteODBCwhere")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
			requete@select=stringr::str_c("SELECT mrl_id,mrl_msr_id,	mrl_text", 
					" FROM ref.ts_messagerlang_mrl")
			requete@where=stringr::str_c("where mrl_lang='",lang,"'")
			requete@order_by="ORDER BY mrl_msr_id ASC"  
			requete<-stacomirtools::connect(requete)  
			object@messagerlang<-requete@query
			return(object)
		})

#' createmessage method for RefMsg referential objects 
#' @param ojbect An object of class RefMsg
#' @param database_expected Default to TRUE, if FALSE, no database connection is expected and the messages will be loaded from msg dataset within the package
#' @return An S4 object of class RefMsg
#' @note When coming from the database, doublequotes are now escaped with an antislash (/"), those at the beginning and end are left as doublequotes, 
#' the others are single quote when they are to be pasted within the text as code example. The remainder "c("a","b","c") are rebuilt into vectors by the function
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
#' @keywords internal
setMethod("createmessage",signature=signature("RefMsg"),definition=function(object,database_expected) {
			if (database_expected){
				#object=new("RefMsg")
				object<-charge(object)
				object<-charge_avec_filtre(object,lang=get("lang",envir=envir_stacomi))
				if (nrow(object@messager)!=nrow(object@messagerlang)) stop("internal error, check messager and messagerlang length, they should match")
				msg=list()
				buildmsg<-merge(object@messager,object@messagerlang,by.x="msr_id",by.y="mrl_msr_id")
				buildmsg$msr_endofline2<-ifelse(as.logical(buildmsg$"msr_endofline"),"\n","")
				buildmsg1<-apply(buildmsg,1,function(X)stringr::str_c(X["msr_element"],
									".",
									as.integer(X["msr_number"])))
				# special case for graphical interface which contains number like 2.1 ...
				buildmsg1[buildmsg$msr_element=="interface_graphique_menu"]<-apply(buildmsg[buildmsg$msr_element=="interface_graphique_menu",],1,function(X)stringr::str_c(X["msr_element"],
									".",
									as.character(X["msr_number"])))
				buildmsg1<-gsub(' ', '', buildmsg1)
				buildmsg2<-apply(buildmsg,1,function(X)stringr::str_c(
									X["mrl_text"],
									X["msr_endofline2"]))
				nettoye<-function(X){
					X<-gsub(".00","",X)		
					X<-gsub("0","",X)
					return(X)
				}
				buildmsg1[buildmsg$msr_element=="interface_graphique_menu"]<-nettoye(buildmsg1[buildmsg$msr_element=="interface_graphique_menu"])
				
				
				
				# internal problem patch
				buildmsg1[match("interface_graphique_menu.2.9",buildmsg1)+1]<-"interface_graphique_menu.2.10"
				# here I'm dealing with "\" but only at the beginning and ending of strings
				list<-gregexpr("(\")", buildmsg2)
				for (i in 1:length(buildmsg2)){
					if (length(list[[i]])<=2) buildmsg2[i]<-gsub("\"","", buildmsg2[i])						
				}
				# dealing with the special problems of vectors which I want as vector c("toto","titi") and not as "c(toto,titi)" 
				# 
				create_vector<-function(m){
					# expressions contenant get
					index<-grep("(c\\()",m)
					# debug j<-index[1]
					for (j in index){
						list<-gregexpr("(\")", m[[j]])
						# getting vector elements and coalescing them in a vector
						m[[j]]<-do.call(c,lapply(2*(1:(length(list[[1]])/2))-1,function(i){
											substring(m[[j]],list[[1]][i]+1,list[[1]][i+1]-1)
										}))
					}
					return(m)
					
				}
				
				for (i in 1:length(buildmsg1)){
					msg[buildmsg1[i]]<-buildmsg2[i]
				}
				msg<-create_vector(msg)
				assign("msg",msg,envir=envir_stacomi)
			} else	{ # !database_expected
				utils::data("msg")
				assign("msg",msg,envir=envir_stacomi)
			}
			invisible(NULL)
		})
