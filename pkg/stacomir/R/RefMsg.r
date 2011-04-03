# Nom fichier :        RefMsg   (classe)
# Projet :             controle migrateur
# Organisme :          IAV
# Auteur :             Cedric Briand
# Contact :            cedric.briand@lavilaine.com
# Date de creation :   31/03/2008 17:21:30

#' @title Refstades referential class to load message according to the langage chosen
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @slot data="data.frame"
#' @example objet=new("RefMsg")
setClass(Class="RefMsg",representation= representation(messager="data.frame",messagerlang="data.frame" ))
#' Loading method for RefMsg referential objects
#' loads the common table ts_messager_msr
#' @returnType S4 object
#' @return An S4 object of class RefMsg
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @example 
#'  objet=new("RefMsg")
#'  charge(objet)
setMethod("charge",signature=signature("RefMsg"),definition=function(objet) {
			req=new("RequeteODBC") 
			req@sql="SELECT * from ref.ts_messager_msr  ORDER BY msr_id ASC ;"
			req=connect(req)  # appel de la methode connect de l'objet requeteODBC
			objet@messager<-req@query
			return(objet)
		})

#' Loading method for RefMsg referential objects searching ref.ts_messagerlang_mrl for the lines corresponding to lang
#' @returnType S4 object
#' @return An S4 object of class RefMsg
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @exportMethod
#'  objet=new("RefMsg")
#'  charge_avec_filtre(objet,lang=lang)
setMethod("charge_avec_filtre",signature=signature("RefMsg"),definition=function(objet,lang) {
			requete=new("RequeteODBCwhere")
			requete@baseODBC=baseODBC
			requete@select=str_c("SELECT mrl_id,mrl_msr_id,	mrl_text", 
					" FROM ref.ts_messagerlang_mrl")
			requete@where=str_c("where mrl_lang='",lang,"'")
			requete@order_by="ORDER BY mrl_msr_id ASC"  
			requete=connect(requete)  
			objet@messagerlang<-requete@query
			return(objet)
		})
#' createmessage method for RefMsg referential objects 
#' @returnType S4 object
#' @return An S4 object of class RefMsg
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @exportMethod
#'  dc_selectionne=6
#'  objet=new("RefMsg")
#'  charge_avec_filtre(objet,lang=lang)
setMethod("createmessage",signature=signature("RefMsg"),definition=function(objet) {
			objet<-charge(objet)
			objet<-charge_avec_filtre(objet,lang=get("lang",envir=envir_stacomi))
			if (nrow(objet@messager)!=nrow(objet@messagerlang)) stop("internal error, check messager and messagerlang length, they should match")
			msg=list()
			buildmsg<-merge(objet@messager,objet@messagerlang,by.x="msr_id",by.y="mrl_msr_id")
			buildmsg$msr_endofline2<-ifelse(as.logical(buildmsg$"msr_endofline"),"\n","")
			buildmsg1<-apply(buildmsg,1,function(X)str_c(X["msr_element"],
								".",
								as.character(X["msr_number"])))
			buildmsg2<-apply(buildmsg,1,function(X)str_c(
								X["mrl_text"],
								X["msr_endofline2"]))
			nettoye<-function(X){
				X<-gsub(".00","",X)		
				X<-gsub(' ', '', X)
				X<-gsub("0","",X)
				return(X)
			}
			# le interface_graphique_menu.2.10 est là deux fois, il faut le remettre à la main
			buildmsg1<-nettoye(buildmsg1)
			# le interface_graphique_menu.2.1 est là deux fois, il faut le remettre le 2.10 à la main
			buildmsg1[match("interface_graphique_menu.2.9",buildmsg1)+1]<-"interface_graphique_menu.2.10"
			buildmsg2<-gsub("\"", "",buildmsg2)
			for (i in 1:length(buildmsg1)){
				msg[buildmsg1[i]]<-buildmsg2[i]
			}
			assign("msg",msg,envi=envir_stacomi)
			return(NULL)
		})
