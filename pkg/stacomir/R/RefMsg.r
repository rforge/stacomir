# Nom fichier :        RefMsg   (classe)
# Projet :             controle migrateur
# Organisme :          IAV
# Auteur :             Cedric Briand
# Contact :            cedric.briand00@gmail.com
# Date de creation :   31/03/2008 17:21:30

#' @title Refstades referential class to load message according to the langage chosen
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @slot data="data.frame"
#' @expamples objet=new("RefMsg")
setClass(Class="RefMsg",representation= representation(messager="data.frame",messagerlang="data.frame" ))
#' Loading method for RefMsg referential objects
#' loads the common table ts_messager_msr
#' @returnType S4 object
#' @return An S4 object of class RefMsg
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @expamples 
#'  objet=new("RefMsg")
#'  charge(objet)
setMethod("charge",signature=signature("RefMsg"),definition=function(objet) {
			req=new("RequeteODBC")
			req@baseODBC<-get("baseODBC",envir=envir_stacomi)
			req@sql="SELECT * from ref.ts_messager_msr  ORDER BY msr_id ASC ;"
			req=connect(req)  # appel de la methode connect de l'objet requeteODBC
			objet@messager<-req@query
			return(objet)
		})

#' Loading method for RefMsg referential objects searching ref.ts_messagerlang_mrl for the lines corresponding to lang
#' @returnType S4 object
#' @return An S4 object of class RefMsg
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @exportMethod
#'  objet=new("RefMsg")
#'  charge_avec_filtre(objet,lang='French')
setMethod("charge_avec_filtre",signature=signature("RefMsg"),definition=function(objet,lang) {
			requete=new("RequeteODBCwhere")
			requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
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
#' @note When coming from the database, " are now /", those at the beginning and end are turned into ", the others are single quote when they are to be pasted within the text as code example. The remainder "c("a","b","c") are rebuilt into vectors by the function
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @exportMethod
#'  objet=new("RefMsg")
setMethod("createmessage",signature=signature("RefMsg"),definition=function(objet) {
			objet<-charge(objet)
			objet<-charge_avec_filtre(objet,lang=get("lang",envir=envir_stacomi))
			if (nrow(objet@messager)!=nrow(objet@messagerlang)) stop("internal error, check messager and messagerlang length, they should match")
			msg=list()
			buildmsg<-merge(objet@messager,objet@messagerlang,by.x="msr_id",by.y="mrl_msr_id")
			buildmsg$msr_endofline2<-ifelse(as.logical(buildmsg$"msr_endofline"),"\n","")
			buildmsg1<-apply(buildmsg,1,function(X)str_c(X["msr_element"],
								".",
								as.integer(X["msr_number"])))
			# special case for graphical interface which contains number like 2.1 ...
			buildmsg1[buildmsg$msr_element=="interface_graphique_menu"]<-apply(buildmsg[buildmsg$msr_element=="interface_graphique_menu",],1,function(X)str_c(X["msr_element"],
								".",
								as.character(X["msr_number"])))
			buildmsg1<-gsub(' ', '', buildmsg1)
			buildmsg2<-apply(buildmsg,1,function(X)str_c(
								X["mrl_text"],
								X["msr_endofline2"]))
			nettoye<-function(X){
				X<-gsub(".00","",X)		
				X<-gsub("0","",X)
				return(X)
			}
			buildmsg1[buildmsg$msr_element=="interface_graphique_menu"]<-nettoye(buildmsg1[buildmsg$msr_element=="interface_graphique_menu"])
		
			
			
			# l' interface_graphique_menu.2.1 est l� deux fois, il faut le remettre le 2.10 � la main
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
			return(NULL)
		})
