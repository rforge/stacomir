# Nom fichier :        utilitaires.R
# Projet :             stacomiR


#############################################
# functions copied from Hmisc
#############################################
#' function used to print the html tables of output (see xtable documentation)
#' @param data a data frame
#' @param caption the caption
#' @param top of top the caption is placed on top
#' @param outfile outfile is the path to the file
#' @param clipboard if clipboard TRUE, a copy to the clipboard is made
#' @param append is the file appended to the previous one ?
#' @param digits 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
funhtml=function(data,caption=NULL,top=TRUE,outfile=NULL,clipboard=FALSE,append=TRUE,digits=NULL,...){
	
	xt=xtable::xtable(data, caption=caption,digits=digits)
	xt=print(xt,type="html",caption.placement="top",file=outfile)
	# pour changer le defaut "bottom" des caption
	if (clipboard) utils::writeClipboard(xt) 
} 
###########################################
# special functions (exported as they are usefull
#############################################
#' This function replaces the variable names in a data.frame
#' @param object a data frame
#' @param old_variable_name 
#' @param new_variable_name 
#' @return object
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
chnames=function(object,
		old_variable_name,
		new_variable_name){
		if (length(old_variable_name)!=length(new_variable_name)) stop("les variables de remplacement doivent avoir le meme nombre que les variables de depart")
		if (!all(!is.na(match(old_variable_name,colnames(object))))) {
		   stop(paste("les noms",paste(is.na(match(old_variable_name,colnames(object))),collapse="/"),"ne correspondent pas aux variables du tableau"))
    }
	colnames(object)[match(old_variable_name,colnames(object))]<- new_variable_name
	return(object)
}

# fonction qui retourne l'index des valeurs repetees d'un vecteur
# see duplicated

#' fonction qui renvoit l'index des valeurs apparaissant une seule fois
#' @param a 
#' @return the index unique  values within a vector
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
induk=function(a){
	sol=match(unique(a),a)     #index des valeurs uniques
	return(sol)   
}


#' very usefull function used to "kill" the factors, noticeably after loading with odbc
#' 
#' @param df a data.frame
#' @return df
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
killfactor=function(df){
	for (i in 1:ncol(df))
	{
		if(is.factor(df[,i])) df[,i]=as.character(df[,i])
	}
	return(df)
}

#' ex fonction to write to excel, not used within the program but can still be used
#' @param d 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
ex<-function(d=NULL){
	if (is.null(d)){
		xl=utils::select.list(choices=ls(envir=globalenv()), preselect = NULL, multiple = FALSE, title = "choisir l'object")
		utils::write.table(get(xl),"clipboard",sep="\t",col.names=NA)
	} else {
		utils::write.table(d,"clipboard",sep="\t",col.names=NA)
	}
}



#' id.odd function modified from package sma (which did not verify that the entry was indeed an integer)
#' @param x 
#' @return a logical
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
is.odd=function (x) 
{
    if (x==as.integer(x)) {
        if (x%%2 == 0) {
            return(FALSE)
        }
        else {
            return(TRUE)
        }
    }
    else {
        stop("is.odd should be used with an integer")
    }
}
#' is.even function modified from package sma (which did not verified that the entry was indeed an integer)
#' @param x 
#' @return a logical
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
is.even=function (x) 
{
    if (x==as.integer(x)) {
        if (x%%2 != 0) {
            return(FALSE)
        }
        else {
            return(TRUE)
        }
    }
    else {
        stop("is.even should be used with an integer")
    }
}

#' Function to transform a ftable into dataframe but just keeping the counts works with ftable of dim 2
#' @param tab 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
tab2df<-function(tab){
	if (length((attributes(tab)$dim))>2) stop("only works with tables of dim 2")
	df=as.data.frame(matrix(as.vector(tab),nrow(tab),ncol(tab)))
	rownames(df)<-attributes(tab)$row.vars[[1]]
	colnames(df)<-attributes(tab)$col.vars[[1]]	
	return(df)
}
#' Function loaded in this package to avoid errors, if the package is called without stacomiR
#' @param text The text to display
#' @param arret Boolean should the program stop
#' @param wash= FALSE only used when called from within stacomiR, and there is a widget interface,
#' kept there for consistency 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
funout<-function(text,arret=FALSE,wash=FALSE){
	if(arret) stop(text) else print(text,quote=FALSE)
}