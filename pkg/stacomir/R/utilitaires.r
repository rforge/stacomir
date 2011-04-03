# Nom fichier :        utilitaires.R
# Projet :             stacomiR


#############################################
# functions copied from Hmisc
#############################################
#' round.POSIXT function imported from depuis Hmisc that cannot be loaded for reasons
#' of compatibility with xtable
#' @param x 
#' @param digits 
#' @returnType POSIXct
#' @return a rounded time value
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
round.POSIXt<-function (x, digits = c("secs", "mins", "hours", "days", "months", 
				"years")) 
{
	if (is.numeric(digits) && digits == 0) 
		digits <- "secs"
	units <- match.arg(digits)
	month.length <- monthDays(x)
	x <- as.POSIXlt(x)
	if (length(x$sec) > 0) 
		switch(units, secs = {
					x$sec <- x$sec + 0.5
				}, mins = {
					x$sec <- x$sec + 30
				}, hours = {
					x$sec <- 0
					x$min <- x$min + 30
				}, days = {
					x$sec <- 0
					x$min <- 0
					x$hour <- x$hour + 12
					isdst <- x$isdst <- -1
				}, months = {
					x$sec <- 0
					x$min <- 0
					x$hour <- 0
					x$mday <- x$mday + trunc(monthDays(x)/2)
					isdst <- x$isdst <- -1
				}, years = {
					x$sec <- 0
					x$min <- 0
					x$hour <- 0
					x$mday <- 0
					x$mon <- x$mon + 6
					isdst <- x$isdst <- -1
				})
	return(trunc(as.POSIXct(x), units = units))
}
#' trunc.POSIXT function imported from depuis Hmisc that cannot be loaded for reasons
#' of compatibility with xtable
#' @param x 
#' @param digits 
#' @returnType POSIXlt
#' @return a truncated time value
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
trunc.POSIXt<-function (x, units = c("secs", "mins", "hours", "days", "months", 
				"years"), ...) 
{
	units <- match.arg(units)
	x <- as.POSIXlt(x)
	isdst <- x$isdst
	if (length(x$sec) > 0) 
		switch(units, secs = {
					x$sec <- trunc(x$sec)
				}, mins = {
					x$sec <- 0
				}, hours = {
					x$sec <- 0
					x$min <- 0
				}, days = {
					x$sec <- 0
					x$min <- 0
					x$hour <- 0
					isdst <- x$isdst <- -1
				}, months = {
					x$sec <- 0
					x$min <- 0
					x$hour <- 0
					x$mday <- 1
					isdst <- x$isdst <- -1
				}, years = {
					x$sec <- 0
					x$min <- 0
					x$hour <- 0
					x$mday <- 1
					x$mon <- 0
					isdst <- x$isdst <- -1
				})
	x <- as.POSIXlt(as.POSIXct(x))
	if (isdst == -1) {
		x$isdst <- -1
	}
	return(x)
}

###########################################
# functions used by the graphical interface
#############################################
#' function used to clean the objects whithin the group and the graphes
#' and also elements remaining in the envir_stacomi environment
#' @param ... 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
quitte=function(...){
	if (exists("ggroupboutonsbas")) delete(ggroupboutons,ggroupboutonsbas)
	if (exists("group")) {
		delete(ggroupboutons,group) 
		rm(group,envir= .GlobalEnv)
	}
	if (exists("graphes")) {
		delete(ggrouptotal1, graphes) 
		rm(graphes,envir= .GlobalEnv)
	}
	if (exists("envir_stacomi")){
		miettes=ls(envir=envir_stacomi)
		if (length(miettes)> 0 ) {
			miettes=miettes[!miettes%in%c("msg")]
			rm(list=miettes,envir=envir_stacomi)
		}      
	}
	if (length(ls(pattern="frame",envir=.GlobalEnv))!=0) {
		rm(list=ls(pattern="frame",envir=.GlobalEnv),envir=.GlobalEnv)
	}
}

#' function used for some lattice graphes with dates 
#' @param vectordate date or POSIXt 
#' @returnType vector
#' @return vectordate (without class)
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
graphdate<-function(vectordate){
	vectordate<-as.POSIXct(vectordate)
	attributes(vectordate)<-NULL
	unclass(vectordate)
	return(vectordate)
}

#' function used to remove special non utf8 character which cause the gtk interface to crash 
#' @param text 
#' @returnType character
#' @return text
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
fun_char_spe<-function(text){
	text=gsub("e","e",text)
	text=gsub("e","e",text)
	text=gsub("e","e",text)
	text=gsub("à","a",text)
	return(text)}
#' this function uses gfile, edits a text with info and changing colors
#' @param text 
#' @param arret 
#' @param wash 
#' @returnType 
#' @return nblignes assigned in .Global
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
funout<-function(text,arret=FALSE,wash=FALSE){
	if (exists("gSortie")) {
	if (wash) dispose(gSortie)
	nbligne=nbligne+1
	text<-fun_char_spe(text)
	add(gSortie,text,do.newline=FALSE,font.attr=list(style="italic", 
					col=col.sortie[nbligne],family="monospace",sizes="medium"),where="beginning")
	if (nbligne==20) nbligne=1
	nbligne<<-nbligne
	} 
	# this is printed anyway
	if(arret) stop(text) else print(text)
}
#' chargexml loads the informations stored in c:/program files/stacomi/calcmig.xml file
#' @returnType list
#' @return a list with the datawd place and the baseODBC vector
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
chargexml=function(){ 
	library(XML)  # chargement du package XML
	options(guiToolkit = "RGtk2")
	# use of stringr package 
	# by default the xml file is in C:/Program Files/stacomi/ and we don't want to change that
	filexml<-"C:/Program Files/stacomi/calcmig.xml"
	#filexmlx86<-"C:/Program Files(x86)/stacomi/calcmig.xml" #windows7
	filexmlR=str_c("C:/Program Files/R/R-2.12.1/library/stacomiR","/config/calcmig.xml")
	test<-expression(doc<-(xmlInternalTreeParse(filexml)))
	# if the file does not open, we will switch to the file located within the package
	doc<-try(eval(test), TRUE)
	if (class(doc)[1]=="try-error") {
		# then we test using the file from the package in the config folder
		test<-expression(doc<-(xmlInternalTreeParse(filexmlR)))
		doc<-try(eval(test), FALSE)
	}		
	doc=xmlRoot(doc)   # vire les infos d'ordre generales
	tableau_config = xmlSApply(doc, function(x) xmlSApply(x, xmlValue)) # renvoit une liste
	datawd=tableau_config["datawd",]
	lang=tableau_config["lang",]
#pgwd=tableau_config["pgwd",]
	baseODBC=c(tableau_config["lienODBC",],tableau_config["uid",],tableau_config["pwd",])
	return(list("datawd"=datawd,"baseODBC"=baseODBC,"lang"=lang))
}

#' function used to print the html tables of output (see xtable documentation)
#' @param data a data frame
#' @param caption the caption
#' @param top of top the caption is placed on top
#' @param outfile outfile is the path to the file
#' @param clipboard if clipboard TRUE, a copy to the clipboard is made
#' @param append is the file appended to the previous one ?
#' @param digits 
#' @param ... 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
funhtml=function(data,caption=NULL,top=TRUE,outfile=NULL,clipboard=FALSE,append=TRUE,digits=NULL,...){
	data[is.na(data)]<-""
	xt=xtable(data, caption=caption,digits=digits)
	xt=print(xt,type="html",caption.placement="top",file=outfile)
	# pour changer le defaut "bottom" des caption
	if (clipboard) writeClipboard(xt) 
} 
###########################################
# special functions (exported as they are usefull
#############################################
#' This function replaces the variable names in a data.frame
#' @param objet a data frame
#' @param old_variable_name 
#' @param new_variable_name 
#' @returnType data.frame
#' @return objet
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
chnames=function(objet,
		old_variable_name,
		new_variable_name){
		if (length(old_variable_name)!=length(new_variable_name)) stop("les variables de remplacement doivent avoir le meme nombre que les variables de depart")
		if (!all(!is.na(match(old_variable_name,colnames(objet))))) {
		   stop(paste("les noms",paste(is.na(match(old_variable_name,colnames(objet))),collapse="/"),"ne correspondent pas aux variables du tableau"))
    }
	colnames(objet)[match(old_variable_name,colnames(objet))]<- new_variable_name
	return(objet)
}

#' fonction pour renvoyer les index dans b  des valeurs du vecteur a
#' b peut apparaitre plusieurs fois dans a
#' @note attention le vecteur de resultat est dans le desordre
#' @param a 
#' @param b 
#' @returnType vector
#' @return index of b in a
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
ind=function(a,b){
	index=(1:length(b))[b%in%a]
	return(index)
}

#' fonction qui retourne l'index des valeurs repetees d'un vecteur
#' @param a 
#' @returnType vector
#' @return the index of repeated values within a vector
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
indrepeated=function(a){
	sol=match(unique(a),a)     #index des valeurs uniques
	rep=a[-sol]  # valeurs repetees
	return(ind(rep,a))   # index des valeurs repetees
}
#' fonction qui renvoit l'index des valeurs apparaissant une seule fois
#' @param a 
#' @returnType vector
#' @return the index unique  values within a vector
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
induk=function(a){
	sol=match(unique(a),a)     #index des valeurs uniques
	return(sol)   
}


#' very usefull function used to "kill" these bloody factors that appears, noticeably after loading with odbc
#' @param df a data.frame
#' @returnType data.frame
#' @return df
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
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
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
ex<-function(d=NULL){
	if (is.null(d)){
		xl=tk_select.list(ls(envir=globalenv()), preselect = NULL, multiple = FALSE, title = "choisir l'objet")
		write.table(get(xl),"clipboard",sep="\t",col.names=NA)
	} else {
		write.table(d,"clipboard",sep="\t",col.names=NA)
	}
}



#' id.odd function modified from package sma (which did not verified that the entry was indeed an integer)
#' @param x 
#' @returnType logical
#' @return a logical
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
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
#' @returnType logical
#' @return a logical
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
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
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
tab2df=function(tab){
	if (length((attributes(tab)$dim))>2) stop("ne fonctionne qu'avec des ftable de dim 2")
	df=as.data.frame(matrix(as.vector(tab),nrow(tab),ncol(tab)))
	rownames(df)<-attributes(tab)$row.vars[[1]]
	colnames(df)<-attributes(tab)$col.vars[[1]]	
	return(df)
}

