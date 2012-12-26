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
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
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
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
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
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
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
			miettes=miettes[!miettes%in%c("msg","datawd","sch","lang","baseODBC")]
			rm(list=miettes,envir=envir_stacomi)
		}      
	}
	if (length(ls(pattern="frame",envir=.GlobalEnv))!=0) {
		rm(list=ls(pattern="frame",envir=.GlobalEnv),envir=.GlobalEnv)
	}
	if (exists("g")) rm(g)
}

#' function used for some lattice graphes with dates 
#' @param vectordate date or POSIXt 
#' @returnType vector
#' @return vectordate (without class)
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
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
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
fun_char_spe<-function(text){
	text=gsub("é","e",text)
	text=gsub("è","e",text)
	text=gsub("ê","e",text)
	text=gsub("à","a",text)
	return(text)}
#' this function uses gfile, edits a text with info and changing colors
#' @param text 
#' @param arret 
#' @param wash 
#' @returnType 
#' @return nblignes assigned in .Global
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
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
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
chargexml=function(){ 
	library(XML)  # chargement du package XML
	options(guiToolkit = "RGtk2")
	# use of stringr package 
	# by default the xml file is in C:/Program Files/stacomi/ and we don't want to change that
	filexml<-"C:/Program Files/stacomi/calcmig.xml"
	#filexmlx86<-"C:/Program Files(x86)/stacomi/calcmig.xml" #windows7
	filexmlR=str_c(.libPaths(),"/stacomiR","/config/calcmig.xml")
	# str_c(R.version$major,R.vers
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

