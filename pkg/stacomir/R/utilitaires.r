# Nom fichier :        utilitaires.R
# Projet :             stacomiR


#############################################
# functions copied from Hmisc
#############################################
monthDays<-
		function (time) 
{
	time <- as.POSIXlt(time)
	time$mday[] <- time$sec[] <- time$min <- time$hour <- 0
	time$mon <- time$mon + 1
	return(as.POSIXlt(as.POSIXct(time))$mday)
}
#' round.POSIXT function imported from  Hmisc that cannot be loaded
#' @param x 
#' @param digits 
#' @returnType POSIXct
#' @return a rounded time value
#' @author Charles Dupont
round.POSIXt<-function (x, digits = c("secs", "mins", "hours", "days", "months", 
				"years"),...) 
{
	if (is.numeric(digits) && digits == 0) 	digits <- "secs"
	units <- match.arg(digits)
	month.length <- monthDays(x)
	x <- as.POSIXlt(x)
	if (length(x$sec) > 0) 
		switch(units, secs = {
					x$sec <- x$sec
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
	return(trunc(as.POSIXct(x), digits = units))
}
#' trunc.POSIXT function imported from depuis Hmisc that cannot be loaded 
#' @param x 
#' @param digits 
#' @returnType POSIXlt
#' @return a truncated time value
#' @author Charles Dupont
trunc.POSIXt<-function (x, digits = c("secs", "mins", "hours", "days", "months", 
				"years"), ...) 
{
	#UseMethod("trunc.POSIXt")
	units <- match.arg(digits)
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
#' ceil.POSIXT function imported from depuis Hmisc that cannot be loaded 
#' @param x 
#' @param digits 
#' @returnType POSIXlt
#' @return a truncated time value
#' @author Charles Dupont
ceil.POSIXt<-function (x, digits = c("secs", "mins", "hours", "days", "months", 
				"years"), ...) 
{
	units <- match.arg(digits)
	x <- as.POSIXlt(x)
	isdst <- x$isdst
	if (length(x$sec) > 0 && x != trunc.POSIXt(x, digits = units)) {
		switch(units, secs = {
					x$sec <- ceiling(x$sec)
				}, mins = {
					x$sec <- 0
					x$min <- x$min + 1
				}, hours = {
					x$sec <- 0
					x$min <- 0
					x$hour <- x$hour + 1
				}, days = {
					x$sec <- 0
					x$min <- 0
					x$hour <- 0
					x$mday <- x$mday + 1
					isdst <- x$isdst <- -1
				}, months = {
					x$sec <- 0
					x$min <- 0
					x$hour <- 0
					x$mday <- 1
					x$mon <- x$mon + 1
					isdst <- x$isdst <- -1
				}, years = {
					x$sec <- 0
					x$min <- 0
					x$hour <- 0
					x$mday <- 1
					x$mon <- 0
					x$year <- x$year + 1
					isdst <- x$isdst <- -1
				})
		x <- as.POSIXlt(as.POSIXct(x))
		if (isdst == -1) {
			x$isdst <- -1
		}
	}
	return(x)
}

###########################################
# functions used by the graphical interface
#############################################
#' function used to clean the objects whithin the group and the graphes
#' and also elements remaining in the envir_stacomi environment
#' @param ... 
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
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
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
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
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
fun_char_spe<-function(text){
	text=gsub("\u00e9","e",text) #é
	text=gsub("\u00e8","e",text) #è
	text=gsub("\u00ea","e",text) #ê
	text=gsub("\u00e0","a",text) #à
	return(text)}
#' this function uses gfile, edits a text with info and changing colors
#' @param text 
#' @param arret 
#' @param wash 
#' @returnType 
#' @return nblignes assigned in .Global
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
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
#' chargecsv loads the informations stored in c:/program files/stacomi/calcmig.csv file
#' @returnType list
#' @return a list with the datawd place and the baseODBC vector
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
chargecsv=function(){ 
	#library(XML)  # chargement du package XML
	options(guiToolkit = "RGtk2")
	# use of stringr package 
	# by default the csv file is in C:/Program Files/stacomi/ and we don't want to change that
	filecsv<-"C:/Program Files/stacomi/calcmig.csv"
	# note this will only be tested once the program is packages otherwise the path is inst/config/calcmig.csv
	filecsvR=file.path(.libPaths(),"stacomiR","config","calcmig.csv")
	
	test<-file.access(filecsv,0)==0
	# if the file does not open, we will switch to the file located within the package
	if (test) {
		doc<-read.csv(filecsv,header=TRUE,sep=";")
		# then we test using the file from the package in the config folder
	} else {
		test2<-file.access(filecsvR,0)==0
		if (test2) {
			doc<-read.csv(filecsvR,header=TRUE,sep=";")
		} else {
			stop("internal error, no access to the csv configuration file")
		}		
	}
	
	
	tableau_config = t(doc) # renvoit une liste
	datawd=tableau_config["datawd",]
	lang=tableau_config["lang",]
#pgwd=tableau_config["pgwd",]
	baseODBC=c(tableau_config["lienODBC",],tableau_config["uid",],tableau_config["pwd",],tableau_config["host",],tableau_config["port",])
	return(list("datawd"=datawd,"baseODBC"=baseODBC,"lang"=lang))
}


#' Transforms a vector into a string called within an sql command  e.g. c(A,B,C) => in ('A','B','C')
#' @param vect 
#' @returnType character
#' @return listsql a list of value
#' @author Cedric Briand \email{cedric.briand@@eptb-vilaine.fr}
#' @export
vector_to_listsql<-function(vect)
{
	
	# vect = un vecteur caractere
	if (length(vect)==1) 
	{
		listsql=paste("(","'",vect,"'",")",sep="")
	}
	
	if (length(vect)>2)
	{
		listsql=paste("(","'",vect[1],"'",",", sep="")
		for(j in 2:(length(vect)-1)){
			listsql=paste(listsql,"'",vect[j],"'",",",sep="")
		}
		listsql=paste(listsql,"'",vect[length(vect)],"'",")", sep="")
	} 
	else if  (length(vect)==2)
	{
		listsql=paste("(","'",vect[1],"'",",", sep="")
		listsql=paste(listsql,"'",vect[length(vect)],"'",")", sep="") 
	}
	
	return(listsql)
} 
