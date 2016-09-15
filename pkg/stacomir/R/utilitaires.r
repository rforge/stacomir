#' function used to clean the objects whithin the group and the graphes and
#' also elements remaining in the envir_stacomi environment
#' 
#' 
#' @param \dots additional arguments passed to the function
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
quitte=function(...){
	if (exists("ggroupboutonsbas",envir=envir_stacomi)) delete(ggroupboutons,ggroupboutonsbas)
     rm
	if (exists("group",envir=envir_stacomi)) {
		delete(ggroupboutons,group) 
		rm(group,envir= .GlobalEnv)
	}

	if (exists("envir_stacomi")){
		miettes=ls(envir=envir_stacomi)
		if (length(miettes)> 0 ) {
			miettes=miettes[!miettes%in%c("msg","datawd","sch","lang","baseODBC","usrname","usrpwd")]
			rm(list=miettes,envir=.GlobalEnv)
		}      
	}
	if (length(ls(pattern="frame",envir=envir_stacomi))!=0) {
		rm(list=ls(pattern="frame",envir=envir_stacomi),envir=envir_stacomi)
	}
	if (exists("g")) rm(g)
}

#' function used for some lattice graphes with dates 
#' @param vectordate date or POSIXt 
#' @return vectordate (without class)
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
graphdate<-function(vectordate){
	vectordate<-as.POSIXct(vectordate)
	attributes(vectordate)<-NULL
	unclass(vectordate)
	return(vectordate)
}







#' function used to remove special non utf8 character which cause the gtk
#' interface to crash
#' 
#' 
#' @param text a text string which might contain no utf8 characters
#' @return text
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
fun_char_spe<-function(text){
	text=gsub("\u00e9","e",text) #é
	text=gsub("\u00e8","e",text) #è
	text=gsub("\u00ea","e",text) #ê
	text=gsub("\u00e0","a",text) #à
	return(text)}






#' this function uses gfile, edits a text with info and changing colors
#' 
#' 
#' @param text The text to display both in the gtk interface and in the R
#' console
#' @param arret Should this cause the program to stop ?
#' @param wash Should the console be cleared after displaying the message
#' @param ... Additional parameters passed to print
#' @return nblignes Assigned in .Global
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
#' @export
# internal= funout is exported to ease debug during tests but not showns to users
funout<-function(text,arret=FALSE,wash=FALSE,...){
	if (exists("gSortie",envir=.GlobalEnv)) {
		if (isExtant(gSortie)){
			if (wash) dispose(gSortie)
			nbligne=nbligne+1
			text<-fun_char_spe(text)
			add(gSortie,text,do.newline=FALSE,font.attr=list(style="italic", 
							col=col.sortie[nbligne],family="monospace",sizes="medium"),where="beginning")
			if (nbligne==20) nbligne=1
			nbligne<<-nbligne
		} else {
			# gSortie exists but has not been removed
			rm("gSortie",envir=.GlobalEnv)
		}
	} 
	# this is printed anyway
	if(arret) stop(text) else print(text,quote=FALSE,...)
}






#' chargecsv loads the informations stored in c:/program
#' files/stacomi/calcmig.csv file
#' 
#' be sure to configure your odbc link to the
#' database, the name is the name of the first column of the calcmig.csv file. 
#' 	\code{uid}, \code{pwd} are identifier and password to connect to the database, they should
#' correspond to your own schema in the database. \code{pgwd} is the path to the R
#' source if you plan not to use the compiler but run manually using inst/config/stacomi_manual_launch.R for development.\cr
#' \code{datawd}, is the
#' directory where you want to place the outputs, mostly tables, from the
#' program, default to ~//CalcmigData lang, is either one of French, English or
#' Spanish
#' 	other fields correspond to sqldf options.
#' @param database expected Are the program (stacomi directory) and database expected to be installed,
#' this argument is necessary to pass tests on system where stacomi is not installed (e.g. R-forge)
#' @note A version of the calcmig.csv is packaged in the config directory of the stacomiR library.
#' 
#' @return a list with the datawd place and the baseODBC vector
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
#' @keywords internal
chargecsv=function(database_expected){ 
	#library(XML)  # chargement du package XML
	options(guiToolkit = "RGtk2")
	# use of stringr package 
	# by default the csv file is in C:/Program Files/stacomi/ and we don't want to change that
	# note this will only be tested once the program is packages otherwise the path is inst/config/calcmig.csv
	
	
	if (database_expected) {
		filecsv<-"C:/Program Files/stacomi/calcmig.csv"
		test<-file.access(filecsv,0)==0
		if (test) {
			calcmig<-utils::read.csv(filecsv,header=TRUE,sep=";")
			# then we test using the file from the package in the config folder
		} else {
			# the access to csv file failed despite database_expected=true
			# if the file does not open, we switch to the file located within the package
			cat("C:/program files/calcmig.csv does not exist, switching to defaut package file")
			data("calcmig",envir=environment())				
		}
	} else {
		# no access to the database is expected, we are using the file in the data directory of the package
		data("calcmig",envir=environment())
		test<-FALSE
	}
	tableau_config = t(calcmig) # renvoit une liste
	datawd=tableau_config["datawd",]
	lang=tableau_config["lang",]
#pgwd=tableau_config["pgwd",]
	baseODBC=c(tableau_config["lienODBC",],tableau_config["uid",],tableau_config["pwd",])
	sqldf.options=c(tableau_config["sqldf.uid",],tableau_config["sqldf.pwd",],tableau_config["sqldf.dbname",],tableau_config["sqldf.host",],tableau_config["sqldf.port",])
	return(list("datawd"=datawd,"baseODBC"=baseODBC,"lang"=lang,"sqldf.options"=sqldf.options))
}






#' Transforms a vector into a string called within an sql command e.g.
#' c('A','B','C') => in ('A','B','C')
#' 
#' Transforms a vector into a string called within an sql command e.g. c(A,B,C)
#' => in ('A','B','C')
#' 
#' 
#' @param vect a character vector
#' @return listsql a list of value
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
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


#' Progress bar using a gtkdialog, the progress bar is assigned in .GlobalEnv
#' This progress bar has a button to close.
#' @note The name of the progress bar is \code{progres}, it will be assigned in .GlobalEnv,
#' it contains a progress bar widget named progress bar, also assigned in .GLobalEnv. See example for use.
#' It seems that when sys.sleep is not used it is necessary to use RGtk2::gtkMainIterationDo(FALSE) for a regular display
#' @param title The title of the bar
#' @param progress_text The text to display for progression
#' @param width Width of the progress bar
#' @param height Height of the progress bar
#' @param pulse Do you want the widget to pulse
#' @return nothing but progress_bar and dialog gwidgets are assigned to .GlobalEnv
#' 
#' @author cedric.briand
#' @examples 
#' \dontrun{
#' mygtkProgressBar(title="Trial",progress_text="progress text")
#' fraction_progressed=seq(0,1,length.out=50)
#' for(i in fraction_progressed) {
#'      Sys.sleep(0.1)
#'    progress_bar$setText(sprintf("%d%% progression",round(100*i)))
#'     progress_bar$setFraction(i)
#' }
#'dispose(progres)
#' }
mygtkProgressBar<-function(title,progress_text,width=400,height=50,pulse=TRUE){
	.dialog <- RGtk2::gtkDialog(title=title, NULL, NULL,
			"gtk-close", RGtk2::GtkResponseType["none"],
			show = FALSE)
	assign("progres",.dialog,envir=envir_stacomi)
	## Ensure that the dialog box is destroyed when the user responds.
	RGtk2::gSignalConnect(.dialog, "response", RGtk2::gtkWidgetDestroy)	
	progress_bar <- RGtk2::gtkProgressBar()
	assign("progress_bar",progress_bar,.GlobalEnv)
	RGtk2::gtkWidgetSetSizeRequest(progress_bar,width=width,height=height)
	.dialog[["vbox"]]$add(progress_bar)
	progress_bar$setText(progress_text)
	if (pulse) RGtk2::gtkProgressBarPulse(progress_bar)
	.dialog$showAll()
	return(invisible(NULL))
}


