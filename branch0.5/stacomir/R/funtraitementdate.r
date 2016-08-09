# Nom fichier :        funtraitementdate.R    (classe)







#' This function extracts temporal characteristics from a dataframe
#' 
#' This function extracts temporal characteristics from a dataframe
#' 
#' 
#' @param data a data frame containing a Date or POSIXt column
#' @param nom_coldt the name of the column containing date or POSIXt entry to
#' be processed
#' @param annee logical do you want a column describing year to be added to the
#' dataframe
#' @param mois logical, add column with month
#' @param quinzaine logical, add column with 15 days
#' @param semaine logical, add column with weeks
#' @param jour_an logical, add column with day of year
#' @param jour_mois logical, add column with day of month
#' @param heure logical, add column with hour
#' @return data
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
funtraitementdate=function(data, # tableau de donnees à importer
		nom_coldt, # nom de la colonne
		annee=TRUE,
		mois=TRUE,
		quinzaine=FALSE,
		semaine=TRUE,
		jour_an=FALSE,
		jour_mois=TRUE,
		heure=FALSE                           
		){
	if (annee) data$annee=as.factor(strftime(as.POSIXlt(data[,nom_coldt]),format="%Y"))                        
	if (mois) data$mois= as.factor(strftime(as.POSIXlt(data[,nom_coldt]),format="%m"))
	# %b Abbreviated month name in the current locale. (Also matches full name on input.)
	if (quinzaine) {data$quinzaine=ceiling(as.numeric(strftime(as.POSIXlt(data[,nom_coldt]),format="%W"))/2)
	data$quinzaine=as.character(data$quinzaine)
	data$quinzaine[as.numeric(data$quinzaine)<10]= paste("0",data$quinzaine[as.numeric(data$quinzaine)<10],sep="")
	data$quinzaine=as.factor(data$quinzaine)}
	if (semaine) data$semaine=as.factor(strftime(as.POSIXlt(data[,nom_coldt]),format="%W"))
	#%W : Week of the year as decimal number (00–53) using Monday as the first day of week (and typically with the first Monday of the year as day 1 of week 1). The UK convention
	if (jour_an) data$jour_365=strftime(as.POSIXlt(data[,nom_coldt]),format="%j")                          
	if (jour_mois)data$jour_mois=as.factor(strftime(as.POSIXlt(data[,nom_coldt]),format="%d"))  
	# %d :  Day of the month as decimal number (01–31).
	if (heure)data$jour_mois=as.factor(strftime(as.POSIXlt(data[,nom_coldt]),format="%H"))  
	#%H     Hours as decimal number (00–23).    
	return(data)
}              

