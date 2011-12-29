# Nom fichier :        funstat.R

#' Function to calculate statistics per month
#' @param tableau 
#' @param duree 
#' @param taxon 
#' @param stade 
#' @param DC 
#' @returnType data.frame
#' @return resum
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
#' @export
funstat=function(tableau,duree,taxon,stade,DC){
	funout(get("msg",envir=envir_stacomi)$funstat.1)
	mois=strftime(as.POSIXlt(duree),"%m")
	moislab=unique(mois)
	annee=unique(strftime(as.POSIXlt(duree),"%Y"))
	somme=tapply(tableau$Effectif_total, mois, sum, na.rm=TRUE) # sums
	moyennes_journalieres=tapply(tableau$Effectif_total, mois, mean, na.rm=TRUE) # means
	#ecarts_types=tapply(tableau$Effectif_total, mois, sd, na.rm=TRUE) # std. deviations
	#nombre=as.integer(tapply(tableau$Effectif_total, mois, function(x) sum(!is.na(x)))) # counts
	resum=rbind(somme,moyennes_journalieres)#,moyennes_journalieres,ecarts_types,nombre)	
	if (taxon=="Anguilla anguilla"& stade=="civelle") 
	{
		poids_depuis_effectif=tapply(tableau$poids_depuis_effectif, mois,  sum, na.rm=TRUE)
		poids_mesure=tapply(tableau$Poids_total, mois,  sum, na.rm=TRUE)
		Poids_total=poids_depuis_effectif+ poids_mesure
		resum=rbind(somme,moyennes_journalieres,poids_depuis_effectif,poids_mesure,Poids_total)		
	}
	resum=resum[,moislab]
	resum=as.data.frame(resum)
	resum["somme","bilan"]=round(sum(tableau$Effectif_total, na.rm=TRUE),2)
	resum["moyennes_journalieres","bilan"]=mean(tableau$Effectif_total,na.rm=TRUE)
	#resum["moyennes_journalieres","bilan"]=round(mean(tableau$Effectif_total, na.rm=TRUE),2)
	#resum["ecarts_types","bilan"]=round(sd(tableau$Effectif_total, na.rm=TRUE),2)
	if (taxon=="Anguilla anguilla"& stade=="civelle") 
	{
		resum["poids_depuis_effectif","bilan"]=round(sum(tableau$poids_depuis_effectif, na.rm=TRUE),2)
		resum["poids_mesure","bilan"]=round(sum(tableau$Poids_total, na.rm=TRUE),2)
		resum["Poids_total","bilan"]=round(sum(Poids_total, na.rm=TRUE),2)
	}
	resum=cbind("label"=paste("DC",DC,taxon,stade,annee,sep="_"),resum)
 # funout(paste(DC,taxon,stade,annee,"\n"))
 	#funout(paste("DC","code_taxon","code_stade","annee","\n"))
  funout(get("msg",envir=envir_stacomi)$funstat.1)   	
	print( resum["somme",])
	return(resum)
}