#' This writes monthly data in t_bilanmensuel_mens table
#' 
#' @note This function is launched by fun_EcritBilanJournalier, the resum 
#' dataset is created by the \link{funstat} function
#' 
#' 
#' @param bilanMigration an object of class \code{\linkS4class{BilanMigration}}
#' @param resum data frame with summary per month
#' @export
fn_EcritBilanMensuel<-function(bilanMigration,resum){
	# voir essai_table_bilanmensuel.sql pour le format du tableau
	# below not the most elegant way to do it but efficient
	
	t_bilanmigrationmensuel_bme=stacomirtools::killfactor(
			cbind(bilanMigration@dc@dc_selectionne,
					bilanMigration@taxons@data$tax_code,
					bilanMigration@stades@data$std_code,
					unique(strftime(as.POSIXlt(bilanMigration@time.sequence),"%Y")), # une valeur
					rep(rownames(resum),(ncol(resum)-2)), # nb of month except columns bilan and label
					stack(resum,select=c(2:(ncol(resum)-1))),# stack re-ordonne les tab de donnees !  
					format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
					substr(toupper(get("sch",envir=envir_stacomi)),1,nchar(toupper(get("sch",envir=envir_stacomi)))-1)
			)
	)
	
	# la requete pour la suppression
	requete=new("RequeteODBC")
	requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	requete@open<-TRUE # on laisse la base ouverte
	
	# ecriture dans la base...
	for (i in 1:nrow(t_bilanmigrationmensuel_bme)) {
		requete@sql=paste("INSERT INTO ",get("sch",envir=envir_stacomi),"t_bilanMigrationMensuel_bme (",			
				"bme_dis_identifiant,bme_tax_code,bme_std_code,bme_annee,bme_labelquantite,bme_valeur,bme_mois,bme_horodateexport,bme_org_code)",
				" VALUES ('",paste(t_bilanmigrationmensuel_bme[i,],collapse="','"),"');",sep="")
		requete<-stacomirtools::connect(requete)   
	} # end for
	odbcClose(requete@connection)
funout(paste(get("msg",envir=envir_stacomi)$fn_EcritBilanMensuel.1,"\n"))	
} # end function

