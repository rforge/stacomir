# Nom fichier :        funBilanMigrationAnnuel
# Projet :             calcmig/prog/fonctions
# Organisme :          IAV/CSP
# Auteur :             Cedric Briand
# Contact :            cedric.briand"at"eptb-vilaine.fr
# Date de creation :   23/05/2022
# Compatibilite :      R 2.14
# Etat :               fonctionne
# Description          Workhorse fonction pour le calcul des bilans migratoires 
#**********************************************************************
#*
#* Modifications :







#' this functions performs the sum over the year attention this function does
#' not count subsamples.
#' 
#' this functions performs the sum over the year attention this function does
#' not count subsamples.
#' 
#' 
#' @param bilanMigration an object of class \code{\linkS4class{BilanMigration}}
funBilanMigrationAnnuel=function(bilanMigration) {
	# *********************
	# Boucle sur chacune des periodes du pas de temps
	# *********************
	req=new("RequeteODBC")
	req@baseODBC<-get("baseODBC", envir=envir_stacomi)
	##############################			
	##############################"  
	dateDebut=strftime(as.POSIXlt(bilanMigration@pasDeTemps@dateDebut),format="%Y-%m-%d %H:%M:%S")
	dateFin=strftime(as.POSIXlt(DateFin(bilanMigration@pasDeTemps)),format="%Y-%m-%d %H:%M:%S")
	year=as.numeric(strftime(as.POSIXlt(bilanMigration@pasDeTemps@dateDebut),format="%Y"))
	dcCode = as.character(bilanMigration@dc@dc_selectionne)
  	req@sql = paste(" select sum(lot_effectif) as effectif from ",get("sch",envir=envir_stacomi),"t_operation_ope ",
					" join ",get("sch",envir=envir_stacomi),"t_lot_lot on lot_ope_identifiant=ope_identifiant where ope_dic_identifiant=",dcCode,
					" and extract(year from ope_date_debut)=", year,
					" and lot_tax_code='",bilanMigration@taxons@data$tax_code,
					"' and lot_std_code='",bilanMigration@stades@data$std_code,
					"' and lot_lot_identifiant is null",
					" ;",sep="" )
			req<-stacomirtools::connect(req)
			rs=req@query			
	return (rs)

}              
