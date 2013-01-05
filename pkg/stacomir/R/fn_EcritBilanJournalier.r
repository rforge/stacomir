# function fn_EcritBilanJournalier.r
#' fn_EcritBilanJournier writes the daily migration in the t_bilanmigrationjournalier_bjo table
#' @note the user is asked whether or not he wants to overwrite data, if no data are present in the database, the import is done anyway
#' @param bilanMigration 
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
fn_EcritBilanJournalier<-function(bilanMigration){
	# voir essai_table_bilanJournalier.sql pour le format du tableau
	# je cherche les colonnes que je ne veux pas retenir
	data=bilanMigration@data	
	jour_dans_lannee_non_nuls=strftime(bilanMigration@duree,'%Y-%m-%d %H:%M:%S')[data$Effectif_total!=0]
	data=data[data$Effectif_total!=0,]
	col_a_retirer=match(c("No.pas","Type_de_quantite"),colnames(data))
	data=data[,-col_a_retirer]
	data$"Taux_d_echappement"[data$Taux_d_echappement==-1]<-NA 
	data$Coef_conversion[data$"Coef_conversion"==1]<-NA 
	peuventpaszero=match(c("Taux_d_echappement","Coef_conversion"),colnames(data))
	data[,-peuventpaszero][data[,-peuventpaszero]==0]<-NA
	t_bilanmigrationjournalier_bjo=cbind(bilanMigration@dc@dc_selectionne,
			bilanMigration@taxons@data$tax_code,
			bilanMigration@stades@data$std_code,
			unique(strftime(as.POSIXlt(bilanMigration@duree),"%Y")),
			rep(jour_dans_lannee_non_nuls,ncol(data)),
			stack(data),  
			format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
			substr(toupper(get("sch",envir=envir_stacomi)),1,nchar(toupper(get("sch",envir=envir_stacomi)))-1)
	)
	t_bilanmigrationjournalier_bjo= killfactor(t_bilanmigrationjournalier_bjo[!is.na(t_bilanmigrationjournalier_bjo$values),])
	
	#####
	# Ci dessous conversion de la classe vers migration Interannuelle pour utiliser
	# les methodes de cette classe
	bil=as(bilanMigration,"BilanMigrationInterAnnuelle")
	bil=connect(bil)
	
	hconfirm=function(h,...){			
		# suppression des donnees actuellement presentes dans la base
		supprime(bil)			
		requete=new("RequeteODBC")
		requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
		requete@silent=TRUE
		requete@open=TRUE
		progres<-winProgressBar(title = get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.3,
				label = get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.4,
				min = 0,
				max = 1, 
				initial = 0,
				width = 400)
		for (i in 1:nrow(t_bilanmigrationjournalier_bjo)) {				
			zz=i/nrow(t_bilanmigrationjournalier_bjo)				
			setWinProgressBar(progres,
					zz,
					title=get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.5,
					label=sprintf("%d%% progression",
							round(100*zz)))     
			requete@sql=paste( "INSERT INTO ",get("sch",envir=envir_stacomi),"t_bilanmigrationjournalier_bjo (",			
					"bjo_dis_identifiant,bjo_tax_code,bjo_std_code,bjo_annee,bjo_jour,bjo_valeur,bjo_labelquantite,bjo_horodateexport,bjo_org_code)",
					" VALUES " ,"('",paste(t_bilanmigrationjournalier_bjo[i,],collapse="','"),"');",sep="")
			requete<-connect(requete) 
			
		} # end for
		funout(paste(get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.5,"\n"))
		# si l'utilisateur accepte de remplacer les valeurs
		close(progres)
		odbcClose(requete@connection)
		# ecriture �galement du bilan mensuel
		taxon= as.character(bilanMigration@taxons@data$tax_nom_latin)
		stade= as.character(bilanMigration@stades@data$std_libelle)
		DC=as.numeric(bilanMigration@dc@dc_selectionne)	
		resum=funstat(tableau=bilanMigration@data,duree=bilanMigration@duree,taxon,stade,DC )
		fn_EcritBilanMensuel(bilanMigration,resum)
	}#end function hconfirm
	
	if (nrow(bil@data)>0)
	{ 
		choix<-gconfirm(paste(get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.1, # Un bilan a deja ete ecrit dans la base
						unique(bil@data$bjo_horodateexport),
						get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.2),
				handler=hconfirm) # voulez vous le remplacer ?
		
		
	}
	else  # sinon on ecrit les resultats quoiqu'il arrive
	{
		requete=new("RequeteODBC")
		requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
		requete@silent=TRUE
		requete@open=TRUE
		progres<-winProgressBar(title = get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.5,
				label = "progression %",
				min = 0,
				max = 1, 
				initial = 0,
				width = 400)
		for (i in 1:nrow(t_bilanmigrationjournalier_bjo)) {
			zz=i/nrow(t_bilanmigrationjournalier_bjo)				
			setWinProgressBar(progres,
					zz,
					title=get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.5,
					label=sprintf("%d%% progression",
							round(100*zz)))
			requete@sql=paste( "INSERT INTO ",get("sch",envir=envir_stacomi),"t_bilanmigrationjournalier_bjo (",			
					"bjo_dis_identifiant,bjo_tax_code,bjo_std_code,bjo_annee,bjo_jour,bjo_valeur,bjo_labelquantite,bjo_horodateexport,bjo_org_code)",
					" VALUES " ,
					"('",paste(t_bilanmigrationjournalier_bjo[i,],collapse="','"),"');",sep="")
			requete<-connect(requete)   
		} # end for
		close(progres)
		odbcClose(requete@connection)
		funout(paste(get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.5,"\n"))
		taxon= as.character(bilanMigration@taxons@data$tax_nom_latin)
		stade= as.character(bilanMigration@stades@data$std_libelle)
		DC=as.numeric(bilanMigration@dc@dc_selectionne)	
		resum=funstat(tableau=bilanMigration@data,duree=bilanMigration@duree,taxon,stade,DC)
		fn_EcritBilanMensuel(bilanMigration,resum)
	} # end else
} # end function
