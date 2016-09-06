#' fn_EcritBilanJournier writes the daily migration in the
#' t_bilanmigrationjournalier_bjo table
#' 
#' Daily values are needed to compare migrations from year to year, by the class \link{BilanMigrationInterAnnuelle-class}. They are added by
#' the function fn_EcritBilanJournalier
#' 
#' @param bilanMigration an object of class \code{\linkS4class{BilanMigration}}
#' @param silent : TRUE to avoid messages
#' @note the user is asked whether or not he wants to overwrite data, if no
#' data are present in the database, the import is done anyway
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#' stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE) 
#' data("bM_Arzal")
#' bM_Arzal<-calcule(bM_Arzal)
#' fn_EcritBilanJournalier(bilanMigration=bM_Arzal,silent=FALSE)
#' }
#' @export
fn_EcritBilanJournalier<-function(bilanMigration,silent){
	# voir essai_table_bilanJournalier.sql pour le format du tableau
	# je cherche les colonnes que je ne veux pas retenir
	if (class(bilanMigration)!="BilanMigration") stop("the bilanMigration should be of class BilanMigration")
	if (class(silent)!="logical") stop("the silent argument should be a logical")
	dc=as.numeric(bilanMigration@dc@dc_selectionne)[1]
	data=bilanMigration@calcdata[[stringr::str_c("dc_",dc)]][["data"]]
	jour_dans_lannee_non_nuls=data$debut_pas
	data=data[data$Effectif_total!=0,]
	col_a_retirer=match(c("No.pas","type_de_quantite","debut_pas","fin_pas"),colnames(data))
	data=data[,-col_a_retirer]
	data$taux_d_echappement[data$taux_d_echappement==-1]<-NA 
	data$coe_valeur_coefficient[data$"coe_valeur_coefficient"==1]<-NA 
	peuventpaszero=match(c("taux_d_echappement","coe_valeur_coefficient"),colnames(data))
	data[,-peuventpaszero][data[,-peuventpaszero]==0]<-NA
	t_bilanmigrationjournalier_bjo=cbind(
			bilanMigration@dc@dc_selectionne,
			bilanMigration@taxons@data$tax_code,
			bilanMigration@stades@data$std_code,
			unique(strftime(as.POSIXlt(bilanMigration@time.sequence),"%Y")), # une valeur
			rep(as.character(jour_dans_lannee_non_nuls),ncol(data[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total","taux_d_echappement","coe_valeur_coefficient")])),
			utils::stack(data[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total","taux_d_echappement","coe_valeur_coefficient")]),  
			format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
			substr(toupper(get("sch",envir=envir_stacomi)),1,nchar(toupper(get("sch",envir=envir_stacomi)))-1)
	)
	t_bilanmigrationjournalier_bjo= stacomirtools::killfactor(t_bilanmigrationjournalier_bjo[!is.na(t_bilanmigrationjournalier_bjo$values),])
	
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
		# progress bar
#  OLD CODE = problems to pass Rcheck
#		progres<-utils::winProgressBar(title = get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.3,
#				label = get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.4,
#				min = 0,
#				max = 1, 
#				initial = 0,
#				width = 400)
		
		mygtkProgressBar(title=get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.3,
		progress_text=get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.4)
		
		for (i in 1:nrow(t_bilanmigrationjournalier_bjo)) {				
			zz=i/nrow(t_bilanmigrationjournalier_bjo)
			progress_bar$setFraction(zz)
			progress_bar$setText(sprintf("%d%% progression",round(100*zz)))
			# TODO verifier si nécessaire ci dessous
			RGtk2::gtkMainIterationDo(FALSE)
#			utils::setWinProgressBar(progres,
#					zz,
#					title=get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.5,
#					label=sprintf("%d%% progression",
#							round(100*zz)))     
			requete@sql=paste( "INSERT INTO ",get("sch",envir=envir_stacomi),"t_bilanmigrationjournalier_bjo (",			
					"bjo_dis_identifiant,bjo_tax_code,bjo_std_code,bjo_annee,bjo_jour,bjo_valeur,bjo_labelquantite,bjo_horodateexport,bjo_org_code)",
					" VALUES " ,"('",paste(t_bilanmigrationjournalier_bjo[i,],collapse="','"),"');",sep="")
			requete<-stacomirtools::connect(requete) 
			
		} # end for
		if (!silent){
		funout(paste(get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.5,"\n"))
		}
		# si l'utilisateur accepte de remplacer les valeurs
		odbcClose(requete@connection)
		progres<-get("progres",envir=envir_stacomi)
		gtkWidgetDestroy(progres)
		# ecriture egalement du bilan mensuel
		taxon= as.character(bilanMigration@taxons@data$tax_nom_latin)
		stade= as.character(bilanMigration@stades@data$std_libelle)
		DC=as.numeric(bilanMigration@dc@dc_selectionne)	
		resum=funstat(tableau=bilanMigration@data,time.sequence=bilanMigration@time.sequence,taxon,stade,DC )
		fn_EcritBilanMensuel(bilanMigration,resum)
	}#end function hconfirm
	
	if (nrow(bil@data)>0)
	{ 
		if (!silent){
		choice<-gWidgets::gconfirm(paste(get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.1, # Un bilan a deja ete ecrit dans la base
						unique(bil@data$bjo_horodateexport),
						get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.2),
				handler=hconfirm) # voulez vous le remplacer ?
		} else {
			hconfirm(h=NULL)
		}
		
	}
	else  # sinon on ecrit les resultats quoiqu'il arrive
	{
		requete=new("RequeteODBC")
		requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
		requete@silent=TRUE
		requete@open=TRUE
		mygtkProgressBar(title=get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.3,
				progress_text=get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.4)

#		progres<-utils::winProgressBar(title = get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.5,
#				label = "progression %",
#				min = 0,
#				max = 1, 
#				initial = 0,
#				width = 400)
		for (i in 1:nrow(t_bilanmigrationjournalier_bjo)) {
			zz=i/nrow(t_bilanmigrationjournalier_bjo)				
#			setWinProgressBar(progres,
#					zz,
#					title=get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.5,
#					label=sprintf("%d%% progression",
#							round(100*zz)))
		progress_bar$setFraction(zz)
		progress_bar$setText(sprintf("%d%% progression",round(100*zz)))
		RGtk2::gtkMainIterationDo(FALSE)
			requete@sql=paste( "INSERT INTO ",get("sch",envir=envir_stacomi),"t_bilanmigrationjournalier_bjo (",			
					"bjo_dis_identifiant,bjo_tax_code,bjo_std_code,bjo_annee,bjo_jour,bjo_valeur,bjo_labelquantite,bjo_horodateexport,bjo_org_code)",
					" VALUES " ,
					"('",paste(t_bilanmigrationjournalier_bjo[i,],collapse="','"),"');",sep="")
			requete<-stacomirtools::connect(requete)   
		} # end for
		RODBC::odbcClose(requete@connection)
		if (!silent) funout(paste(get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.5,"\n"))
		taxon= as.character(bilanMigration@taxons@data$tax_nom_latin)
		stade= as.character(bilanMigration@stades@data$std_libelle)
		DC=as.numeric(bilanMigration@dc@dc_selectionne)	
		tableau<-bilanMigration@calcdata[[stringr::str_c("dc_",DC)]][["data"]]
		resum=funstat(tableau=tableau,time.sequence=tableau$debut_pas,taxon,stade,DC,silent=silent)
		fn_EcritBilanMensuel(bilanMigration,resum)
	} # end else
} # end function
