#' fn_EcritBilanJournier writes the daily migration in the
#' t_bilanmigrationjournalier_bjo table
#' 
#' Daily values are needed to compare migrations from year to year, by the class \link{BilanMigrationInterAnnuelle-class}. They are added by
#' the function fn_EcritBilanJournalier
#' 
#' @param bilanMigration an object of class \code{\linkS4class{BilanMigration}}
#' @param silent : TRUE to avoid messages
#' @param dbname : the name of the database, defaults to "bd_contmig_nat"
#' @param host : the host for sqldf, defaults to "localhost"
#' @param port : the port, defaults to 5432
#' @note the user is asked whether or not he wants to overwrite data, if no
#' data are present in the database, the import is done anyway. The name of the database
#' is not passed in odbc link, here defaults to "bd_contmig_nat"
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#' stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE) 
#' data("bM_Arzal")
#' bM_Arzal<-calcule(bM_Arzal)
#' fn_EcritBilanJournalier(bilanMigration=bM_Arzal,silent=FALSE)
#' }
#' @export
fn_EcritBilanJournalier<-function(bilanMigration,silent=FALSE,dbname="bd_contmig_nat",host="localhost",port=5432){
	# dbname="bd_contmig_nat";host="localhost";silent=FALSE;port=5432
	if (class(bilanMigration)!="BilanMigration") stop("the bilanMigration should be of class BilanMigration")
	if (class(silent)!="logical") stop("the silent argument should be a logical")
	dc=as.numeric(bilanMigration@dc@dc_selectionne)[1]
	data=bilanMigration@calcdata[[stringr::str_c("dc_",dc)]][["data"]]
	data=data[data$Effectif_total!=0,]
	jour_dans_lannee_non_nuls=data$debut_pas	
	col_a_retirer=match(c("No.pas","type_de_quantite","debut_pas","fin_pas"),colnames(data))
	data=data[,-col_a_retirer]
	data$taux_d_echappement[data$taux_d_echappement==-1]<-NA 
	data$coe_valeur_coefficient[data$"coe_valeur_coefficient"==1]<-NA 
	peuventpaszero=match(c("taux_d_echappement","coe_valeur_coefficient"),colnames(data))
	data[,-peuventpaszero][data[,-peuventpaszero]==0]<-NA
	annee<-as.numeric(unique(strftime(as.POSIXlt(bilanMigration@time.sequence),"%Y"))[1])
	aat_bilanmigrationjournalier_bjo=cbind(
			bilanMigration@dc@dc_selectionne,
			bilanMigration@taxons@data$tax_code,
			bilanMigration@stades@data$std_code,
			annee, # une valeur
			rep(jour_dans_lannee_non_nuls,ncol(data[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total","taux_d_echappement","coe_valeur_coefficient")])),
			utils::stack(data[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total","taux_d_echappement","coe_valeur_coefficient")]),  
			Sys.time(),
			substr(toupper(get("sch",envir=envir_stacomi)),1,nchar(toupper(get("sch",envir=envir_stacomi)))-1)
	)
	aat_bilanmigrationjournalier_bjo= stacomirtools::killfactor(aat_bilanmigrationjournalier_bjo[!is.na(aat_bilanmigrationjournalier_bjo$values),])
	colnames(aat_bilanmigrationjournalier_bjo)<-c("bjo_dis_identifiant","bjo_tax_code","bjo_std_code","bjo_annee","bjo_jour","bjo_valeur","bjo_labelquantite","bjo_horodateexport","bjo_org_code")

	#####
	# Ci dessous conversion de la classe vers migration Interannuelle pour utiliser
	# les methodes de cette classe
	bil=as(bilanMigration,"BilanMigrationInterAnnuelle")
	bil=connect(bil,silent=silent)
	
	hconfirm=function(h,...){			
		# suppression des donnees actuellement presentes dans la base
		# bilanjournalier et bilanmensuel
		supprime(bil)			
		baseODBC<-get("baseODBC",envir=envir_stacomi)
		sql<-stringr::str_c("INSERT INTO ",get("sch",envir=envir_stacomi),"t_bilanmigrationjournalier_bjo (",			
				"bjo_dis_identifiant,bjo_tax_code,bjo_std_code,bjo_annee,bjo_jour,bjo_valeur,bjo_labelquantite,bjo_horodateexport,bjo_org_code)",
				" SELECT * FROM  aat_bilanmigrationjournalier_bjo;")
		invisible(utils::capture.output(
						sqldf::sqldf(x=sql,
								drv="PostgreSQL",
								user=baseODBC["uid"],
								dbname=dbname,				
								password=baseODBC["pwd"],
								host=host,
								port=port)
				))		


if (!silent){
	funout(paste(get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.5,annee,"\n"))
}
# si l'utilisateur accepte de remplacer les valeurs

#progres<-get("progres",envir=envir_stacomi)
#gtkWidgetDestroy(progres)
# ecriture egalement du bilan mensuel
taxon= as.character(bilanMigration@taxons@data$tax_nom_latin)
stade= as.character(bilanMigration@stades@data$std_libelle)
DC=as.numeric(bilanMigration@dc@dc_selectionne)	
tableau<-bilanMigration@calcdata[[stringr::str_c("dc_",DC)]][["data"]]
resum=funstat(tableau=tableau,time.sequence=tableau$debut_pas,taxon,stade,DC,silent=silent )
fn_EcritBilanMensuel(bilanMigration,resum,silent=silent)
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
	
	baseODBC<-get("baseODBC",envir=envir_stacomi)
	sql<-stringr::str_c("INSERT INTO ",get("sch",envir=envir_stacomi),"t_bilanmigrationjournalier_bjo (",			
			"bjo_dis_identifiant,bjo_tax_code,bjo_std_code,bjo_annee,bjo_jour,bjo_valeur,bjo_labelquantite,bjo_horodateexport,bjo_org_code)",
			" SELECT * FROM  aat_bilanmigrationjournalier_bjo;")
	invisible(utils::capture.output(
					sqldf::sqldf(x=sql,
							drv="PostgreSQL",
							user=baseODBC["uid"],
							dbname=dbname,				
							password=baseODBC["pwd"],
							host=host,
							port=port)
			))		
#	

	if (!silent) funout(paste(get("msg",envir=envir_stacomi)$fn_EcritBilanJournalier.5,"\n"))
	taxon= as.character(bilanMigration@taxons@data$tax_nom_latin)
	stade= as.character(bilanMigration@stades@data$std_libelle)
	DC=as.numeric(bilanMigration@dc@dc_selectionne)	
	tableau<-bilanMigration@calcdata[[stringr::str_c("dc_",DC)]][["data"]]
	resum=funstat(tableau=tableau,time.sequence=tableau$debut_pas,taxon,stade,DC,silent=silent)
	fn_EcritBilanMensuel(bilanMigration,resum,silent=silent)
} # end else
} # end function
