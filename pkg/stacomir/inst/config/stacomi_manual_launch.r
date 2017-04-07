
rm(list=ls(all=TRUE))


## lancement du programme proprement dit
#  if (exists("group")) {rm(group)}
#  if (exists("graphes")) {rm(graphes) }
#ci dessous en lancement manuel il est necessaire d'indiquer le chemin du repertoire de travail
# avant toute chose
#require(XML)
options(guiToolkit = "RGtk2")
filecsv="C:/Program Files/stacomi/calcmig.csv"
doc<-read.csv(filecsv,header=TRUE,sep=";")
tableau_config = t(doc) 

les_utilisateurs <- tableau_config[1]
#datawd=tableau_config["datawd",]
#assign("datawd",datawd,envir=envir_stacomi)
pgwd=tableau_config["pgwd",]
baseODBC=c(tableau_config["lienODBC",],tableau_config["uid",],tableau_config["pwd",])
setwd(pgwd)
# pour voir apparaitre toutes les requetes dans R
# assign("showmerequest",1,envir=envir_stacomi)
source ("F:/workspace/stacomir/pkg/stacomir/inst/config/libraries.r")
source ("C:/Users/logrami/workspace/stacomir/pkg/stacomir/inst/config/libraries.r")

libraries()

source("utilitaires.r") # contient  funout (pour ecrire dans la console) et filechoose
source("messages.R")
source("fn_table_per_dis.r")  
#source("vector_to_listsql.r")
source("funstatJournalier.r") 
source("fn_EcritBilanMensuel.r")
                      

#listes de connection a la base de donnee (programmation S4)
source("create_generic.r") 
#cree les fonctions generiques et l'environnement envir_stacomi
source("RefDF.r")
source("RefDC.r")
source("RefTaxon.r")
source("RefStades.r")
source("PasdeTemps.r")
source("PasDeTempsJournalier.r")
source("Refpar.r")
source("Refparquan.r")
source("Refparqual.r")
source("RefAnnee.r")
source("RefCoe.r") # coeff de conversion poids effectif
source("RefListe.r") #liste de donnees pour un choice
source("RefChoix.r")
source("ReftextBox.r")
source("RefCheckBox.r")
source("RefStationMesure.r")
source("Refperiode.r")
source("RefHorodate.r")
source("RefMsg.r")
source("BilanFonctionnementDC.r")
source("BilanFonctionnementDF.r")
source("BilanOperation.r")
source("BilanMigration.r")
source("BilanMigrationMult.r")
source("BilanConditionEnv.r")
source("BilanMigrationMultConditionEnv.r")
source("Bilan_carlot.r")
source("BilanMigrationCar.r")
source("BilanMigrationInterAnnuelle.r")
require(xtable)
source("BilanAnnuels.r")
source("BilanArgentee.r")

#source("Bilan_taille.r") 
source("Bilan_poids_moyen.r")
source("BilanEspeces.r")
#source("Bilan_stades_pigm.r")
source("BilanAgedemer.r")
source("setAs.r")
#source ("ggplot_user_interface.r")
#ggplot_user_interface()

# fonctions
source("funSousListeBilanMigrationPar.R")
source("funtraitement_poids.r")
source("fungraph_civelle.r")
source("fungraph.r")
source("funstat.r")
source("funtable.r")
source("interface_BilanMigrationInterAnnuelle.r")
source("interface_Bilan_carlot.r")
source("interface_bilan_poids_moyen.r")
source("interface_Bilan_taille.r")
source("interface_BilanConditionEnv.r")
source("interface_BilanMigration.r")
source("interface_BilanMigrationMultConditionEnv.r")
source("interface_BilanMigrationCar.r")
source("interface_BilanFonctionnementDC.r")
source("interface_BilanFonctionnementDF.r")
source("interface_BilanMigrationMult.r")
source("interface_BilanArgentee.r")
source("interface_BilanAnnuels.r")
source("interface_BilanAgedemer.r")
source("stacomi.r")
# interface_BilanEspeces dans BilanEspeces
setwd("F:/workspace/stacomir/pkg/stacomir")
data("msg")
stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)


