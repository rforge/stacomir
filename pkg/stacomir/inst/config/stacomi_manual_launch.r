# Nom fichier :        stacomi 
# Projet :             controle migrateur 
# Organisme :          IAV/ONEMA
# Auteur :             Cedric Briand
# Contact :            cedric.briand"at"eptb-vilaine.fr
# Date de creation :   11/04/2006 15:51:44
# Compatibilite :      PostgreSQL 8.4
#                      R 2.10.1                                        
# Etat :               fonctionne 
# Description          interface principale, fonction de lancement
#                      chargement des librairies
#**********************************************************************
# initiation des parametres en fonctions des utilisateurs
rm(list=ls(all=TRUE))
# les donnees sont stockees dans un fichier xml stocke dans le repertoire par defaut,
# ce fichier peut etre edite pour modifier des parametres

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
source ("F:/workspace/stacomir/branch0.5/stacomir/inst/config/libraries.r")

libraries()

source("utilitaires.r") # contient  funout (pour ecrire dans la console) et filechoose
source("messages.R")
source("fn_table_per_dis.r") 
source("fn_sql_dis.r")
source("funtraitementdate.r")  
#source("vector_to_listsql.r")
source("funstatJournalier.r") 
source("fn_EcritBilanMensuel.r")
source("fn_EcritBilanJournalier.r")                        

#listes de connection a la base de donnee (programmation S4)
source("create_generic.r") 
#cree les fonctions generiques et l'environnement envir_stacomi
source("RefDF.r")
source("RefDC.r")
source ("RefTaxon.r")
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
source("RefPoidsMoyenPeche.r")
source("RefStationMesure.r")
source("Refperiode.r")
source("RefHorodate.r")
source("RefMsg.r")
source("BilanFonctionnementDC.r")
source("BilanFonctionnementDF.r")
source("BilanMigration.r")
source("BilanMigrationMult.r")
source("BilanConditionEnv.r")
source("BilanMigrationConditionEnv.r")
source("BilanMigrationPar.r")
source("BilanMigrationInterAnnuelle.r")
source("Bilan_carlot.r")
source("Bilan_taille.r") 
source("Bilan_poids_moyen.r")
source("BilanEspeces.r")
source("Bilan_stades_pigm.r")
source("setAs.r")
#source ("ggplot_user_interface.r")
#ggplot_user_interface()

# fonctions
source("funSousListeBilanMigration.r")
source("funSousListeBilanMigrationPar.R")
source("funBilanMigrationAnnuel.R")
source("funtraitement_poids.r")
source("fungraph_civelle.r")
source("fungraph.r")
source("fungraph_env.r")
source("funstat.r")
source("funtable.r")
source("interface_BilanMigrationInterAnnuelle.r")
source("interface_Bilan_lot.r")
source("interface_bilan_poids_moyen.r")
source("interface_Bilan_taille.r")
source("interface_BilanConditionEnv.r")
source("interface_BilanMigration.r")
source("interface_BilanMigrationConditionEnv.r")
source("interface_BilanMigrationPar.r")
source("interface_BilanFonctionnementDC.r")
source("interface_BilanFonctionnementDF.r")
source("interface_BilanMigrationMult.r")
source("stacomi.r")
# interface_BilanEspeces dans BilanEspeces
 stacomi(gr_interface=FALSE)
 # pour aller chercher les donnees
 setwd("F:/workspace/stacomir/branch0.5/stacomir")
