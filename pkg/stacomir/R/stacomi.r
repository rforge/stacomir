# Nom fichier :        stacomi 
# Projet :             controle migrateur 
# Organisme :          IAV/ONEMA
# Auteur :             Cedric Briand
# Contact :            cedric.briand@lavilaine.com
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
require(XML)
options(guiToolkit = "RGtk2")
filexml="C:/Program Files/stacomi/calcmig.xml"
doc = xmlInternalTreeParse(filexml)
doc=xmlRoot(doc)   # vire les infos d'ordre generales
tableau_config = xmlSApply(doc, function(x) xmlSApply(x, xmlValue)) # renvoit une liste

les_utilisateurs <- tableau_config[1]
datawd=tableau_config["datawd",]
#assign("datawd",datawd,envir=.GlobalEnv)
pgwd=tableau_config["pgwd",]
baseODBC=c(tableau_config["lienODBC",],tableau_config["uid",],tableau_config["pwd",])
setwd(pgwd)
# pour voir apparaitre toutes les requètes dans R
# assign("showmerequest",1,envir=envir_stacomi)
source ("interface/libraries.r")

libraries()

source("interface/utilitaires.r") # contient  funout (pour ecrire dans la console) et filechoose
source("interface/messages.R")
source("fonctions/fn_table_per_dis.r") 
source("fonctions/fn_sql_dis.r")
source("fonctions/funtraitementdate.r")  
source("fonctions/vector_to_listsql.r")
source("fonctions/funstatJournalier.r") 
source("fonctions/fn_EcritBilanMensuel.r")
source("fonctions/fn_EcritBilanJournalier.r")                        

#listes de connection à la base de donnee (programmation S4)
source("classes/create_generic.r") 
#cree les fonctions generiques et l'environnement envir_stacomi
source("classes/RefDF.r")
source("classes/RefDC.r")
source ("classes/RefTaxon.r")
source("classes/RefStades.r")
source("classes/PasdeTemps.r")
source("classes/PasDeTempsJournalier.r")
source("classes/Refpar.r")
source("classes/Refparquan.r")
source("classes/Refparqual.r")
source("classes/RefAnnee.r")
source("classes/RefCoe.r") # coeff de conversion poids effectif
source("classes/RefListe.r") #liste de donnees pour un choix
source("classes/RefChoix.r")
source("classes/ReftextBox.r")
source("classes/RefCheckBox.r")
source("classes/RefPoidsMoyenPeche.r")
source ("classes/RefStationMesure.r")
source ("classes/Refperiode.r")
source("classes/ConnexionODBC.r")
source("classes/RequeteODBC.r")
source("classes/RequeteODBCwhere.r")
source("classes/RequeteODBCwheredate.r")
source("classes/RefHorodate.r")
source ("classes/BilanFonctionnementDC.r")
source ("classes/BilanFonctionnementDF.r")
source ("classes/BilanMigration.r")
source ("classes/BilanConditionEnv.r")
source ("classes/BilanMigrationConditionEnv.r")
source ("classes/BilanMigrationPar.r")
source ("classes/BilanMigrationInterAnnuelle.r")
source ("classes/Bilan_lot.r")
source("classes/Bilan_taille.r") 
source ("classes/Bilan_poids_moyen.r")
source("classes/BilanEspeces.r")
source("classes/Bilan_stades_pigm.r")
source ("classes/setAs.r")
#source ("classes/ggplot_user_interface.r")
#ggplot_user_interface()

# fonctions
source("fonctions/funSousListeBilanMigration.r")
source("fonctions/funSousListeBilanMigrationPar.R")
source("fonctions/funtraitement_poids.r")
source("fonctions/fungraph_civelle.r")
source("fonctions/fungraph.r")
source("fonctions/fungraph_env.r")
source("fonctions/funstat.r")
source("fonctions/funtable.r")
source("interface/interface_BilanMigrationInterAnnuelle.r")
source("interface/interface_Bilan_lot.r")
source("interface/interface_bilan_poids_moyen.r")
source("interface/interface_Bilan_taille.r")
source("interface/interface_BilanConditionEnv.r")
source("interface/interface_BilanMigration.r")
source("interface/interface_BilanMigrationConditionEnv.r")
source("interface/interface_BilanMigrationPar.r")
source("interface/interface_BilanFonctionnementDC.r")
source("interface/interface_BilanFonctionnementDF.r")
source("interface/interface_graphique.r")
# interface_BilanEspeces dans BilanEspeces
 stacomi(gr_interface=TRUE)