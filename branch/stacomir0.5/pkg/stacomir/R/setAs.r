# Nom fichier :        setAs.R
# Projet :             GEMAC/Prog global
# Organisme :          IAV/CEMAGREF
# Auteur :             Cedric Briand
# Contact :            cedric.briand@lavilaine.com
# Date de creation :   02/12/2005 14:28:09
# Description :         Fonction de conversion entre classes
# Data needed
# Packages needed
# remarques
#
#**********************************************************************
#*
#* Modifications :
#* ---------------
#* JJ-MM-AAAA #No Prenom NOM [INITIALES] :
#*    explication de la modification
#*
#**********************************************************************
# from=new("BilanMigration")
setAs("BilanMigration","BilanMigrationInterAnnuelle",function(from,to){
  anneeDebut=new("RefAnnee")
   anneeFin=new("RefAnnee")
  anneeDebut@annee_selectionnee=min(getAnnees(from@pasDeTemps))
  anneeFin@annee_selectionnee=max(getAnnees(from@pasDeTemps))
  bilanMigrationInterAnnuelle=new("BilanMigrationInterAnnuelle")
  bilanMigrationInterAnnuelle@dc=from@dc
  bilanMigrationInterAnnuelle@taxons=from@taxons
  bilanMigrationInterAnnuelle@stades=from@stades
  bilanMigrationInterAnnuelle@anneeDebut=anneeDebut
  bilanMigrationInterAnnuelle@anneeFin=anneeFin
  return(bilanMigrationInterAnnuelle)
})