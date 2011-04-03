# label.R
# 
# Author: cedric
###############################################################################

#' Function for creating a list gathering all msgs for internationalization.
#' Attention all msgs are starting with a letter "l" numbers are wrong when used as msgs within a list
#' 
#' @author Cedric Briand \email{cedric.briand@@lavilaine.com}
messages=function(lang="french"){
	msg=list()
# Referentiel (called after having checked it results from query from referential object are stored in envir_stacomi
	msg$ref.1<-"Il faut choisir un dispositif de comptage,cliquez sur valider \n"
	msg$ref.2<-"Il faut choisir un taxon,cliquez sur valider \n"
	msg$ref.3<-"Il faut choisir un stade,cliquez sur valider \n"
	msg$ref.4<-"Il faut choisir un parametre,cliquez sur valider \n"
	msg$ref.5<-"Il faut choisir la date de debut\n"
	msg$ref.6<-"Il faut choisir la date de fin\n"
	msg$ref.7<-"Il faut choisir un paramètre quantitatif\n"
	msg$ref.8<-"Il faut choisir un parametre qualitatif\n"
	msg$ref.9<-"Il faut choisir une categorie d'effectif\n"
	msg$ref.10<-"Il faut choisir l'annee de debut\n"
	msg$ref.11<-"Il faut choisir l'annee de fin\n"
	msg$ref.12<-"il faut choisir un dispositif de franchissement,cliquez sur valider \n"
# Bilan_lot
	msg$Bilan_lot.1<-"La requete est effectuee pour charger les caracteristiques de lot \n"
	msg$Bilan_lot.2<-"Aucune donnee pour ces lots dans la periode selectionnee\n"
	msg$Bilan_lot.3<-"Pour recuperer le tableau, tapper : bilan_lot=get('bilan_lot',envir_stacomi), requete=bilan_lot@requete@sql\n" 
	msg$Bilan_lot.4<-"Pour recuperer l'objet graphique, tapper : p, c'est l'objet ggplot cree par l'interface, dans cette interface, la caracteristique choisie s'appelle val_quant\n"
# Bilan_poids_moyen
	msg$Bilan_poids_moyen.1<-"La requete est effectuee pour charger les coefficients de conversion \n"
	msg$Bilan_poids_moyen.2<-"lignes trouvees pour les coefficients de conversion\n" # context nrows found for conversion coefficients
	msg$Bilan_poids_moyen.3<-"Pour recuperer le tableau, tapper : bilan_poids_moyen=get('bilan_poids_moyen',envir_stacomi)@data\n"
	msg$Bilan_poids_moyen.4<-"effectif non renseigne, lots" # at the start of a warning
	msg$Bilan_poids_moyen.5<-"humides" # context weight of undrained glass eels
	msg$Bilan_poids_moyen.6<-"secs"	# context weight of glass eels individually mesured (with muccus but no water)	
	msg$Bilan_poids_moyen.7<-"humides et secs" # all weights both cases selected
	# graphs
	msg$Bilan_poids_moyen.8<-"date"
	msg$Bilan_poids_moyen.9<-"poids moyens"
	msg$Bilan_poids_moyen.10<-"Tendance saisonniere des poids" # within a paste
	msg$Bilan_poids_moyen.11<-", anneedebut="
	msg$Bilan_poids_moyen.12<-", anneefin="
	msg$Bilan_poids_moyen.13<-"modele sinusoidal,a.cos.2pi.(jour-T)/365)+b "
	msg$Bilan_poids_moyen.14<-"Pour recuperer le tableau, tapper : import_coe=get('import_coe',envir_stacomi)\n"
	msg$Bilan_poids_moyen.15<-"Repertoire d'ecriture des donnees :"
	# frame question
	msg$Bilan_poids_moyen.16<-"Voulez vous ecrire les donnees dans la base ?"
	# frame title
	msg$Bilan_poids_moyen.17<-"attention"
	# buttons msg
	msg$Bilan_poids_moyen.18<-"Gra" # for graphes
	msg$Bilan_poids_moyen.19<-"Coe" # for coefficients
	msg$Bilan_poids_moyen.20<-"Tail" # for size (bubbles of different size according to the numbers
	msg$Bilan_poids_moyen.21<-"Reg" # for regression
	msg$Bilan_poids_moyen.22<-"export" # to export the data
	msg$Bilan_poids_moyen.23<-"quitter" # button to quit
# Bilan_stades_pigm
	msg$Bilan_stades_pigm.1<-"La requete a été effectuée pour charger les stades pigmentaires\n"
	msg$Bilan_stades_pigm.2<-"Bilan des stades pigmentaires\n"
	msg$Bilan_stades_pigm.3<-"BILAN STADES PIGMENTAIRES" # glablel
	msg$Bilan_stades_pigm.4<-"Stades pigmentaires"
	msg$Bilan_stades_pigm.5<-"et dates d'arrivees en estuaire"
	msg$Bilan_stades_pigm.6<-"Choix du titre"
# Bilan_taille
	msg$Bilan_taille.1<-"Il faut choisir au moins une caracteristique quantitative ou qualitative\n"	
	msg$Bilan_taille.2<-"Attention cette requete (croisement de deux vues) peut prendre plusieurs minutes, soyez patient(e)...\n"
	msg$Bilan_taille.3<-"Pour recuperer le tableau, tapper : bilan_taille=get('bilan_taille',envir_stacomi)\n, pour le script de la requète requete=bilan_taille@requete@sql\n"
	msg$Bilan_taille.4<-"La requete : requete=get('bilan_taille',envir_stacomi)@requete@sql \n"
	msg$Bilan_taille.5<-"Les donnees : donnees_taille=get('bilan_taille',envir_stacomi)@data\n"
	msg$Bilan_taille.6<-"Donnees directement issues de la requete : don=get('bilan_taille',envir_stacomi)@requete@query\n" 
	msg$Bilan_taille.7<-"Il faut d'abord faire le calcul, appuyez sur calcul\n"
	msg$Bilan_taille.8<-"Requete effectuee\n"
# Bilan conditions environnementales
	msg$BilanCondtionEnv.1<-"La requete est effectuee pour charger les conditions environnementales \n"
	msg$BilanCondtionEnv.2<-"Il faut choisir une station de mesure puis cliquer sur valider \n"
	msg$BilanCondtionEnv.3<-"Certaines stations de mesure n'ont pas de valeurs associees\n"
	msg$BilanCondtionEnv.4<-"Aucune valeur de conditions environnementales pour les stations de mesure selectionnees (BilanConditionEnv.r)\n"
	msg$BilanCondtionEnv.5<-"Statistiques :\n"
# BilanEspeces
	msg$BilanEspeces.1<-"L'objet Bilan est stocke dans l'environnement envir_stacomi, ecrire  ecrire bilanEspeces=get('bilanEspeces',envir_stacomi), \n les graphiques peuvent être charges \n"
	msg$BilanEspeces.2<-"Il faut faire tourner les calculs avant, cliquer sur calc \n"
	msg$BilanEspeces.3<-"Echec de la requete vers la vue vue_ope_lot_car \n "
	msg$BilanEspeces.4<-"Il faut lancer les calculs avant cliquez sur calcul\n "
	msg$BilanEspeces.5<-"Il n'y a aucun poisson dans la base sur cette periode\n "
	msg$BilanEspeces.6<-"Attention certains effectifs négatifs sont transformés en positifs\n "
	msg$BilanEspeces.7<-"Verifications des objets et lancement de la requete\n "
	
# BilanFonctionnementDC
	msg$BilanFonctionnementDC.1<-"La requete est effectuee pour charger les pas de temps du DC \n"
	msg$BilanFonctionnementDC.2<-"Il n'y a pas de donnees sur ce DC \n"
	# graphe
	msg$BilanFonctionnementDC.3<-"mois" # xlabel
	msg$BilanFonctionnementDC.4<-"temps en heures" # ylabel
	msg$BilanFonctionnementDC.5<-"Fonctionnement du dispositif de comptage" # title
	msg$BilanFonctionnementDC.6<-"suivi video" # legend
	msg$BilanFonctionnementDC.7<-"arret video" # legend
	# 
	msg$BilanFonctionnementDC.8<-"Ecriture de tableau dans l'environnement envir_stacomi : ecrire periodeDC=get('periodeDC',envir_stacomi) \n"
	# graph (boxes)
	msg$BilanFonctionnementDC.9<-"DC" # ylab
	msg$BilanFonctionnementDC.10<-c("Fonc","Arr","Fonc normal")# legend
	msg$BilanFonctionnementDC.11<-c("Fonc","Arr")
	msg$BilanFonctionnementDC.12<-"Fonctionnement DC" # upper box title
	msg$BilanFonctionnementDC.13<-"Types d'arrets du DC" # lower box title
	msg$BilanFonctionnementDC.14<-"Ecriture de" # path
	msg$BilanFonctionnementDC.15<-"peut prendre un peu de temps, soyez patient....\n"
# BilanFonctionnementDF
	msg$BilanFonctionnementDF.1<-"La requete est effectuee pour charger les pas de temps du DF \n"
	msg$BilanFonctionnementDF.2<-"Il n'y a pas de donnees sur ce DF \n"
	msg$BilanFonctionnementDF.3<-"Construction du graphe, patientez \n"
	msg$BilanFonctionnementDF.4<-"calcul..." # progressbar
	msg$BilanFonctionnementDF.5<-"progression %" # progressbar
	msg$BilanFonctionnementDF.6<-c("duree","type_fonct.","fonctionnement")
	msg$BilanFonctionnementDF.7<-"Fonctionnement DF"
	msg$BilanFonctionnementDF.8<-"ecriture de tableau dans l'environnement envir_stacomi : ecrire periodeDF=get('periodeDF',envir_stacomi) \n"
	msg$BilanFonctionnementDF.9<-"DF"
	msg$BilanFonctionnementDF.10<-"Types d'arrets du DF"
# Bilan Migration
	msg$BilanMigration.1<-"Attention le choix du pas de temps n'a pas ete effectue, calcul avec la valeur par defaut \n"
	msg$BilanMigration.2<-"Debut du bilan migration... patientez \n"
	msg$BilanMigration.3<-"L'objet Bilan est stocke dans l'environnement envir_stacomi, ecrire  ecrire bilanMigration=get('bilanMigration',envir_stacomi)\n"
	msg$BilanMigration.4<-"Ecriture du tableau de bilan des migrations dans l'environnement envir_stacomi : ecrire tableau=get('tableau',envir_stacomi)\n "
	msg$BilanMigration.5<-"Il faut faire tourner les calculs avant, cliquer sur calc \n"
	# graphique migration cumulée
	msg$BilanMigration.6<-"Migration cumulee"
	msg$BilanMigration.7<-"Effectif cumule, "
	msg$BilanMigration.8<-"Attention cette fonction est pour les bilans annuels \n"
	msg$BilanMigration.9<-"Statistiques concernant la migration : \n"
# BilanMigrationConditionEnv
	msg$BilanMigrationConditionEnv.1<-"L'objet Bilan est stocke dans l'environnement envir_stacomi\n, ecrire bilanMigrationConditionEnv=get('bilanMigrationConditionEnv',envir_stacomi)\n"
	msg$BilanMigrationConditionEnv.2<-"Il faut faire tourner les calculs avant, cliquer sur calc\n"
	msg$BilanMigrationConditionEnv.3<-"Vous n'avez pas de conditions environnementales sur la periode de temps\n"
	msg$BilanMigrationConditionEnv.4<-"pas station selectionnee => graphe simple\n"
	msg$BilanMigrationConditionEnv.5<-"le nombre de lignes du tableau des conditions environnentales ("
	msg$BilanMigrationConditionEnv.6<-") ne correspond pas a la duree du bilan Migration ("
	msg$BilanMigrationConditionEnv.7<-"Attention : sur une des stations :"
	msg$BilanMigrationConditionEnv.8<-"il y a plusieurs enregistrements pour la même journée : "
	msg$BilanMigrationConditionEnv.9<-"seule la première valeur sera intégrée dans le bilan \n "
# BilanMigrationInterannuelle
	msg$BilanMigrationInterannuelle.1<-"Attention il n'existe pas de bilan migration pour l'annee "	# part of a message
	msg$BilanMigrationInterannuelle.2<-", ce taxon et ce stade (BilanMigrationInterAnnuelle.r)" # part of a message
	msg$BilanMigrationInterannuelle.3<-"La requete est effectuee pour charger les migrations sur les annees"
	## graph
	msg$BilanMigrationInterannuelle.4<-"Effectifs" # pasted within the title
	msg$BilanMigrationInterannuelle.5<-"ATTENTION : Veuillez effectuer un Bilan Migration pour au moins une des annees selectionnees avant de lancer un bilan inter-annuel"
	# graph cum
	msg$BilanMigrationInterannuelle.6<-"annee" # group
	msg$BilanMigrationInterannuelle.7<-"date" # X
	msg$BilanMigrationInterannuelle.8<-"Pourcentage de la migration annuelle" # Y
	msg$BilanMigrationInterannuelle.9<-", EPACS"# within title
	msg$BilanMigrationInterannuelle.10<-"Ecriture de l'objet graphique dans l'environnement envir_stacomi : ecrire g=get('gi',envir_stacomi) avec "
# BilanMigrationPar
	msg$BilanMigrationPar.1<-"Debut du bilan migration avec parametres... patientez \n"	
	msg$BilanMigrationPar.2<-"Il faut choisir au moins une caracteristique quantitative ou qualitative\n"	
	msg$BilanMigrationPar.3<-"Attention, ce traitement ne s'effectue pas sur les quantites de lots \n"	
	msg$BilanMigrationPar.4<-"Ecriture de data dans l'environnement envir_stacomi : ecrire data=get('data',envir_stacomi) \n"	
	msg$BilanMigrationPar.5<-"Il faut faire tourner les calculs avant\n"
	msg$BilanMigrationPar.6<-"Ecriture de l'objet graphique dans l'environnement envir_stacomi : ecrire g=get('g',envir_stacomi) \n"
	msg$BilanMigrationPar.7<-"Ecriture de"# path
# ConnexionODBC (note messages below are not printed in the console, so do not need \n
	msg$ConnexionODBC.1<-"Il faut definir un vecteur baseODBC avec le lien ODBC, l'utilistateur et le mot de passe\n"
	msg$ConnexionODBC.2<-"La librairie RODBC est necessaire, chargez le package ! \n"
	msg$ConnexionODBC.3<-"Essai de connexion, attention cette classe ne doit être utilisée que pour les tests : \n"
	msg$ConnexionODBC.4<-"Connexion impossible :\n"
	msg$ConnexionODBC.5<-"Connexion établie"
	msg$ConnexionODBC.6<-"Connexion en cours"
# PasdeTemps
	msg$PasdeTemps.1<-"Choix des Pas de Temps"
	msg$PasdeTemps.2<-"Choix du nombre de pas de temps"
	msg$PasdeTemps.3<-"Erreur interne : le tableau des pas de temps ne contient aucune ligne\n"	
	msg$PasdeTemps.4<-"Date de fin"
# PasdeTempsJournalier
	msg$PasdeTempsJournalier.1<-"la duree du pas devrait etre journaliere"
	msg$PasdeTempsJournalier.2<-"le pas de temps ne doit pas etre a cheval sur plusieurs annnees"
	msg$PasdeTempsJournalier.3<-"Choix des Pas de Temps (duree 1 an)"
	msg$PasdeTempsJournalier.4<-"Date de debut"
	msg$PasdeTempsJournalier.5<-"Pas de temps"
	msg$PasdeTempsJournalier.6<-"Nb jour"
	msg$PasdeTempsJournalier.7<-"Date de fin"
	msg$PasdeTempsJournalier.8<-"Les pas de temps ont ete charges\n"	
# RefAnnee
	msg$RefAnnee.1<-"Choix de l'annee"
	msg$RefAnnee.2<-"Annee selectionnee\n"
	msg$RefAnnee.3<-"probleme lors du chargement des donnees ou pas de donnees dans la base (lien ODBC ?)"
# RefDC
	msg$RefDC.1<-"Le DC a ete selectionne \n"
	msg$RefDC.2<-"selection des taxons du DC (pour l'instant sur toutes les periodes) \n"
	msg$RefDC.3<-"Donnees sur les Dispositifs de Comptage"
	msg$RefDC.4<-"fermer" # button
	msg$RefDC.5<-"Choix du Dispositif de Comptage"# title
	msg$RefDC.6<-"Tableau"
	msg$RefDC.7<-"Erreur : Aucun DC n'est rentre dans la base (aucune ligne de retour de la requete)\n"
# RefDF
	msg$RefDF.1<-"Le DF a ete selectionne \n"
	msg$RefDF.2<-"Donnees sur les Dispositifs de Franchissement"
	msg$RefDF.3<-"Choix du Dispositif de Franchissement"
	msg$RefDF.4<-"Aucun DF n'est rentre dans la base (aucune ligne de retour de la requete)\n"
# Refpar
	msg$Refpar.1<-"La requete est effectuee pour charger les parametres \n"
	msg$Refpar.2<-"Pas de donnees pour ce DC, ce taxon et ce stade \n"
	msg$Refpar.3<-"La caracteristique a ete selectionnee \n"
	msg$Refpar.4<-"erreur interne, aucune caracteristique n'a pu être chargée pour faire le choix \n"
# Refparqual
	msg$Refparqual.1<-"Erreur interne : il devrait y avoir une ligne dans Refparqual@data, or nbligne="
# Refparquan
# Refperiode
# RefpoidsMoyenPeche
# RefStades 
	msg$RefStades.1<-"Pas de donnees pour ce DC, et ce taxon \n"
	msg$RefStades.2<-"Le stade a ete selectionne \n"
	msg$RefStades.3<-"Caracteristique qualitative" # this is a frame label
	msg$RefStades.4<-"Caracteristique quantitative" # this is a frame label
	msg$RefStades.5<-"Stop erreur interne : charger les donnees pour faire le choix \n"
	msg$RefStades.6<-"Choix du Stade" # this is a frame label
# RefStationMesure
	msg$RefStationMesure.1<-"selectionnez au moins une valeur\n"	
	msg$RefStationMesure.2<-"Les stations de mesure ont ete selectionnees \n"
	msg$RefStationMesure.3<-"Choix des stations de mesure" # frame label
	msg$RefStationMesure.4<-"Stop il n'y  a aucune donnee de station de mesure (pb lien ODBC ?) \n"
# Reftaxon	
	msg$RefTaxon.1<-"Le taxon a ete selectionne \n"
	msg$RefTaxon.2<-"Choix du Taxon" # frame label
	msg$RefTaxon.3<-"Stop il n'y  a aucune ligne dans la table des taxons (pb lien ODBC ?) \n" 
# RequeteODBC
	msg$RequeteODBC.1<-"Erreur ODBC =>Il faut definir un vecteur baseODBC avec le lien ODBC, l'utilistateur et le mot de passe \n"
	msg$RequeteODBC.2<-"Essai de connexion :"
	msg$RequeteODBC.3<-"Connexion impossible :"
	msg$RequeteODBC.4<-"Connexion reussie \n"
	msg$RequeteODBC.5<-"Essai de la requete \n"
	msg$RequeteODBC.6<-"Requete reussie \n"
# RequeteODBCwhere
# RequeteODBCwheredate
# FUNCTIONS
# fn_EcritBilanJournalier
	msg$fn_EcritBilanJournalier.1<-"Un Bilan a deja ete ecrit dans la base le :"
	msg$fn_EcritBilanJournalier.2<-"voulez vous le remplacer ?"
	msg$fn_EcritBilanJournalier.3<-"ecriture du bilan journalier dans la base"
	msg$fn_EcritBilanJournalier.4<-"progression %"
	msg$fn_EcritBilanJournalier.5<-"ecriture du bilan journalier dans la base"
	msg$fn_EcritBilanMensuel.1<-"ecriture du bilan mensuel dans la base"
# fungraph_civelle
	msg$fungraph_civelle.1<-"graph civelle :" # followed by dis_commentaire
	msg$fungraph_civelle.2<-"Effectif de civelles (x1000)" # title
	msg$fungraph_civelle.3<-"Effectif estimes,"
	msg$fungraph_civelle.4<-c("eff. journ. poids","eff. journ. compt.")
	msg$fungraph_civelle.5<-"nombre d'operations="
	msg$fungraph_civelle.6<-"duree moyenne du piegeage="
	msg$fungraph_civelle.7<-"duree max="
	msg$fungraph_civelle.8<-"duree min="
	msg$fungraph_civelle.9<-c("Fonc","Arr","Fonc normal")
	msg$fungraph_civelle.10<-c("Fonc","Arr")
	msg$fungraph_civelle.11<-"DF"
	msg$fungraph_civelle.12<-"DC"
	msg$fungraph_civelle.13<-"OP"
	msg$fungraph_civelle.14<-"Mois"
	msg$fungraph_civelle.15<-"Effectif (x1000)"
	msg$fungraph_civelle.16<-c("eff. mens. poids","eff. mens. compt.")
# fungraph_env
	msg$fungraph_env.1<-"Effectif"
	msg$fungraph_env.2<-"Ecriture de l'objet graphique dans l'environnement envir_stacomi : ecrire g=get('g',envir_stacomi) \n"
# fungraph
	msg$fungraph.1<-"ATTENTION, il y a des quantite de lots rentrees pour un taxon autre que civelles, verifier"
	msg$fungraph.2<-"effectif"
	msg$fungraph.3<-"Date"
	msg$fungraph.4<-"Effectif estime, "
	msg$fungraph.5<-c("Ponctuel","expert","calcule","mesure")
	msg$fungraph.6<-"somme effectifs ="
	msg$fungraph.7<-c("Effectifs","type","duree","mois","quinzaine","semaine","jour_365") # column names
# funstat
	msg$funstat.1<-"calcul des bilans mensuels\n"
	msg$funstat.2<-	"Le resume des effectifs mensuel est colle dans la console :\n"
# funtable
	msg$funtable.1<-"ecriture de"
# funtraitement_poids
	msg$funtraitement_poids.1<-"Conversion poids effectif \n"
	msg$funtraitement_poids.2<-"Attention somme =0,vous n'avez pas encore rentre les coef de conversion\n"
# INTERFACE
# interface_Bilan_lot
	msg$interface_Bilan_lot.1<-"chargement de la vue (vue_ope_lot) et choix du dc et des pas de temps\n"
	msg$interface_Bilan_lot.2<-"BILAN LOTS" # glablel
	msg$interface_Bilan_lot.3<-"horodate de debut"
	msg$interface_Bilan_lot.4<-"horodate de fin"
	msg$interface_Bilan_lot.5<-"La date de debut a ete choisie\n"
	msg$interface_Bilan_lot.6<-"La date de fin a ete choisie\n"
	msg$interface_Bilan_lot.7<-"Graphe interactif" # tooltip
	msg$interface_Bilan_lot.8<-"tableau" # tooltip
	msg$interface_Bilan_lot.9<-"Quitter" # tootip and name
# interface_Bilan_poids_moyen
	msg$interface_Bilan_poids_moyen.1<-	"BILAN POIDS MOYEN"	
	msg$interface_Bilan_poids_moyen.2<-	"Tendance annuelle des poids moyens" # tooltip
	msg$interface_Bilan_poids_moyen.3<-	"tableau" # tooltip
	msg$interface_Bilan_poids_moyen.4<-	"Quitter" # tooltip and name
	msg$interface_Bilan_poids_moyen.5<-	"Annee de debut"
	msg$interface_Bilan_poids_moyen.6<-	"Annee de fin"
	# interface_Bilan_taille
	msg$interface_Bilan_taille.1<-	"BILAN TAILLES"
	msg$interface_Bilan_taille.2<-"requete croisee taille/ caract qualitative"# tooltip
# interface_Bilan_ConditionEnv
	msg$interface_BilanConditionEnv.1<-"Chargement des stations de mesure \n"
	msg$interface_BilanConditionEnv.2<-"graphe bilan" # tootip
	msg$interface_BilanConditionEnv.3<-"tables bilan en .csv"
	msg$interface_BilanConditionEnv.4<-"Quitter"
# interface_BilanEspeces
	msg$interface_BilanEspeces.1<-"Bilan des especes presentes sur le DC\n"
	msg$interface_BilanEspeces.2<-"Bilan Especes" # glabel title
	msg$interface_BilanEspeces.3<-"Chargement"	
	msg$interface_BilanEspeces.4<-"Graphe PieChart"
	msg$interface_Bilanespeces.5<-"Histogramme"
	msg$interface_BilanEspeces.6<-"Tables bilan en .csv et XML"	
	msg$interface_BilanEspeces.7<-"Choix du decoupage" # label de la boite liste
# interface_BilanMigration
	msg$interface_BilanMigration.1<-"Chargement des listes taxons et stades et dc\n"
	msg$interface_BilanMigration.2<-"Calcul des effectifs par pas de temps" # tooltip
	msg$interface_BilanMigration.3<-"Graphe bilan"
	msg$interface_BilanMigration.4<-"Graphe cumul"
	msg$interface_BilanMigration.5<-"Tables bilan en .csv"
# interface_BilanMigrationConditionEnv
	msg$interface_BilanMigrationConditionEnv.1<-"calcul des condition environnementales par pas de temps" # 
	msg$interface_BilanMigrationConditionEnv.2<-"Chargement des listes taxons et stades et dc et stations de mesure \n"
# interface_BilanMigrationInterannuelle
	msg$interface_BilanMigrationInterannuelle.1<-"Chargement des bilanJournaliers existants\n"
	msg$interface_BilanMigrationInterannuelle.2<-"L'annee de debut a ete choisie\n"
	msg$interface_BilanMigrationInterannuelle.3<-"Annee de debut"
	msg$interface_BilanMigrationInterannuelle.4<-"L'annee de fin a ete choisie\n"
	msg$interface_BilanMigrationInterannuelle.5<-"Annee de fin"
	msg$interface_BilanMigrationInterannuelle.6<-"Migration de toutes les annees dans le meme graphique" # tooltip
	msg$interface_BilanMigrationInterannuelle.7<-"cumul migratoires en %" # tooltip
	msg$interface_BilanMigrationInterannuelle.8<-"Tableau" # tooltip
	msg$interface_BilanMigrationInterannuelle.9<-"Quitter" # tooltip
	msg$interface_BilanMigrationInterannuelle.10<-"jour"
	msg$interface_BilanMigrationInterannuelle.11<-"Migration journalière"
	msg$interface_BilanMigrationInterannuelle.12<-"sem"
	msg$interface_BilanMigrationInterannuelle.13<-"Migration hebdomadaire"
	msg$interface_BilanMigrationInterannuelle.14<-"quin"
	msg$interface_BilanMigrationInterannuelle.15<-"Migration par quinzaine"
	msg$interface_BilanMigrationInterannuelle.16<-"mois"
	msg$interface_BilanMigrationInterannuelle.17<-"Migration mensuelle"
# interface_BilanMigrationPar
	msg$interface_BilanMigrationPar.1<-"Chargement des listes taxons,stades,dc, parametres qualitatifs et quantitatifs  \n"
	msg$interface_BilanMigrationPar.2<-"Choix du type de lot, inclusion des echantillons ?"
	msg$interface_BilanMigrationPar.3<-"Calcul des effectifs par pas de temps"
	msg$interface_BilanMigrationPar.4<-"graphe mensuel"
	msg$interface_BilanMigrationPar.5<-"graphe journalier"
	msg$interface_BilanMigrationPar.6<-"tables bilan en .csv"
	msg$interface_BilanMigrationPar.7<-"quitter"
# interface_FonctionnementDC
	msg$interface_BilanFonctionnementDC.1<-"Chargement des listes dc et choix pas de temps\n"
	msg$interface_BilanFonctionnementDC.2<-"Graphe mensuel"# tootip
	msg$interface_BilanFonctionnementDC.3<-"Diagramme en boites"
	msg$interface_BilanFonctionnementDC.4<-"tableau"
	msg$interface_BilanFonctionnementDC.5<-"Quitte"
	
# interface graphique
	msg$interface_graphique.1<-"Calculs du fonctionnement du df\n"
	msg$interface_graphique.2<-"Calculs du fonctionnement du dc\n"
	msg$interface_graphique.3<-"Bilan des operations d'un dispositif ...a developper\n"
	msg$interface_graphique.4<-"Bilan croises du fonctionnement du DF et du DC,a developper \n"
	msg$interface_graphique.5<-"Bilan migration (pour une espèce et un stade)\n"
	msg$interface_graphique.6<-"Bilan migration inter-annuel\n"
	msg$interface_graphique.7<-"Bilan migration conditions environnementales\n"
	msg$interface_graphique.8<-"Bilan migration avec parametres\n"
	msg$interface_graphique.9<-"Bilan des conditions environnementales\n"
	msg$interface_graphique.10<-"Bilan lots par appel de la vue vue lot ope\n"
	msg$interface_graphique.11<-"Bilan tailles \n"
	msg$interface_graphique.12<-paste("Bilan des poids moyens en vue du calcul des relations poids effectif.\n",
			"Cette interface est adaptee pour le recalcul du poids moyen des civelles pendant la saison d'arrivee :\n",
			"-la base permet de rentrer des quantites de lots c'est a dire de peser les civelles au lieu de les denombrer.\n",
			"Le poids moyen des civelles non egoutees est alors utilise pour calculer les effectifs.\n",
			"Ce calcul peut etre fait de deux manieres, soit on utilise une relation standard (preciser ici le calcul...),\n",
			"soit on utilise une regression du poids moyen a l'aide d'un modele sinusoidal =>",
			"cette interface vous propose d'analyser les donnees puis de recalculer un modele\n",
			"et enfin de reimporter les donnees dans la base, un erreur apparaitra si cette procedure d'import a deja ete effectuee\n",sep="")
	msg$interface_graphique.13<-"Calcul des stades pigmentaires \n"
	msg$interface_graphique.14<-paste("Pour de l'aide cedric Briand - 02 99 90 88 44 - cedric.briand@lavilaine.com\n",
			"http://www.eptb-vilaine.fr/site/index.php/telechargement2\n",
			"dans R :'? stacomi' pour une aide html sur le package\n")
	msg$interface_graphique.15<-"Lancement de Rcmdr ( menu donnees, jeu de donnees actif, selectionner le jeu de donnees actif\n"
	msg$interface_graphique.16<-"Traitement migrateur" # TITRE PRINCIPAL !!!
	msg$interface_graphique.17<-"TODO à développer" 
	msg$interface_graphique.18<-"sorties du programme\n"
	msg$interface_graphique.19<-"Bilan des espèces du DC\n"
	msg$interface_graphique_menu.1<-"Station"
	msg$interface_graphique_menu.1.1<-"DF"
	msg$interface_graphique_menu.1.2<-"DC"
	msg$interface_graphique_menu.1.3<-"Operation (TODO)"
	msg$interface_graphique_menu.1.4<-"DF sans DC (TODO)"
	msg$interface_graphique_menu.2<-"Bilan"
	msg$interface_graphique_menu.2.1<-"Migration"
	msg$interface_graphique_menu.2.2<-"Cond. Env."
	msg$interface_graphique_menu.2.3<-"Migr.~Cond. Env."
	msg$interface_graphique_menu.2.4<-"Migr./ parm. quant / parm. qual"
	msg$interface_graphique_menu.2.5<-"Migr. interannuel"
	msg$interface_graphique_menu.2.6<-"Parm. de lot"
	msg$interface_graphique_menu.2.7<-"Poids Moyen civelle"
	msg$interface_graphique_menu.2.8<-"Tailles"
	msg$interface_graphique_menu.2.9<-"Stades pigmentaires"
	msg$interface_graphique_menu.2.10<-"Migr. juvéniles (TODO)"
	msg$interface_graphique_menu.2.11<-"Migr. inter DC (TODO)"
	msg$interface_graphique_menu.2.12<-"parm qual (proportions) (TODO)"
	msg$interface_graphique_menu.2.13<-"Especes"
	msg$interface_graphique_menu.3<-"Aide"
# 	
	msg$interface_graphique_log.1<-"Connexion"
	msg$interface_graphique_log.2<-"Utilisateur"
	msg$interface_graphique_log.3<-"Mot de passe"
	msg$interface_graphique_log.4<-"Login"
	msg$interface_graphique_log.5<-"Erreur" # title of the frame
	msg$interface_graphique_log.6<-"Probleme lors du test de la connexion ODBC" 
	msg$interface_graphique_log.7<-"Erreur dans l'utilisation de la methode connect de la classe ConnexionODBC" 
	msg$interface_graphique_log.8<-"Probleme lors du test, le lien ODBC fonctionne mais ne pointe pas vers la base version 0.3, verifiez le lien ODBC"
	msg$interface_graphique_log.9<-"Lien ODBC"
	assign("msg",msg,envi=envir_stacomi)
}