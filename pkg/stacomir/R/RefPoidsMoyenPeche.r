# Nom fichier :        RefPoidsMoyenPeche(classe)
# Projet :             controle migrateur
# Organisme :          IAV
# Auteur :             Cedric Briand
# Contact :            cedric.briand@lavilaine.com
# Date de creation :  31/03/2008 17:21:25 / modif10/01/2009 22:50:42
# Compatibilite :
# Etat :               
# Description          Classe appellee pour sa methode charge qui permet le chargement des lots de la base MORTCIV des peches experimentales
#                      entre une date de debut et une date de fin 
#                      Utilise dans Bilan_poids_moyen uniquement pour Arzal
#                      


setClass(Class="RefPoidsMoyenPeche",representation=
				representation(data="data.frame",datedebut="POSIXlt",datefin="POSIXlt"),
		prototype=prototype(data=data.frame()))
# pour test  
# objet= new("RefPoidsMoyenPeche")

#retourne la liste des annees presentes dans la base
setMethod("charge",signature=signature("RefPoidsMoyenPeche"),definition=function(objet){
			requete=new("RequeteODBCwheredate")
			requete@datedebut=objet@datedebut
			requete@datefin=objet@datefin
			requete@colonnedebut="datedebutpeche"
			requete@colonnefin="datefinpeche"
			requete@baseODBC=baseODBCmortciv
			requete@select="SELECT lot_identifiant,
					datedebutpeche AS ope_date_debut ,
					datefinpeche AS ope_date_fin,
					lot_effectif,
					car_valeur_quantitatif as poids,
					(car_valeur_quantitatif/lot_effectif) AS poids_moyen, 
					(datefinpeche-datedebutpeche)/2 AS duree,
					datedebutpeche+(datefinpeche-datedebutpeche)/2 AS datemoy,
					date_part('year', datedebutpeche) AS annee,
					date_part('month',datedebutpeche) AS mois  
					FROM vue_ope_lot_poids"
			requete@and=""
			requete=connect(requete)  
			objet@data<-requete@query
			return(objet)
		})
# pas de methode choix, le choix est dejà fait dans l'annee de l'interface
#charge(refPoidsMoyenPeche)