# Nom fichier :        funtraitement_poids
# Projet :             calmig/prog/fonctions
# Organisme :          IAV/CSP
# Auteur :             Cedric Briand
# Contact :            cedric.briand00@gmail.com
# Date de creation :   17 mai 2004
# Compatibilite :      R 2.8.0
# Etat :               OK
# Description           tansformation des donnees de poids en nombre

#' returns a table where weights and number are calculated from number and weights respectively
#' performs a query to collect the conversion coefficients
#' @param tableau 
#' @param duree 
#' @returnType data.frame
#' @return tableau
#' @author Cedric Briand \email{cedric.briand00@@gmail.com}
#' @export
funtraitement_poids=function(tableau,duree) { 
	index=tableau$No.pas+1
	funout(get("msg",envir=envir_stacomi)$funtraitement_poids.1)
	
	tableaupoids=subset(tableau,tableau$Type_de_quantite==unique(tableau$Type_de_quantite)[2])
	tableaueffectif=subset(tableau,tableau$Type_de_quantite==unique(tableau$Type_de_quantite)[1])
	# Conversion des  poids en effectifs
	tableauconvert=tableaupoids[,2:6]
	tableauconvert=tableauconvert*tableaupoids$Coef_conversion       # les coeff sont du type 2.54 et non 0.3
	if (sum(tableaupoids$Coef_conversion)==0) funout(get("msg",envir=envir_stacomi)$funtraitement_poids.2)
	# creation d'une tableau (matricepoids) à 5 colonnes comprenant les effectifs convertis
	matricepoids=cbind(tableaupoids[,1],tableauconvert,tableaupoids[,2:6],tableaupoids$Coef_conversion)
	dimnames(matricepoids)=list(1:length(tableaupoids[,1]),c(
					"No.pas",
					"Mesure",
					"Calcule",
					"Expert",
					"Ponctuel",
					"Effectif_total",
					"poids_Mesure",
					"poids_Calcule",
					"poids_Expert",
					"poids_Ponctuel",
					"Poids_total",
					"Coef_conversion"))
	
	tableau=merge(tableaueffectif, matricepoids, all.x = TRUE,by.x="No.pas",by.y="No.pas",
			sort = TRUE, suffixes=c(".e",".p"))
	# je vire les NA
	tableau[is.na(tableau)]=0
	tableau$Mesure=tableau$Mesure.e+tableau$Mesure.p        
	tableau$Calcule=tableau$Calcule.e+tableau$Calcule.p 
	tableau$Expert=tableau$Expert.e+tableau$Expert.p
	tableau$Ponctuel=tableau$Ponctuel.e+tableau$Ponctuel.p 
	tableau$Effectif_total=tableau$Effectif_total.e+tableau$Effectif_total.p
	
	# recuperation des coefficients de conversion quantite effectif
	req=new("RequeteODBCwheredate")
	req@baseODBC<-baseODBC
	req@select="select * from tj_coefficientconversion_coe"
	req@datedebut<-as.POSIXlt(duree[min(index)])
	req@datefin<-as.POSIXlt(duree[max(index)])
	req@colonnedebut<-"coe_date_debut"
	req@colonnefin<-"coe_date_fin"
	req@and<-c("and coe_tax_code='2038'","and coe_std_code='CIV'")
	req@order_by<-"order by coe_date_debut"
	req<-connect(req)
	coe<-req@query
	coe$coe_date_debut
	#annees bissextiles and missing data
	if (length(coe$coe_valeur_coefficient)<length(tableau$Effectif_total.e)){
		if (sum(tableaupoids$Coef_conversion)>0){			
		funout(get("msg",envir=envir_stacomi)$funtraitement_poids.3)#there are some coefficient but they are incomplete
		warnings(get("msg",envir=envir_stacomi)$funtraitement_poids.3)
		}
		mtch<-match(as.character(coe$coe_date_debut),as.character(as.Date(round(duree,"day"))))
		tableau$poids_depuis_effectifs<-0  
		tableau[mtch,"poids_depuis_effectifs"]=tableau[mtch,"Effectif_total.e"]/
				coe$coe_valeur_coefficient
		#tableau$Coef_conversion.e[toto]<-coe$coe_valeur_coefficient
	}   else {
		tableau$poids_depuis_effectifs=tableau$Effectif_total.e/coe$coe_valeur_coefficient
		tableau$Coef_conversion.e<-1/coe$coe_valeur_coefficient
		
	}
	
	#Je reordonne les donnees
	tableau=tableau[,c(1,7:8,20,2:6,10:19,21:26)]
	return(tableau)
}


#