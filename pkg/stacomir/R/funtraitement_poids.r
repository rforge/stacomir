#' returns a table where weights and number are calculated from number and weights respectively
#' performs a query to collect the conversion coefficients
#' @param tableau A table passed BilanMigration
#' @param time.sequence Time sequence passed from BilanMigration
#' @return tableau
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
funtraitement_poids=function(tableau,time.sequence) { 
	index=tableau$No.pas+1
	funout(gettext("Conversion weight / number\n"))
	
	tableaupoids=subset(tableau,tableau$type_de_quantite==unique(tableau$type_de_quantite)[2])
	tableaueffectif=subset(tableau,tableau$type_de_quantite==unique(tableau$type_de_quantite)[1])
	# Conversion des  poids en effectifs
	tableauconvert=tableaupoids[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total")]
	tableauconvert=tableauconvert*tableaupoids$coe_valeur_coefficient       # les coeff sont du type 2.54 et non 0.3
	if (sum(tableaupoids$coe_valeur_coefficient)==0) funout(gettext("Attention sum=0, you didn't enter the coefficient of conversion\n"))
	# creation d'une tableau (matricepoids) a 5 colonnes comprenant les effectifs convertis
	matricepoids=cbind(tableaupoids[,1],tableauconvert,tableaupoids[,c("MESURE","CALCULE","EXPERT","PONCTUEL","Effectif_total")],tableaupoids$coe_valeur_coefficient)
	dimnames(matricepoids)=list(1:length(tableaupoids[,1]),c(
					"No.pas",
					"MESURE",
					"CALCULE",
					"EXPERT",
					"PONCTUEL",
					"Effectif_total",
					"poids_MESURE",
					"poids_CALCULE",
					"poids_EXPERT",
					"poids_PONCTUEL",
					"Poids_total",
					"coe_valeur_coefficient"))
	
	tableau=merge(tableaueffectif, matricepoids, all.x = TRUE,by.x="No.pas",by.y="No.pas",
			sort = TRUE, suffixes=c(".e",".p"))
	# je vire les NA
	tableau[is.na(tableau)]=0
	tableau$MESURE=tableau$MESURE.e+tableau$MESURE.p        
	tableau$CALCULE=tableau$CALCULE.e+tableau$CALCULE.p 
	tableau$EXPERT=tableau$EXPERT.e+tableau$EXPERT.p
	tableau$PONCTUEL=tableau$PONCTUEL.e+tableau$PONCTUEL.p 
	tableau$Effectif_total=tableau$Effectif_total.e+tableau$Effectif_total.p
	
	# recuperation des coefficients de conversion quantite effectif
	req=new("RequeteODBCwheredate")
	req@baseODBC<-get("baseODBC",envir=envir_stacomi)
	req@select="select * from tj_coefficientconversion_coe"
	req@datedebut<-as.POSIXlt(time.sequence[min(index)])
	req@datefin<-as.POSIXlt(time.sequence[max(index)])
	req@colonnedebut<-"coe_date_debut"
	req@colonnefin<-"coe_date_fin"
	req@and<-c("and coe_tax_code='2038'","and coe_std_code='CIV'")
	req@order_by<-"order by coe_date_debut"
	req<-stacomirtools::connect(req)
	coe<-req@query
	coe$coe_date_debut
	#annees bissextiles and missing data
	if (length(coe$coe_valeur_coefficient)<length(tableau$Effectif_total.e)){
		if (sum(tableaupoids$coe_valeur_coefficient)>0){			
		funout(gettext("Attention there are probably missing coefficients in the database, verify or the conversion from number to weights and weights to number might be wrong\n")) 
		warnings(gettext("Attention there are probably missing coefficients in the database, verify or the conversion from number to weights and weights to number might be wrong\n"))
		}
		mtch<-match(as.character(coe$coe_date_debut),as.character(as.Date(round(time.sequence,"day"))))
		tableau$poids_depuis_effectifs<-0  
		tableau[mtch,"poids_depuis_effectifs"]=tableau[mtch,"Effectif_total.e"]/
				coe$coe_valeur_coefficient
		#tableau$coe_valeur_coefficient.e[toto]<-coe$coe_valeur_coefficient
	}   else {
		tableau$poids_depuis_effectifs=tableau$Effectif_total.e/coe$coe_valeur_coefficient
		tableau$coe_valeur_coefficient.e<-1/coe$coe_valeur_coefficient
		
	}
	
	#Je reordonne les donnees
	tableau=tableau[,c(1,7:8,20,2:6,10:19,21:26)]
	return(tableau)
}


#