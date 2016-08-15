# Nom fichier :        funSousListeBilanMigration
# Projet :             calcmig/prog/fonctions
# Organisme :          IAV/CSP
# Auteur :             Cedric Briand
# Contact :            cedric.briand"at"eptb-vilaine.fr
# Date de creation :   23/05/2006
# Compatibilite :      R 2.8.0
# Etat :               fonctionne
# Description          Workhorse fonction pour le calcul des bilans migratoires 
#**********************************************************************
#*
#* Modifications :








#' funSousListeBilanMigration
#' 
#' workhorse function for bilanMigration. Calculates the number for a stage and
#' a taxa per day.  The operation for the fishway is never from 00:00 to 00:00
#' so the number per day is calculated according to the ration between the
#' duration of the operation and the duration of the day. This function will
#' allow daily reports to be saved into the database when graph is launched
#' 
#' 
#' @param bilanMigration an object of class \code{\linkS4class{BilanMigration}}
funSousListeBilanMigration=function(bilanMigration) {
	# *********************
	# Boucle sur chacune des periodes du pas de temps
	# *********************
	req=new("RequeteODBC")
	req@baseODBC<-get("baseODBC", envir=envir_stacomi)
	req@open<-TRUE
	progres<-utils::winProgressBar(title = "calcul des effectifs par pas de temps",
			label = "progression %",
			min = 0,
			max = 1, 
			initial = 0,
			width = 400)
	##############################			
	on.exit(close(progres)) # fermeture de la barre de progres
	on.exit(if(!is.null(req@connection)) odbcClose(req@connection))   # ne pas lancer en debug
	##############################"
	##debug           
	#  bilanMigration@pasDeTemps<-get("pasDeTemps",envir_stacomi)  
	#bilanMigration@pasDeTemps@noPasCourant=as.integer(-(difftime(as.POSIXlt(strptime("2006-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S")),as.POSIXlt(strptime("2006-03-27 00:00:00",format="%Y-%m-%d %H:%M:%S")),unit="days")))  
	#bilanMigration@pasDeTemps@noPasCourant=as.integer(264)  
	
	dateFin=strftime(as.POSIXlt(DateFin(bilanMigration@pasDeTemps)),format="%Y-%m-%d %H:%M:%S")
	while (getnoPasCourant(bilanMigration@pasDeTemps) != -1) {
		zz=(getnoPasCourant(bilanMigration@pasDeTemps)+1)/bilanMigration@pasDeTemps@nbPas
		utils::setWinProgressBar(progres,zz,title="calcul des effectifs par pas de temps",label=sprintf("%d%% progression",round(100*zz)))                    
		debutPas = as.POSIXlt(currentDateDebut(bilanMigration@pasDeTemps))
		finPas = as.POSIXlt(currentDateFin(bilanMigration@pasDeTemps))
		if(finPas!=round(finPas,"day")) stop("problemes d'arrondi dans le calcul de la date, verifier la fonction funsouslistebilanmigration")
		# finPas= as.POSIXlt(DateFin(bilanMigration@pasDeTemps))
		#cat(paste("pas courant=",bilanMigration@pasDeTemps@noPasCourant,"\n") )
		#cat(paste("duree du pas",format(debutPas) ," -> ", format(finPas),"\n"))
		
		
		
		# *********************
		#
		# Taux d'echappement pour le pas de temps courant
		#
		# *********************
		
		
		# Si un seul taxon et un seul stade recherche du taux d'echappement
		if ((length(bilanMigration@taxons@data$tax_code) == 1) && (length(bilanMigration@stades@data$std_code) == 1)) {
			
			# recherche des taux qui recoupent la periode du pas de temps
			req@sql = paste(
					" SELECT txe_date_debut, txe_date_fin, txe_valeur_taux " ,
					" FROM   ",get("sch",envir=envir_stacomi),"tj_tauxechappement_txe " ,
					" WHERE  txe_tax_code = '" , as.character(bilanMigration@taxons@data$tax_code) , "'" ,
					" AND txe_std_code = '" , as.character(bilanMigration@stades@data$std_code) , "'" ,
					" AND txe_sta_code ='" , as.character(bilanMigration@dc@station) , "'" ,     
					" AND txe_valeur_taux IS NOT NULL" ,
					" AND (txe_date_debut, txe_date_fin) OVERLAPS (TIMESTAMP '" , getdateDebut(bilanMigration@pasDeTemps) , "', TIMESTAMP '" , dateFin , "')" ,
					" ;",sep="" )
			
			#cat(paste("Requete SQL : \n" , sql,  "\n"))
			req<-connect(req)
			rs=req@query
			lesTauxEch=rs$txe_valeur_taux
			if (length(lesTauxEch) > 0) {
				# Recherche des coefficients pour ponderer le taux et des dates d'application des taux
				datesDebutTauxEch=as.POSIXlt(rs$txe_date_debut)
				datesFinTauxEch=as.POSIXlt(rs$txe_date_fin)
				
				# Traitement special pour le premier et le dernier taux
				
				# Si le premier taux commence avant la periode du pas, on le modifie pour qu'il commence en meme temps que le pas
				if (datesDebutTauxEch[1]<debutPas) {
					datesDebutTauxEch[1]<-debutPas
				}
				
				# Si le dernier taux termine apres la periode du pas, on le modifie pour qu'il termine en meme temps que le pas
				if (datesFinTauxEch[length(datesFinTauxEch)]>finPas) {
					datesFinTauxEch[length(datesFinTauxEch)]<-finPas
				}
				
				
				tauxEch = 0
				cumulPeriodes = 0  # les durees cumulees des periodes d'application des taux
				periodePas = 0  # var temporaire pour la duree d'application d'un certain taux
				
				# Boucle sur chaque taux et application d'un coefficient de ponderation
				for (i in 1: length(lesTauxEch)) {
					# tauxI * dureeI
					periodePas = datesFinTauxEch[i] - datesDebutTauxEch[i]
					tauxEch = tauxEch + as.double(lesTauxEch[i]*periodePas)
					
					cumulPeriodes = as.double(cumulPeriodes + periodePas)
					
				}
				# Divise par la duree cumulee pour retomber sur un taux
				if (cumulPeriodes != 0) {
					# Division par le cumul des periodes des taux et non par la periode du pas de temps pour le cas ou les taux ne seraient pas definis sur la totalite du pas
					tauxEch = tauxEch / cumulPeriodes
				}else {
					# erreur
					tauxEch = -1
				}
				
			} else {
				# Le taux n'est pas calculable
				tauxEch = -1
			}
			
			#cat(paste("Taux pondere : " , tauxEch, "\n"))
			
			
			
			
		}    #fin du if
		# *********************
		#
		# Coeff de conversion quantite-effectif pour le pas de temps courant et pour chaque type de quantite
		#
		# *********************
		
		
		
		# Si un seul taxon et un seul stade recherche du coef
		if ((length(bilanMigration@taxons@data$tax_code) == 1) && (length(bilanMigration@stades@data$std_code) == 1)) {
			
			
			# recherche des coef qui recoupent la periode du pas de temps
			req@sql = paste(" SELECT coe_date_debut, coe_date_fin, coe_valeur_coefficient, qte_libelle" ,
					" FROM   ",get("sch",envir=envir_stacomi),"tj_coefficientconversion_coe " ,
					" INNER JOIN ref.tr_typequantitelot_qte ON coe_qte_code = qte_code" ,
					" WHERE  coe_tax_code = '" , bilanMigration@taxons@data$tax_code , "'" ,
					" AND coe_std_code = '" , bilanMigration@stades@data$std_code, "'" ,
					" AND (coe_date_debut, coe_date_fin) OVERLAPS (DATE '" , debutPas , "', DATE '" , finPas , "')" ,
					" ORDER BY coe_date_debut, qte_libelle " ,
					" ;",sep="" )
			
			#cat(paste("Requete SQL : \n" , req@sql,  "\n"))
			req=connect(req)
			rs=stacomirtools::killfactor(req@query) # pour eviter certains pb
			coef=NULL
			if (nrow(rs)>0){
				# Recherche des poids pour ponderer le coef et des dates d'application des coef
				
				dateDebutCoef=as.POSIXlt(rs$coe_date_debut)  # le vecteur date de debut d'application d'un taux
				dateFinCoef=as.POSIXlt(rs$coe_date_fin)  # le vecteur date de fin d'application d'un taux
				coef=rs$coe_valeur_coefficient  # le vecteur valeur du taux
				type=as.character(rs$qte_libelle) ; # le vecteur type de quantite
				
				
				# if (nrow(rs)>0){
				# le nombre de lignes du resultset est positif est il existe datedebut coeff...
				
				# Traitement special pour le premier et le dernier taux
				# Si le coef commence avant le pas de temps courant, sa periode est reduite pour commencer au debut du pas
				# reindexation du vecteur
				dateDebutCoef[dateDebutCoef<debutPas] = debutPas
				
				
				# Si le coef se termine apres la fin du pas, sa periode est reduite pour se terminer a la fin du pas
				# reindexation du vecteur
				dateFinCoef[dateFinCoef>finPas] = finPas
				
				
				# application d'un coefficient de ponderation (vecteur periode)
				periode = difftime(as.POSIXct(dateFinCoef) ,as.POSIXct(dateDebutCoef),units="secs")
				stopifnot(!is.na(periode))
				# meme si la periode est fausse elle est calculee au numerateur et au denominateur
				
				# on multiplie le vecteur coeff par le vecteur des periodes pour obtenir une moyenne ponderee
				coef = as.double(coef * periode)
				# coef est un object difftime avec methode sum non definie si je ne marque pas as.double
				lescoeff=list()
				# lescoeff = une liste dont le nom correspond au type et qui contient le coeff pondere
				# on passe en revue les type de coefficient de conversion possible
				# dans l'immense majorite des cas il n'y en aura qu'un (resistivite ou poids)
				if (length(unique(type))>=0){
					for (letype in as.character(unique(type)) ) {						
						lescoeff[[letype]]=sum(coef[type==letype])/sum(as.double(periode[type==letype]))						
						# Pour chaque type de quantite, divise le cumul des coefs par la duree cumulee pour retomber sur un coef
						# le taux correspondant a chaque type de quantite
					}						
				}else { coef = -1}# pas de types de coeff
			}else { coef = -1}# pas de lignes				
		} else { coef = -1#cat(paste("Coef de conversion non calculables pour plus d'une espece + taxon",  "\n"))
		}	
		
		
		
		
		# *********************
		#
		# Operations concernees par le pas de temps
		#
		# *********************
		
		# Preparation des criteres de selection
		# en pratique il n'est pas possible de choisir plusieurs taxons dans la combobox, et tant mieux par les coeff d'echappement
		# et de coefficient de conversion ne s'appliquent qu'a un couple taxon stade
		
		# DC
		dcCode = as.character(bilanMigration@dc@dc_selectionne)
		
		# Taxon (1 ou plusieurs)
		complementTax = paste(" AND ( lot_tax_code='" , bilanMigration@taxons@data$tax_code[1], "'",sep="")
		if(length(bilanMigration@taxons@data$tax_code)>1){
			for (i in 2:length(bilanMigration@taxons@data$tax_code)){
				complementTax = paste(complementTax , " OR lot_tax_code='" , bilanMigration@taxons@data$tax_code[i],"'",sep="" )
			}
		}
		complementTax = paste(complementTax , ")" )
		
		
		# Stade (1 ou plusieurs)
		complementStade = paste(" AND ( lot_std_code='" ,bilanMigration@stades@data$std_code[1], "'",sep="")
		if(length(bilanMigration@stades@data$std_code)>1){
			for (i in 2:length(bilanMigration@stades@data$std_code)){
				complementStade = paste(complementStade , " OR lot_std_code='" ,bilanMigration@stades@data$std_code[i], "'" ,sep="")
			}
		}
		complementStade = paste(complementStade ,")")
		
		
		# ---------------------
		#
		#  Lots avec effectif
		#
		# ---------------------
		
		# recherche des operations qui recoupent la periode du pas de temps
		# Calcul de la somme des effectifs
		# On ne prend pas les echantillons
		req@sql=paste(" SELECT ope_date_debut, ope_date_fin, lot_methode_obtention, SUM(lot_effectif) AS effectif " ,
				" FROM   ",get("sch",envir=envir_stacomi),"t_operation_ope " ,
				"        INNER JOIN ",get("sch",envir=envir_stacomi),"t_lot_lot ON ope_identifiant = lot_ope_identifiant " ,
				" WHERE  ope_dic_identifiant ='" , dcCode , "' ",
				"        AND lot_effectif IS NOT NULL " ,
				"        AND lot_lot_identifiant IS NULL " ,
				complementTax ,
				complementStade ,
				"        AND (ope_date_debut, ope_date_fin) OVERLAPS (TIMESTAMP '" , debutPas , "', TIMESTAMP '" , finPas , "')" ,
				" GROUP BY ope_date_debut, ope_date_fin, lot_methode_obtention" ,
				" ORDER BY ope_date_debut " ,
				" ;" ,sep="")
		
		#cat(paste("Requete SQL : \n" , req@sql))
		req=connect(req)
		rs=req@query
		
		if (nrow(rs)>0){
			
			debutOpe=as.POSIXlt(rs$ope_date_debut)
			finOpe= as.POSIXlt(rs$ope_date_fin)
			finOpe[finOpe==debutOpe]<-finOpe+1 # pour �viter les divisions par z�ro pour les op�rations de 0s
			methode=rs$lot_methode_obtention
			effectif=rs$effectif
			
			# creation des sommes effectif_MESURE ...
			
			# Si l'operation commence avant le pas de temps courant, et ne se termine pas apres, il faut conserver une seule partie de l'operation
			# Si l'operation se termine apres la fin du pas mais ne debute pas avant, il faut conserver une seule partie de l'operation
			# Si l'operation commence avant le pas de temps et se termine apres, on ne conserve qu'une partie de l'operation
			# Cas ou l'operation est inferieure ou egale au pas de temps : pas de probleme, on compte l'operation complete
			# ce qui revient � dire que pour ce qui concerne la duree de l'operation effectif sur le pas de temps
			# on prends le max du debut de ope et pas de temps (si l'ope commence avant on garde pas cette partie )
			# et pour la fin on prend le min si l'ope se termine apres on garde pas... ouf
			# et que se passe t'il pour plusieurs op�rations dans la m�me journ�e ????
			debut<-debutOpe
			fin<-finOpe
			debut[debut<debutPas]<-debutPas
			fin[fin>finPas]<-finPas
			# debut et fin correspondent au troncage des op�rations qui d�passent du pas
			# Repartition de l'effectif au prorata
			effectif = effectif * as.double(difftime(time1=fin, time2=debut,units =  "secs"))/as.double(difftime(time1=finOpe,time2=debutOpe,units =  "secs")) 
			
			for (i in c("MESURE","CALCULE","EXPERT","PONCTUEL")){
				assign(eval(paste("effectif_",i,sep="")),sum(effectif[methode==i]))
			}
		} else {
			# dans le cas ou le resultat de la requete est vide pas de ligne je met 0
			# pour les effectifs
			for (i in c("MESURE","CALCULE","EXPERT","PONCTUEL")){
				assign(eval(paste("effectif_",i,sep="")),0)
			}
			effectif=0
		}
		
		# Les effectifs sont classes par methode d'obtention
		
		# cat(paste("Effectif MESURE : " ,effectif_MESURE," \n"))
		#cat(paste("Effectif CALCULE : " , effectif_CALCULE," \n"))
		#cat(paste("Effectif EXPERT : " , effectif_EXPERT, " \n"))
		#cat(paste("Effectif PONCTUEL : " , effectif_PONCTUEL, " \n"))
		
		
		# Enregistrement des donnees du pas courant dans le fichier de sortie
		
		
		if (!exists("tablecalcmig")){
			tablecalcmig=data.frame(
					"No_pas"=bilanMigration@pasDeTemps@noPasCourant   ,
					"debut_pas"=debutPas       ,
					"fin_pas"=finPas           ,
					"MESURE"=effectif_MESURE   ,
					"CALCULE"=effectif_CALCULE  ,
					"EXPERT"=effectif_EXPERT    ,
					"PONCTUEL"=effectif_PONCTUEL  ,
					"type_de_quantite"="effectif",    # dans le cas suivant par exemple type = "poids"
					"taux_d_echappement" = tauxEch,
					"coe_valeur_coefficient"=as.numeric(NA))
		} else   {
			tablecalcmig=rbind(tablecalcmig,
					data.frame(
							"No_pas"=bilanMigration@pasDeTemps@noPasCourant   ,
							"debut_pas"=debutPas       ,
							"fin_pas"=finPas           ,
							"MESURE"=effectif_MESURE   ,
							"CALCULE"=effectif_CALCULE  ,
							"EXPERT"=effectif_EXPERT    ,
							"PONCTUEL"=effectif_PONCTUEL  ,
							"type_de_quantite"="effectif",    # dans le cas suivant par exemple type = "poids"
							"taux_d_echappement" = tauxEch,
							"coe_valeur_coefficient"=as.numeric(NA)))
		}
		
		# ---------------------
		#
		# Lots avec quantite
		#
		# ---------------------
		
		#plusieurs resultats possibles par type de quantite
		
		# recherche des operations qui recoupent la periode du pas de temps
		# On ne prend pas les echantillons
		req@sql = paste( "SELECT ope_date_debut, ope_date_fin, lot_methode_obtention, qte_libelle, SUM(lot_quantite) AS quantite " ,
				" FROM   ",get("sch",envir=envir_stacomi),"t_operation_ope " ,
				"        INNER JOIN ",get("sch",envir=envir_stacomi),"t_lot_lot ON ope_identifiant = lot_ope_identifiant " ,
				"        INNER JOIN ref.tr_typequantitelot_qte ON lot_qte_code = qte_code" ,
				" WHERE  ope_dic_identifiant ='" , dcCode , "' ",
				"        AND lot_quantite IS NOT NULL " ,
				"        AND lot_lot_identifiant IS NULL " ,
				complementTax ,
				complementStade ,
				"        AND (ope_date_debut, ope_date_fin) OVERLAPS (TIMESTAMP '" , debutPas , "', TIMESTAMP '" , finPas , "')" ,
				" GROUP BY ope_date_debut, ope_date_fin, lot_methode_obtention, qte_libelle" ,
				" ORDER BY ope_date_debut " ,
				" ;" ,sep="")
		
		#cat(paste("Requete SQL : \n" , sql))
		req=connect(req)
		rs=stacomirtools::killfactor(req@query)
		
		if (nrow(rs)>0){
			debutOpe=as.POSIXlt(rs$ope_date_debut)
			finOpe= as.POSIXlt(rs$ope_date_fin)
			methode=as.character(rs$lot_methode_obtention)
			quantite=rs$quantite
			typeQte=as.character(rs$qte_libelle)
			
			
			debut<-debutOpe
			fin<-finOpe
			debut[debut<debutPas]<-debutPas
			fin[fin>finPas]<-finPas
			
			# Repartition de l'effectif au prorata
			quantite = quantite * as.double(difftime(time1=fin, time2=debut,units =  "secs"))/as.double(difftime(time1=finOpe,time2=debutOpe,units =  "secs")) 
			
			
			#premiere boucle sur le type de quantite, poids resistivite...
			for (j in as.character(unique(typeQte))){
				# cat(j,"\n") # j=unique(typeQte)[1]
				# ecriture d'une ligne par type de qte
				for (i in c("MESURE","CALCULE","EXPERT","PONCTUEL")){
					assign(eval(paste("quantite_",i,sep="")),sum(quantite[methode==i & typeQte==j]))
				}
				# le tableau a ete cree lors du passage dans le calcul des effectifs (requete precedente)
				tablecalcmig=rbind(tablecalcmig,
						data.frame(
								"No_pas"=bilanMigration@pasDeTemps@noPasCourant   ,
								"debut_pas"=as.POSIXct(debutPas)       ,
								"fin_pas"=as.POSIXct(finPas)           ,
								"MESURE"=quantite_MESURE   ,
								"CALCULE"=quantite_CALCULE  ,
								"EXPERT"=quantite_EXPERT    ,
								"PONCTUEL"=quantite_PONCTUEL  ,
								"type_de_quantite"=j,    # dans le cas suivant par exemple type = "poids"
								"taux_d_echappement" = tauxEch,
								"coe_valeur_coefficient"=ifelse( !is.null(lescoeff[[j]]), as.numeric(lescoeff[[j]]), NA) ))  #
				
			} # fin de la boucle d'ecriture par type de qte
		}  else {# fin du if nrow>0
			
			# dans le cas ou le resultat de la requete est vide 0 rows, je n'ecrit rien
		}
		
		
		# Avance au pas de temps suivant
		bilanMigration@pasDeTemps=suivant(bilanMigration@pasDeTemps)
	}     # end boucle while
	
#odbcClose(channel)
	close(progres)
#close(progres) maintenant lance avec on.exit pour eviter les affichages intempestifs en cas de bug
	
	return (tablecalcmig)
	
}               # end SousListeBilanMigration

