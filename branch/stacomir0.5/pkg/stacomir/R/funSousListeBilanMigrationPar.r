# Nom fichier :        funSousListeBilanMigrationPar
# Projet :             calcmig/prog/fonctions
# Organisme :          IAV/CSP
# Auteur :             Cedric Briand
# Contact :            cedric.briand@lavilaine.com
# Date de creation :   23/05/2006
# Compatibilite :      R 2.8.0
# Etat :               developpement
# Description          Workhorse fonction pour le calcul des bilans migratoires adaptee au travail sur parametres qualitatifs et quantitatifs
# Notes de devt                     Dans les requetes recuperer valeurs quan ou valeur qual ou l'interrogation croisee (trois cas differents)
#                      
#**********************************************************************
#*
#* Modifications :

# load(file="EXAMPLES/devt.Rdata")
#@param  bilanMigrationPar
#@return une liste de deux dataframe
funSousListeBilanMigrationPar=function(bilanMigrationPar) {
	
	# *********************
	#
	# Boucle sur chacune des periodes du pas de temps
	#
	# *********************
#la methode suivant fait passer le pas de temps courant à -1
	req=new("RequeteODBC")
	req@open<-TRUE
	assign("progres",winProgressBar(title = "cumul val. quant. par pas de temps",
					label = "progression %",
					min = 0,
					max = 1, 
					initial = 0,
					width = 400),
			envir = .GlobalEnv)
	##############################			
	on.exit(close(progres))
	on.exit(if(!is.null(req@connexion)) odbcClose(req@connexion))   # ne pas lancer en debug
	##############################"
# recuperation des valeurs possibles du parametre qualitatif (hors boucle)			
	if (bilanMigrationPar@parqual@data$par_nom!="aucune"){
		req@sql=paste("select val_identifiant from ref.tr_valeurparametrequalitatif_val where val_qal_code='",
				bilanMigrationPar@parqual@data$par_code,"';",sep="")
		rs<-connect(req)@query
		valeurs_qal=as.character(rs$val_identifiant)
		req@sql=paste("select val_libelle from ref.tr_valeurparametrequalitatif_val where val_qal_code='",
				bilanMigrationPar@parqual@data$par_code,"';",sep="")
		rs<-connect(req)@query
		libelle_qal=as.character(rs$val_libelle)
		valeurs_qal=c(valeurs_qal,"autre") # "tous" pour ceux qui n'ont pas de caracteristique qual correspondante
		req@sql=paste("select par_nom from ref.tg_parametre_par join ref.tr_parametrequalitatif_qal on qal_par_code=par_code where par_code='",    bilanMigrationPar@parqual@data$par_code,"';",sep="")
		rs<-connect(req)@query
		nomparm=rs$par_nom
		libelle_qal=c(libelle_qal,paste("Pas de parametre qualitatif :",nomparm)) 
		stopifnot(length(valeurs_qal)==length(libelle_qal))
	}
	
	dateFin=strftime(as.POSIXlt(DateFin(bilanMigrationPar@pasDeTemps)),format="%Y-%m-%d %H:%M:%S")
	##debug           
	#  bilanMigrationPar@pasDeTemps<-get("pasDeTemps",envir_stacomi)  
	#bilanMigrationPar@pasDeTemps@noPasCourant=as.integer(-(difftime(as.POSIXlt(strptime("2006-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S")),as.POSIXlt(strptime("2006-03-27 00:00:00",format="%Y-%m-%d %H:%M:%S")),unit="days")))  
	while (getnoPasCourant(bilanMigrationPar@pasDeTemps) != -1) {
		zz=(getnoPasCourant(bilanMigrationPar@pasDeTemps)+1)/bilanMigrationPar@pasDeTemps@nbPas
		setWinProgressBar(progres,zz,title="cumul val. quant. par pas de temps",label=sprintf("%d%% progression",round(100*zz)))                    
		debutPas = as.POSIXlt(currentDateDebut(bilanMigrationPar@pasDeTemps))
		finPas = as.POSIXlt(currentDateFin(bilanMigrationPar@pasDeTemps))
		#finPas=as.POSIXlt(DateFin(bilanMigrationPar@pasDeTemps)) # pour debug avoir quelque chose dans le resultset
		#cat(paste("pas courant=",bilanMigrationPar@pasDeTemps@noPasCourant,"\n") )
		#cat(paste("duree du pas",format(debutPas) ," -> ", format(finPas),"\n"))
		
		# *********************
		#
		# Taux d'echappement pour le pas de temps courant
		#
		# *********************
		
		
		# Si un seul taxon et un seul stade recherche du taux d'echappement
		if ((length(bilanMigrationPar@taxons@data$tax_code) == 1) && (length(bilanMigrationPar@stades@data$std_code) == 1)) {
			
			# recherche des taux qui recoupent la periode du pas de temps
			req@sql = paste(
					" SELECT txe_date_debut, txe_date_fin, txe_valeur_taux " ,
					" FROM   ",sch,"tj_tauxechappement_txe " ,
					" WHERE  txe_tax_code = '" , as.character(bilanMigrationPar@taxons@data$tax_code) , "'" ,
					" AND txe_std_code = '" , as.character(bilanMigrationPar@stades@data$std_code) , "'" ,
					" AND txe_ouv_identifiant ='" , as.character(bilanMigrationPar@dc@ouvrage) , "'" ,     
					" AND txe_valeur_taux IS NOT NULL" ,
					" AND (txe_date_debut, txe_date_fin) OVERLAPS (TIMESTAMP '" , getdateDebut(bilanMigrationPar@pasDeTemps) , "', TIMESTAMP '" , dateFin , "')" ,
					" ;",sep="" )
			
			#cat(paste("Requete SQL : \n" , sql,  "\n"))
			rs<-connect(req)@query
			# Recherche des coefficients pour ponderer le taux et des dates d'application des taux
			datesDebutTauxEch=as.POSIXlt(rs$txe_date_debut)
			datesFinTauxEch=as.POSIXlt(rs$txe_date_fin)
			lesTauxEch=rs$txe_valeur_taux
			
			# Traitement special pour le premier et le dernier taux
			if (length(lesTauxEch) > 0) {
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
				for (i in 1: lenth(lesTauxEch)) {
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
		if ((length(bilanMigrationPar@taxons@data$tax_code) == 1) && (length(bilanMigrationPar@stades@data$std_code) == 1)) {
			
			
			# recherche des coef qui recoupent la periode du pas de temps
			req@sql = paste(" SELECT coe_date_debut, coe_date_fin, coe_valeur_coefficient, qte_libelle" ,
					" FROM   ",sch,"tj_coefficientconversion_coe " ,
					" INNER JOIN ref.tr_typequantitelot_qte ON coe_qte_code = qte_code" ,
					" WHERE  coe_tax_code = '" , bilanMigrationPar@taxons@data$tax_code , "'" ,
					" AND coe_std_code = '" , bilanMigrationPar@stades@data$std_code, "'" ,
					" AND (coe_date_debut, coe_date_fin) OVERLAPS (DATE '" , debutPas , "', DATE '" , finPas , "')" ,
					" ORDER BY coe_date_debut, qte_libelle " ,
					" ;",sep="" )
			
			#cat(paste("Requete SQL : \n" , sql,  "\n"))
			rs<-connect(req)@query
			
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
			# coef est un objet difftime avec methode sum non definie si je ne marque pas as.double
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
			}   else {
				# erreur
				coef = -1
			}
			#}
			
			
			
			#cat(paste("Coef de conversion pondere : " , round(unlist(lescoeff),4),  "\n" ))
			
			
			
		} else {
			# Les coefs ne sont pas calculables
			#cat(paste("Coef de conversion non calculables pour plus d'une espece + taxon",  "\n"))
		}
		
		
		# *********************
		#
		# Operations concernees par le pas de temps
		#
		# *********************
		
		# Preparation des criteres de selection
		# en pratique il n'est pas possible de choisir plusieurs taxons dans la combobox, et tant mieux par les coeff d'echappement
		# et de coefficient de conversion ne s'appliquent qu'a un couple taxon stade
		
		# ---------------------
		#
		#  Lots avec effectif
		#
		# ---------------------
		
		# recherche des operations qui recoupent la periode du pas de temps
		# Calcul de la somme des effectifs
		
		# traitement des sous lots
		# les sql de test / demo sont dans VUE_LOT_ECH_PARQUANCROISEPARQUAL
		if (!as.logical(bilanMigrationPar@echantillon@selected)) {
			echantillons=" AND lot_pere IS NULL"      
		} else {
			echantillons=""      
		} 
		if (bilanMigrationPar@parquan@data$par_nom=="aucune" & bilanMigrationPar@parqual@data$par_nom=="aucune") {
			stop("il faut choisir au moins une caracteristique quantitative ou qualitative")
		} else if (bilanMigrationPar@parquan@data$par_nom=="aucune") {
			#caracteristique qualitative uniquement
			req@sql=paste("SELECT ope_date_debut, ope_date_fin, lot_methode_obtention, SUM(lot_effectif) AS effectif,",
					" car_val_identifiant_tous as car_val_identifiant",
					" FROM (SELECT *,", 
					" CASE when car_val_identifiant is not null then car_val_identifiant",
					" ELSE lot_pere_val_identifiant",
					" END as car_val_identifiant_tous", 
					" FROM ",sch,"vue_ope_lot_ech_parqual", 
					" WHERE ope_dic_identifiant ='",bilanMigrationPar@dc@dc_selectionne,"'",
					echantillons,
					" AND lot_tax_code = '",bilanMigrationPar@taxons@data$tax_code,"'" ,
					" AND lot_std_code = '",bilanMigrationPar@stades@data$std_code,"'" ,
					" AND car_par_code = '",bilanMigrationPar@parqual@data$par_code,"'" ,
					" AND (ope_date_debut, ope_date_fin) OVERLAPS (TIMESTAMP '" , debutPas , "', TIMESTAMP '" , finPas , "')" ,
					" ) AS qan",
					" GROUP BY qan.ope_date_debut, qan.ope_date_fin, qan.lot_methode_obtention, qan.car_val_identifiant_tous " ,
					" ORDER BY qan.ope_date_debut",sep="")
		} else if (bilanMigrationPar@parqual@data$par_nom=="aucune") {
			# Caracteristique quantitative uniquement
			req@sql=paste("SELECT ope_date_debut, ope_date_fin, lot_methode_obtention, SUM(lot_effectif) AS effectif, SUM(car_valeur_quantitatif) AS quantite",
					" FROM ",sch,"vue_ope_lot_ech_parquan",    
					" WHERE ope_dic_identifiant ='",bilanMigrationPar@dc@dc_selectionne,"'",
					echantillons,
					" AND lot_tax_code = '",bilanMigrationPar@taxons@data$tax_code,"'" ,
					" AND lot_std_code = '",bilanMigrationPar@stades@data$std_code,"'" ,
					" AND car_par_code = '",bilanMigrationPar@parquan@data$par_code,"'" ,
					" AND (ope_date_debut, ope_date_fin) OVERLAPS (TIMESTAMP '" , debutPas , "', TIMESTAMP '" , finPas , "')" ,
					" GROUP BY ope_date_debut, ope_date_fin, lot_methode_obtention" ,
					" ORDER BY ope_date_debut",sep="")
		} else {
			#les deux caracteristiques sont choisies, il faut faire un Bilancroise
			# attention je choisis un left  join ça veut dire certaines caracteristiques quant n'ont pas de contrepartie quantitative     
			req@sql=paste(
					" SELECT ope_date_debut,",
					" ope_date_fin,",  
					" SUM(lot_effectif) AS effectif,", 
					" SUM(car_valeur_quantitatif) AS quantite,",
					" car_val_identifiant_tous as car_val_identifiant",
					" FROM (",
					" SELECT *,",
					" CASE when car_val_identifiant is not null then car_val_identifiant",
					" ELSE lot_pere_val_identifiant",
					" END as car_val_identifiant_tous",
					" FROM (",
					" SELECT * FROM ",sch,"vue_ope_lot_ech_parquan", 
					" WHERE ope_dic_identifiant ='",bilanMigrationPar@dc@dc_selectionne,"'",
					echantillons,
					" AND lot_tax_code = '",bilanMigrationPar@taxons@data$tax_code,"'" ,
					" AND lot_std_code = '",bilanMigrationPar@stades@data$std_code,"'" ,
					" AND car_par_code = '",bilanMigrationPar@parquan@data$par_code,"'" ,
					" AND (ope_date_debut, ope_date_fin) OVERLAPS (TIMESTAMP '",debutPas,"',TIMESTAMP '",finPas,"') " ,
					" ) AS qan",
					" LEFT JOIN", 
					" (SELECT lot_identifiant as lot_identifiant1,car_val_identifiant ",
					"  FROM vue_ope_lot_ech_parqual ", 
					" WHERE ope_dic_identifiant ='",bilanMigrationPar@dc@dc_selectionne,"'",
					echantillons,
					" AND lot_tax_code = '",bilanMigrationPar@taxons@data$tax_code,"'" ,     
					" AND lot_std_code = '",bilanMigrationPar@stades@data$std_code,"'" ,
					" AND car_par_code = '",bilanMigrationPar@parqual@data$par_code,"'" ,
					" AND (ope_date_debut, ope_date_fin) OVERLAPS (TIMESTAMP '",debutPas,"',TIMESTAMP '",finPas,"') " ,
					" )as qal ",
					" ON qan.lot_identifiant=qal.lot_identifiant1",
					" )as qanqal",
					" GROUP BY  qanqal.ope_date_debut, qanqal.ope_date_fin, qanqal.car_val_identifiant_tous",
					" ORDER BY qanqal.ope_date_debut",sep="")
			
		}# end else
		
		#cat(paste("Requete SQL : \n" , sql))
		rs<-connect(req)@query
		#cat(nrow(rs))
		if (nrow(rs)>0){
			
			debutOpe=as.POSIXlt(rs$ope_date_debut)
			finOpe= as.POSIXlt(rs$ope_date_fin)
			effectif=rs$effectif
			quantite=rs$quantite
			if (bilanMigrationPar@parqual@data$par_nom!="aucune") {
				rs$car_val_identifiant[is.na(rs$car_val_identifiant)]<-"autre"
			}
			# creation des sommes effectif_MESURE ...
			
			# Si l'operation commence avant le pas de temps courant, et ne se termine pas apres, il faut conserver une seule partie de l'operation
			# Si l'operation se termine apres la fin du pas mais ne debute pas avant, il faut conserver une seule partie de l'operation
			# Si l'operation commence avant le pas de temps et se termine apres, on ne conserve qu'une partie de l'operation
			# Cas ou l'operation est inferieure ou egale au pas de temps : pas de probleme, on compte l'operation complete
			# ce qui revient à dire que pour ce qui concerne la duree de l'operation effectif sur le pas de temps
			# on prends le max du debut de ope et pas de temps (si l'ope commence avant on garde pas cette partie )
			# et pour la fin on prend le min si l'ope se termine apres on garde pas... ouf
			
			debut<-debutOpe
			fin<-finOpe
			debut[debut<debutPas]<-debutPas
			fin[fin>finPas]<-finPas
			
			# Repartition de l'effectif au prorata
			effectif = effectif *  as.double(difftime(time1=fin, time2=debut,units =  "secs"))/as.double(difftime(time1=finOpe,time2=debutOpe,units =  "secs")) 
			quantite=  quantite *  as.double(difftime(time1=fin, time2=debut,units =  "secs"))/as.double(difftime(time1=finOpe,time2=debutOpe,units =  "secs")) 
			if (bilanMigrationPar@parqual@data$par_nom!="aucune") { # il existe des caracteristiques qualitatives de lot			
				# i=c(valeurs_qal,"tous")[2]
				for (i in valeurs_qal){
					assign(eval(paste("effectif_",i,sep="")),sum(effectif[rs$car_val_identifiant==i]))
					assign(eval(paste("quantite_",i,sep="")),sum(quantite[rs$car_val_identifiant==i]))
				}
			} else {# pas de caracteristiques qualitatives de lot et pas de decoupage supplementaire
				effectif<-sum(effectif)
				quantite<-sum(quantite)
			}
		} else {
			# dans le cas ou le resultat de la requete est vide pas de ligne je met 0
			if (bilanMigrationPar@parqual@data$par_nom!="aucune") { # il existe des caracteristiques qualitatives de lot			
				for (i in valeurs_qal){
					assign(eval(paste("effectif_",i,sep="")),0)
					assign(eval(paste("quantite_",i,sep="")),0)
				}
			} else {# pas de caracteristiques qualitatives de lot et pas de decoupage supplementaire
				effectif<-0
				quantite<-0
			}
			
		}
		
		
		# Enregistrement des donnees du pas courant dans le fichier de sortie
		
		if (!exists("tablecalcmig")){
			tablecalcmig=data.frame()
		} 
		j=nrow(tablecalcmig)+1
		tablecalcmig[j,"No_pas"]=bilanMigrationPar@pasDeTemps@noPasCourant   
		tablecalcmig[j,"Debut_pas"]=strftime(debutPas,format="%Y-%m-%d") # je passe en caractere sinon ne marche pas      
		tablecalcmig[j,"Fin_pas"]=strftime(finPas,format="%Y-%m-%d")           
		tablecalcmig[j,"taux_d_echappement"] = tauxEch
		if (bilanMigrationPar@parqual@data$par_nom!="aucune") { # il existe des caracteristiques qualitatives de lot			
			for (i in valeurs_qal){
				tablecalcmig[j,eval(paste("effectif_",i,sep=""))]<-get(eval(paste("effectif_",i,sep="")))
				tablecalcmig[j,eval(paste("quantite_",i,sep=""))]<-get(eval(paste("quantite_",i,sep="")))
			}
		} else {
			tablecalcmig[j,"effectif"]<-effectif
			tablecalcmig[j,"quantite"]<-quantite
		}				
		
		
		# Avance au pas de temps suivant
		bilanMigrationPar@pasDeTemps=suivant(bilanMigrationPar@pasDeTemps)
	}     # end boucle while
	
	
	
	if (bilanMigrationPar@parqual@data$par_nom!="aucune") {
		param_qual=data.frame("val_qal_code"=valeurs_qal,"lib"=libelle_qal)
	} else
	{ param_qual=data.frame()# affectation ulterieur à la classe
	}
	return (list(tablecalcmig,param_qual))
	
}               # end SousListeBilanMigration

