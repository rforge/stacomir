setAs("BilanMigration","BilanMigrationInterAnnuelle",function(from){
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


setAs("BilanMigrationInterAnnuelle","BilanMigrationMult",function(from){
			bilanMigrationMult=new("BilanMigrationMult")
			bilanMigrationMult@dc=from@dc
			bilanMigrationMult@taxons=from@taxons
			bilanMigrationMult@stades=from@stades
			bilanMigrationMult@pasDeTemps@dateDebut=strptime(stringr::str_c(from@anneeDebut@annee_selectionnee,"-01-01"),format="%Y-%m-%d")
			bilanMigrationMult@pasDeTemps@nbStep=364
			return(bilanMigrationMult)
		})


setAs("BilanMigration","BilanMigrationMult",function(from){
			bMM=new("BilanMigrationMult")
			bMM@dc=from@dc
			bMM@taxons=from@taxons
			bMM@stades=from@stades
			bMM@pasDeTemps=from@pasDeTemps
			bMM@coef_conversion=from@coef_conversion
			bMM@data=from@data
			bMM@time.sequence=from@time.sequence
			bMM@calcdata=from@calcdata
			return(bMM)
		})

setAs("BilanMigrationInterAnnuelle","BilanAnnuels",function(from){
			bilA=new("BilanAnnuels")
			bilA@dc=from@dc
			bilA@taxons=from@taxons
			bilA@stades=from@stades
			bilA@anneedebut=from@anneeDebut
			bilA@anneefin=from@anneeFin
			return(bilA)
		})