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


setAs("BilanMigration","BilanMigrationMult",function(from,to){
			bMM=new("BilanMigrationMult")
			bMM@dc=from@dc
			bMM@taxons=from@taxons
			bMM@stades=from@stades
			bMM@datedebut=from@datedebut
			bMM@anneeFin=from@datefin
			bMM@anneeFin=from@datefin
			bMM@data=from@data
			return(bMM)
		})