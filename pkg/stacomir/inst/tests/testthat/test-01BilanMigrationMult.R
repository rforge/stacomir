# Author: cedric.briand
###############################################################################

context("BilanMigrationMult")
test_that("Teste une instance de bilanMigrationMult",{
			bilanMigrationMult<-new("BilanMigrationMult")
			bilanMigrationMult<-load(bilanMigrationMult,
					dc=c(6,7),
					taxons=c("Anguilla anguilla","Salmo salar"),
					stades=c("AGG","AGJ","CIV"),
					datedebut="2012-01-01",
					datefin="2012-12-31")
			expect_s4_class(bilanMigrationMult,
					"BilanMigrationMult")})
test_that("Teste une autre instance de bilanMigrationMult",{
			bilanMigrationMult<-new("BilanMigrationMult")
			bilanMigrationMult<-load(bilanMigrationMult,
					dc=c(6,7),
					taxons=c(2038,2220),
					stades=c("AGG","AGJ","CIV"),
					datedebut="2012-01-01",
					datefin="31/12/2012")
			expect_s4_class(bilanMigrationMult,
					"BilanMigrationMult")})
test_that("Teste une une instance avec erreur (dc n'existe pas)",
		{
			bilanMigrationMult<-new("BilanMigrationMult")
			expect_error(load(bilanMigrationMult,
							dc=c(6,70),
							taxons=c("Anguilla anguilla","Salmo salar"),
							stades=c("AGG","AGJ","CIV"),
							datedebut="2012-01-01",
							datefin="31/12/2012"))
		})



