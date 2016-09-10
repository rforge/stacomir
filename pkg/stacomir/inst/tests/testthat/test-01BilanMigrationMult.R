context("BilanMigrationMult")
test_that("Test an instance of bilanMigrationMult",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			bilanMigrationMult<-new("BilanMigrationMult")
			options(warn = 2)
			bilanMigrationMult<-choice_c(bilanMigrationMult,
					dc=c(6,7),
					taxons=c("Anguilla anguilla","Salmo salar"),
					stades=c("AGG","AGJ","CIV"),
					datedebut="2012-01-01",
					datefin="2012-12-31")
			options(warn = 0)
			expect_s4_class(bilanMigrationMult,
					"BilanMigrationMult")})
## This test check that the code above works with numeric and a different formating for date
test_that("Test another instance of bilanMigrationMult",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			bilanMigrationMult<-new("BilanMigrationMult")
			options(warn = 2)
			bilanMigrationMult<-choice_c(bilanMigrationMult,
					dc=c(6,7),
					taxons=c(2038,2220),
					stades=c("AGG","AGJ","CIV"),
					datedebut="2012-01-01",
					datefin="31/12/2012")
			options(warn = 0)
			expect_s4_class(bilanMigrationMult,
					"BilanMigrationMult")})
test_that("Tests one instance with error (dc does not exist)",
		{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			bilanMigrationMult<-new("BilanMigrationMult")
			options(warn = 2)
			expect_error(choice_c(bilanMigrationMult,
							dc=c(6,7000),
							taxons=c("Anguilla anguilla","Salmo salar"),
							stades=c("AGG","AGJ","CIV"),
							datedebut="2012-01-01",
							datefin="31/12/2012"))
			options(warn = 0)
		})

test_that("Tests one instance with error (dc does not exist)",
		{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			bilanMigrationMult<-new("BilanMigrationMult")
			options(warn = 2)
			expect_error(choice_c(bilanMigrationMult,
							dc=c(6,7000),
							taxons=c("Anguilla anguilla","Salmo salar"),
							stades=c("AGG","AGJ","CIV"),
							datedebut="2012-01-01",
							datefin="31/12/2012"))
			options(warn = 0)
		})

test_that("Test example 01_BilanMigrationMult",
		{
			# check if built with examples (Rtools install --example
			example_path<-file.path(.libPaths(),"stacomiR","R-ex","BilanMigrationMult-class.R")
			test<-file.access(example_path,0)
			if (test!=0) warnings("Package example dir not created ?") else
				source(example_path)
		})
