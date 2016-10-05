context("BilanMigrationMult")
test_that("Test an instance of bilanMigrationMult",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			bilanMigrationMult<-new("BilanMigrationMult")
			options(warn = -1)
			bilanMigrationMult<-choice_c(bilanMigrationMult,
					dc=c(6,7),
					taxons=c("Anguilla anguilla","Salmo salar"),
					stades=c("AGG","AGJ","CIV"),
					datedebut="2012-01-01",
					datefin="2012-12-31")
			options(warn = 0)
			expect_s4_class(bilanMigrationMult,
					"BilanMigrationMult")
		rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
		})

## This test check that the code above works with numeric and a different formating for date
test_that("Test another instance of bilanMigrationMult",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			bilanMigrationMult<-new("BilanMigrationMult")
			options(warn = -1)
			bilanMigrationMult<-choice_c(bilanMigrationMult,
					dc=c(6,7),
					taxons=c(2038,2220),
					stades=c("AGG","AGJ","CIV"),
					datedebut="2012-01-01",
					datefin="31/12/2012")
			options(warn = 0)
			expect_s4_class(bilanMigrationMult,
					"BilanMigrationMult")
	rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
		})
test_that("Tests one instance with error (dc does not exist)",
		{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			bilanMigrationMult<-new("BilanMigrationMult")
			options(warn = -1)
			expect_error(choice_c(bilanMigrationMult,
							dc=c(6,7000),
							taxons=c("Anguilla anguilla","Salmo salar"),
							stades=c("AGG","AGJ","CIV"),
							datedebut="2012-01-01",
							datefin="31/12/2012"))
			options(warn = 0)
	rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
		})

test_that("Test charge method for bilanMigrationMult",
		{
		
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			bilanMigrationMult<-new("BilanMigrationMult")
			options(warn = -1)
			bilanMigrationMult<-choice_c(bilanMigrationMult,
					dc=c(6,7),
					taxons=c(2038),
					stades=c("AGG","AGJ","CIV"),
					datedebut="2012-01-01",
					datefin="31/12/2012")
			options(warn = 0)
			bilanMigrationMult<-charge(bilanMigrationMult,silent=TRUE)
			
			})

	test_that("Test connect method for bilanMigrationMult",
			{
				require(stacomiR)
				stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
				bilanMigrationMult<-new("BilanMigrationMult")
				options(warn = -1)
				bilanMigrationMult<-choice_c(bilanMigrationMult,
						dc=c(6,7),
						taxons=c(2038),
						stades=c("AGG","AGJ","CIV"),
						datedebut="2012-01-01",
						datefin="31/12/2012")
				options(warn = 0)
				bilanMigrationMult<-charge(bilanMigrationMult,silent=TRUE)
				bilanMigrationMult<-connect(bilanMigrationMult,silent=TRUE)
				expect_gt(nrow(bilanMigrationMult@data),0)
				bilanOperation<-get("bilanOperation",envir=envir_stacomi)
				expect_gt(nrow(bilanOperation@data),0)
				bilanFonctionnementDF<-get("bilanFonctionnementDF",envir=envir_stacomi)
				expect_gt(nrow(bilanFonctionnementDF@data),0)
				bilanFonctionnementDC<-get("bilanFonctionnementDC",envir=envir_stacomi)
				expect_gt(nrow(bilanFonctionnementDC@data),0)
		rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
			})
	
test_that("Test example 01_BilanMigrationMult",
		{
			# check if built with examples (Rtools install --example
			example_path<-file.path(.libPaths(),"stacomiR","R-ex","BilanMigrationMult-class.R")
			test<-file.access(example_path,0)
			if (test[1]!=0) warnings("Package example dir not created ?") else
				options(warn = -1)
				source(example_path)
				options(warn = 0)
			summary(bMM_Arzal,silent=TRUE)
		})


