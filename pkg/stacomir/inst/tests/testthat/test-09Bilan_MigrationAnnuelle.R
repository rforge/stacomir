context("Bilan_MigrationAnnuelle")


test_that("Test an instance of BilanAnnuels loaded with choice_c",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
			# overriding user schema to point to iav
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi)
			assign("sch","iav.",envir_stacomi)
			bilA<-new("BilanAnnuels")
			bilA<-choice_c(bilA,
					dc=c(5,6,12),
					taxons=c("Anguilla anguilla"),
					stades=c("AGJ","AGG"),
					anneedebut="1996",
					anneefin="2015",
					silent=FALSE)
			bilA<-connect(bilA,silent=TRUE)	
			expect_s4_class(bilA,"BilanAnnuels")
			rm("envir_stacomi",envir =.GlobalEnv)
		})


test_that("Test methods in BilanAnnuels",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
			# overriding user schema to point to iav
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi)
			assign("sch","iav.",envir_stacomi)
			bilA<-new("BilanAnnuels")
			bilA<-choice_c(bilA,
					dc=c(5,6,12),
					taxons=c("Anguilla anguilla"),
					stades=c("AGJ","AGG"),
					anneedebut="1996",
					anneefin="2015",
					silent=FALSE)			
			bilA<-connect(bilA,silent=TRUE)			
			plot(bilA)
			barplot(bilA)
			rm("envir_stacomi",envir =.GlobalEnv)			
		})

test_that("Test example bilanMigrationInterAnnuelle_example",
		{
			# check if built with examples (Rtools install --example)
			# the file is generate it examples but later loaded to examples from the class using @example
			# be sure you have built Roxygen documentation before running
			example_path<-file.path(.libPaths(),"stacomiR","R-ex","BilanAnnuels-class.R")
			test<-file.access(example_path,0)
			if (test[1]!=0) warnings("Package example dir not created ?") else
				suppressWarnings(source(example_path))
				
		})



