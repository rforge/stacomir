context("Bilan_MigrationInterAnnuelle")


test_that("Test an instance of Bilan_MigrationInterAnnuelle loaded with choice_c",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
			# overriding user schema to point to iav
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi) # "iav."
			assign("sch","iav.",envir_stacomi)
			bmi<-new("BilanMigrationInterAnnuelle")
			# the following will load data for size, 
			# parameters 1786 (total size) C001 (size at video control)
			# dc 5 and 6 are fishways located on the Arzal dam
			# two stages are selected
			bmi<-choice_c(bmi,
					dc=6,
					taxons=c("Anguilla anguilla"),
					stades=c("AGJ"),
					anneedebut=1996,
					anneefin=2015,
					silent=TRUE)
			bmi<-connect(bmi,silent=TRUE)	
			# three warning produced, none shown due to silent=TRUE
			options(warn = 0)
			expect_s4_class(bmi,"BilanMigrationInterAnnuelle")
			rm("envir_stacomi",envir =.GlobalEnv)
		})


test_that("Test method summary in BilanMigrationInterAnnuelle",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
			# overriding user schema to point to iav
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi) # "iav."
			assign("sch","iav.",envir_stacomi)
			bmi<-new("BilanMigrationInterAnnuelle")
			# the following will load data for size, 
			# parameters 1786 (total size) C001 (size at video control)
			# dc 5 and 6 are fishways located on the Arzal dam
			# two stages are selected
			bmi<-choice_c(bmi,
					dc=6,
					taxons=c("Anguilla anguilla"),
					stades=c("AGJ"),
					anneedebut="1996",
					anneefin=2015,
					silent=TRUE)
			bmi<-connect(bmi,silent=TRUE)	
			summary(object=bmi,silent=TRUE)
			# two warning produced
			rm("envir_stacomi",envir =.GlobalEnv)
			
		})

test_that("Test example bilanMigrationInterAnnuelle_example",
		{
			# check if built with examples (Rtools install --example)
			# the file is generate it examples but later loaded to examples from the class using @example
			# be sure you have built Roxygen documentation before running
			example_path<-file.path(.libPaths(),"stacomiR","R-ex","BilanMigrationInterAnnuelle-class.R")
			test<-file.access(example_path,0)
			if (test[1]!=0) warnings("Package example dir not created ?") else
				suppressWarnings(source(example_path))
			
		})

test_that("Test that loading two taxa will fail",
		{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
			# overriding user schema to point to iav
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi) # "iav."
			assign("sch","iav.",envir_stacomi)
			bmi<-new("BilanMigrationInterAnnuelle")
			# the following will load data for size, 
			# parameters 1786 (total size) C001 (size at video control)
			# dc 5 and 6 are fishways located on the Arzal dam
			# two stages are selected
			bmi<-suppressWarnings(choice_c(bmi,
							dc=5,
							taxons=c("Anguilla anguilla","Petromyzon marinus"),
							stades=c("AGJ"),
							anneedebut="1996",
							anneefin=2015,
							silent=TRUE))
			expect_error(charge(bmi))
			
		})


test_that("Test that bilanMigrationInterannuelle loads missing data with correct warning",
		{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
			
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("logrami",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi)
			assign("sch","logrami.",envir_stacomi)
			
			bmi_cha<-new("BilanMigrationInterAnnuelle") #châtelrault
			bmi_cha<-suppressWarnings(choice_c(bmi_cha,
							dc=c(21),
							taxons=c("Salmo salar"),
							stades=c("5"),
							anneedebut="2004",
							anneefin="2006",
							silent=TRUE))
			
			bmi_cha<-charge(bmi_cha,silent=TRUE)
# deleting all data to ensure everything is loaded
			supprime(bmi_cha)
			bmi_cha<-connect(bmi_cha,silent=TRUE)
			
		})

test_that("Test that different sums are the same, for  BilanMigrationInterAnnuelle, BilanMigrationMult",
		{
			stacomi(gr_interface=FALSE,
					login_window=FALSE,
					database_expected=TRUE)	
			# overriding user schema
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi) # "iav."
			assign("sch","iav.",envir_stacomi)
			# this chunk is not launched from examples but loads the bM_Arzal dataset if connection works	
			bmi<-new("BilanMigrationInterAnnuelle")
			# the following will load data for size, 
			# parameters 1786 (total size) C001 (size at video control)
			# dc 5 and 6 are fishways located on the Arzal dam
			# two stages are selected
			bmi<-choice_c(bmi,
					dc=6,
					taxons=c("Anguilla anguilla"),
					stades=c("AGJ"),
					anneedebut=1997,
					anneefin=1997,
					silent=TRUE)
			bmi<-connect(bmi,silent=TRUE)	
			bmM<-as(bmi,"BilanMigrationMult")
			# we still need to load the associated classes properly
			# so we need to launch the choice method.
			bmM<-choice_c(bmM, 
					dc=bmM@dc@dc_selectionne,
					taxons=bmM@taxons@data$tax_code, 
					stades=bmM@stades@data$std_code, 
					datedebut=as.character(bmM@pasDeTemps@dateDebut),
					datefin=as.character(as.POSIXlt(DateFin(bmM@pasDeTemps))),
					silent=TRUE)
			bmM<-charge(bmM,silent=TRUE)
			bmM<-connect(bmM,silent=TRUE)
			bmM<-calcule(bmM,silent=TRUE)
			
			expect_equal(				
					round(sum(bmM@calcdata[["dc_6"]][["data"]]$Effectif_total)),
					round(sum(bmi@data$bjo_valeur[bmi@data$bjo_labelquantite=="Effectif_total"]))
			)
			######################
			# Test for BilanAnnuel
			#####################
			bilA=as(bmi,"BilanAnnuels")
			bilA<-connect(bilA,silent=TRUE)
			# we test that the BilanAnnuel has the same number as
			# BilanMigration
			expect_equal(
					round(sum(bmM@calcdata[["dc_6"]][["data"]]$Effectif_total)),
					round(bilA@data$effectif[1]),
					label="The sum of number in the BilanMigration are different to the
							number in the BilanAnnuel class"
			)		
			rm("envir_stacomi",envir =.GlobalEnv)
		})
