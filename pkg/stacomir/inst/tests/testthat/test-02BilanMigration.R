context("BilanMigration")
test_that("Test an instance of BilanMigration",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			# overriding user schema to point to iav
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi) # "iav."
			assign("sch","iav.",envir_stacomi)
			
			bilanMigration<-new("BilanMigration")
			options(warn = -1)
			bilanMigration<-choice_c(bilanMigration,
					dc=c(6),
					taxons=c("Anguilla anguilla"),
					stades=c("AGJ"),
					datedebut="2013-01-01",
					datefin="2013-12-31")
			options(warn = 0)
			expect_s4_class(bilanMigration,
					"BilanMigration")
			rm("envir_stacomi",envir =.GlobalEnv)
		})

test_that("Test connect method",{
			stacomi(gr_interface=FALSE,
					login_window=FALSE,
					database_expected=FALSE)	
			# overriding user schema
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi) # "iav."
			assign("sch","iav.",envir_stacomi)
	# this chunk is not launched from examples but loads the bM_Arzal dataset if connection works	
			bM_Arzal=new("BilanMigration")
			bM_Arzal=choice_c(bM_Arzal,
					dc=5,
					taxons=c("Liza ramada"),
					stades=c("IND"),
					datedebut="2015-01-01",
					datefin="2015-12-31")
			bM_Arzal<-charge(bM_Arzal,silent=TRUE)
			bM_Arzal<-connect(bM_Arzal,silent=TRUE)
			
			expect_length(bM_Arzal@data,11)
			rm("envir_stacomi",envir =.GlobalEnv)
})

test_that("Test example 02_BilanMigration",
		{
			# check if built with examples (Rtools install --example)
			# the file is generate it examples but later loaded to examples from the class using @example
			# be sure you have built Roxygen documentation before running
			example_path<-file.path(.libPaths(),"stacomiR","R-ex","BilanMigration-class.R")
			test<-file.access(example_path,0)
			if (test[1]!=0) warnings("Package example dir not created ?") else
				source(example_path)
		})


test_that("Summary method works",
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
			data("bM_Arzal")
			bM_Arzal<-calcule(bM_Arzal,silent=TRUE)
			summary(bM_Arzal,silent=TRUE)
			rm("envir_stacomi",envir =.GlobalEnv)
		})

test_that("Test writing an example to the database",
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
			data("bM_Arzal")
			bM_Arzal<-calcule(bM_Arzal,silent=TRUE)
			write_database(object=bM_Arzal,silent=TRUE)
			rm("envir_stacomi",envir =.GlobalEnv)
		})

test_that("print method works",
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
			data("bM_Arzal")
			expect_output(print(bM_Arzal), "bilanMigration=choice_c",info = NULL)
			rm("envir_stacomi",envir =.GlobalEnv)
		})



test_that("test example for fd80",
		{
			stacomi(gr_interface=FALSE,
					login_window=FALSE,
					database_expected=TRUE)	
			# overriding user schema
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("fd80",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi) # "iav."
			assign("sch","fd80.",envir_stacomi)
			# this chunk is not launched from examples but loads the bM_Arzal dataset if connection works	
			bM_EclusierVaux=new("BilanMigration")
			bM_EclusierVaux=choice_c(bM_EclusierVaux,
					dc=6,
					taxons=c("Anguilla anguilla"),
					stades=c("AGG"),
					datedebut="2016-01-01",
					datefin="2016-12-31")
			bM_EclusierVaux<-charge(bM_EclusierVaux,silent=TRUE)
			bM_EclusierVaux<-connect(bM_EclusierVaux,silent=TRUE)
			bM_EclusierVaux<-calcule(bM_EclusierVaux,silent=TRUE)
			plot(bM_EclusierVaux,silent=TRUE)
			summary(bM_EclusierVaux,silent=TRUE)
			rm("envir_stacomi",envir =.GlobalEnv)
		})


test_that("test example with glass eel",
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
			bM_Arzal_civ=new("BilanMigration")
			bM_Arzal_civ=choice_c(bM_Arzal_civ,
					dc=6,
					taxons=c("Anguilla anguilla"),
					stades=c("CIV"),
					datedebut="2003-01-01",
					datefin="2003-12-31")
			bM_Arzal_civ<-charge(bM_Arzal_civ,silent=TRUE)
			bM_Arzal_civ<-connect(bM_Arzal_civ,silent=TRUE)
			bM_Arzal_civ<-calcule(bM_Arzal_civ,silent=TRUE)
			plot(bM_Arzal_civ,silent=TRUE)
			# some additional arguments passed to plot via ...
			plot(bM_Arzal_civ,silent=TRUE,bty="n")
			summary(bM_Arzal_civ,silent=TRUE)
			rm("envir_stacomi",envir =.GlobalEnv)
		})