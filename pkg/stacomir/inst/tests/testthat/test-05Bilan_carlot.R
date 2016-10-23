context("Bilan_carlot")

test_that("Test that view lot_ope_car exists",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			req<-new("RequeteODBC")
			req@baseODBC<-get("baseODBC", envir=envir_stacomi)		
			sch=get("sch",envir=envir_stacomi)
			req@sql=paste("select * from ",sch," vue_lot_ope_car limit 10")
			req<-stacomirtools::connect(req)
			result<-req@query	
			expect_true(nrow(result)>0)
		})

test_that("Test an instance of Bilan_carlot loaded with choice_c",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			# overriding user schema to point to iav
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi) # "iav."
			assign("sch","iav.",envir_stacomi)
			
			b_carlot<-new("Bilan_carlot")
			options(warn = -1)
			b_carlot<-choice_c(b_carlot,
					dc=c(6),
					taxons=c("Anguilla anguilla"),
					stades=c("AGJ"),
					par=c(1785,1786,1787,"C001"),
					horodatedebut="2013-01-01",
					horodatefin="2013-12-31",
					silent=TRUE)
			# three warning produced, none shown due to silent=TRUE
			options(warn = 0)
			expect_s4_class(b_carlot,
					"Bilan_carlot")
			rm("envir_stacomi",envir =.GlobalEnv)
		})


test_that("Test methods in Bilan_carlot",{
			require(stacomiR)
			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
			# overriding user schema to point to iav
			baseODBC<-get("baseODBC",envir=envir_stacomi)
			baseODBC[c(2,3)]<-rep("iav",2)
			assign("baseODBC",baseODBC,envir_stacomi)
			sch<-get("sch",envir=envir_stacomi) # "iav."
			assign("sch","iav.",envir_stacomi)
			
			b_carlot<-new("Bilan_carlot")
			options(warn = 2)
			b_carlot<-choice_c(b_carlot,
					dc=c(5,6),
					taxons=c("Anguilla anguilla"),
					stades=c("AGJ","CIV"),
					par=c(1785,1786,1787,"C001"),
					horodatedebut="2013-01-01",
					horodatefin="2013-12-31",
					silent=TRUE)
			# two warning produced
			options(warn = 0)
			b_carlot<-connect(b_carlot,silent=TRUE)
			expect_true(nrow(b_carlot@data)>0,label="No data for b_carlot")
			b_carlot<-calcule(b_carlot)
			rm("envir_stacomi",envir =.GlobalEnv)
			
		})

test_that("Test example bilancarlot_example",
		{
			# check if built with examples (Rtools install --example)
			# the file is generate it examples but later loaded to examples from the class using @example
			# be sure you have built Roxygen documentation before running
			example_path<-file.path(.libPaths(),"stacomiR","R-ex","Bilan_carlot-class.R")
			test<-file.access(example_path,0)
			if (test[1]!=0) warnings("Package example dir not created ?") else
				source(example_path)
		})



